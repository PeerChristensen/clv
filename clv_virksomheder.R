

library(tidyverse)
library(RODBC)
library(BTYDplus)

#####

# credentials <- read_rds("credentials.rds")
# 
# channel <- odbcConnect(credentials[1],credentials[2],credentials[3])
# 
# 
# query <- "
#  SELECT
#        [DateOrdered_Key]
#  	   ,ord.[Company_Key]
#  	   ,CompanyName
#  	   ,cvr
#        ,[Customer_Key]
#        ,[OrderID]
#        ,[3_NetRevenue]
#        ,[5_DB2]
# 
#    FROM [EDW].[edw].[OrderFactCombined] ord
#    inner join [EDW].[edw].[Company] comp on comp.Company_Key = ord.Company_Key
# 
#    where ord.Company_Key > 0"
#  
#  df <- sqlQuery(channel,query) %>%
#    as_tibble()
#  
#  df <- write_csv(df,"transactions_company.csv")

#############################################################################################

df <- read_csv("transactions_company.csv") %>%
  mutate(date = lubridate::ymd(DateOrdered_Key)) 

# Convert Transaction Log to CBS format
# x = n repeat transactions
# t.x. = time of last recorded transaction (recency) (higher = more recent)
# litt = the sum over logarithmic intertransaction times (required for estimating regularity)
# first = the date of the first transaction
# T.cal = the duration between the first transaction and the end of the calibration period

df2 <- df %>%
  select(cust = cvr, date, sales = `5_DB2`)

cbs <- elog2cbs(df2,units = "days")

params.bgcnbd  <-bgcnbd.EstimateParameters(cbs)

# estimate n repeat purchases next 540 days
cbs$x.star.bgnbd540 <- bgcnbd.ConditionalExpectedTransactions(params.bgcnbd, T.star  = 540,
                                                              x = cbs$x, t.x = cbs$t.x, T.cal = cbs$T.cal) 

# Estimate the likelihood the customer is alive 
cbs$Alive <- bgcnbd.PAlive(params.bgcnbd, cbs$x, cbs$t.x, cbs$T.cal)

cbs$Avg.sales<- ifelse(cbs$x<=0,0,cbs$sales/cbs$x)
cbs$Avg.sales <- ifelse(cbs$Avg.sales<0,0,cbs$Avg.sales)

gamma_params <- BTYD::spend.EstimateParameters(cbs$Avg.sales, cbs$x)

# expected monetary value ?
cbs$ExpectedMonetaryValue <- BTYD::spend.expected.value(gamma_params,cbs$Avg.sales , cbs$x)

# We find the CLTV to go

cbs$clv540 <- cbs$ExpectedMonetaryValue*cbs$x.star.bgnbd540
cbs$Alive <- ifelse(cbs$x==0,NA,cbs$Alive)

company_clv <- cbs %>%
  as_tibble() %>%
  select(cvr      = cust, 
         rep_transactions = x, 
         sum_profit       = sales, 
         est_orders540    = x.star.bgnbd540, 
         Active           = Alive,
         clv540)


company_clv %>% arrange(desc(Active))
hist(company_clv$Active)

#################################################################################################################
# Compare with summed CLV

clv_sum <- read_csv2("clv_virksomheder_2.csv") %>%
  select(CompanyName,cvr,clv540_2 = CLTVtogo540Profit)

combined_clv <- company_clv %>%
  inner_join(clv_sum) %>%
  select(cvr,CompanyName,everything()) %>%
  mutate(clv_avg = (clv540 + clv540_2) / 2)

cor(combined_clv$clv540,combined_clv$CLTVtogo540Profit)

combined_clv %>%
  ggplot(aes(clv540,CLTVtogo540Profit)) +
  geom_point(alpha=.05) +
  xlim(c(0,5000))+
  ylim(c(0,5000)) +
  geom_smooth(method = "lm", se = T)

combined_clv %>% arrange(desc(clv540))

write_csv(combined_clv,"combined_clv.csv")

##############################################################################################################
# EXTRA
k.est <- estimateRegularity(df)

plotTimingPatterns(df)

params.mbgcnbd <- mbgcnbd.EstimateParameters(cbs)

cbs$xstar.mbgcnbd <- mbgcnbd.ConditionalExpectedTransactions(
     params = params.mbgcnbd,
     T.star = cbs$T.star,
     x      = cbs$x,
     t.x    = cbs$t.x,
     T.cal  = cbs$T.cal)

cbs$palive.mbgcnbd <- mbgcnbd.PAlive(
     params = params.mbgcnbd,
     x      = cbs$x,
     t.x    = cbs$t.x,
     T.cal  = cbs$T.cal)




mean(cbs$palive.mbgcnbd)

params.bgnbd <- BTYD::bgnbd.EstimateParameters(cbs)

BTYD::bgnbd.cbs.LL(params.bgnbd, cbs)

BTYD::pnbd.PlotFrequencyInCalibration(params.pnbd, cbs, censor = 7, title = "Pareto/NBD")

inc <- elog2inc(groceryElog)

T.tot <- max(cbs$T.cal+cbs$T.star)

nil <- mbgcnbd.PlotTrackingInc(params.mbgcnbd, cbs$T.cal, T.tot = T.tot, inc, title = "MBG/CNBD-k")
                                  
# plot timing patterns of 30 sampled customers
plotTimingPatterns(df, n = 30, T.cal = today()-540,headers =c("Past", "Future"), title = "")