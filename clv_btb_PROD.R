# ---------------------------------------------------------------------------------
# CLV for virksomheder
# Peer Christensen
# marts 2020

# ---------------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------------

# Company_Key
# rep_transactions - antal gentagne transaktioner
# sum_profit       - Profit til dato
# est_orders540    - estimeret antal ordrer de næste 540 dage
# Active           - sandsynlighed for at kunden er aktiv
# clv540           - ny beregning
# clv540_2         - summeret beregning på kundeniveau
# clv_avg          - gennemsnit af de to cvl-beregninger

# ---------------------------------------------------------------------------------
# Pakker
# ---------------------------------------------------------------------------------

library(tidyverse)
library(RODBC)
library(BTYDplus)

# ---------------------------------------------------------------------------------
# SQL login
# ---------------------------------------------------------------------------------

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

# ---------------------------------------------------------------------------------
# Queries
# ---------------------------------------------------------------------------------

# til ny beregning
query1 <- "
 SELECT
       [DateOrdered_Key]
 	     ,ord.[Company_Key]
       ,[5_DB2]

   FROM [EDW].[edw].[OrderFactCombined] ord
   inner join [EDW].[edw].[Company] comp on comp.Company_Key = ord.Company_Key

  where ord.Company_Key > 0"

# summeret beregning på kundeniveau

query2 <-
  
  "WITH CTE AS
(
  SELECT
  c.Customer_Key,
  f.Company_Key,
  c.CLTVtodateProfit,
  c.CLTVtogo540Profit,
  cp.CompanyName,
  ROW_NUMBER() OVER (PARTITION BY c.Customer_Key ORDER BY f.DateOrdered_Key DESC) AS RowNo

  FROM
  EDW.dim.Customer c
  INNER JOIN EDW.fact.OrderFactCombined f ON f.Customer_Key = c.Customer_Key
  INNER JOIN EDW.dim.Company cp ON cp.Company_Key = f.Company_Key
  
  WHERE
  c.Customer_Key > 0
  AND f.Company_Key > 0
  AND c.CLTVtodateRevenue is not NULL
  
)

, CTE2 AS
(
  SELECT
  CTE.Company_Key,
  SUM(CTE.CLTVtodateProfit) AS CLTVtodateProfit,
  SUM(CTE.CLTVtogo540Profit) AS CLTVtogo540Profit,
  COUNT(DISTINCT CTE.Customer_Key) AS CustomerCount
  
  FROM
  CTE
  
  WHERE
  CTE.RowNo = 1
  
  GROUP BY
  CTE.Company_Key
)

SELECT
CTE2.Company_Key,
CTE2.CLTVtodateProfit,
CTE2.CLTVtogo540Profit


  FROM
   CTE2"

# ---------------------------------------------------------------------------------
# Ny CLV-beregning
# ---------------------------------------------------------------------------------

df <- sqlQuery(channel,query1) %>%
  as_tibble() %>%
  mutate(date = lubridate::ymd(DateOrdered_Key)) %>%
  select(cust = Company_Key, date, sales = `5_DB2`)

# Convert Transaction Log to CBS format
# x = n repeat transactions
# t.x. = time of last recorded transaction (recency) (higher = more recent)
# litt = the sum over logarithmic intertransaction times (required for estimating regularity)
# first = the date of the first transaction
# T.cal = the duration between the first transaction and the end of the calibration period

cbs <- elog2cbs(df,units = "days")

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
  select(Company_Key      = cust, 
         rep_transactions = x, 
         sum_profit       = sales, 
         est_orders540    = x.star.bgnbd540, 
         Active           = Alive,
         clv540)

# ---------------------------------------------------------------------------------
# Summeret CLV-beregning
# ---------------------------------------------------------------------------------

df2 <- sqlQuery(channel,query2)

# ---------------------------------------------------------------------------------
# Join data
# ---------------------------------------------------------------------------------

combined_clv <- company_clv %>%
  inner_join(df2) %>%
  mutate(clv_avg = (clv540 + CLTVtogo540Profit) / 2)

# cor(combined_clv$clv540,combined_clv$CLTVtogo540Profit)

# ---------------------------------------------------------------------------------
# Write to table
# ---------------------------------------------------------------------------------

sqlDrop(channel, "CompanyCLV", errors = FALSE)
sqlSave(channel, combined_clv, tablename = "CompanyCLV",rownames = F, safer=F)

