# CLV på virksomhedsniveau

library(RODBC)
library(tidyverse)


credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

query <- "SELECT

      [companyname]
      ,[CVR]
	  ,CLTVtodateRevenue
	  ,CLTVtodateProfit
	  ,CLTVtogo360Revenue
	  ,CLTVtogo360Profit
	  ,CLTVtogo720Revenue
	  ,CLTVtogo720Profit

  FROM [EDW].[edw].[Customer] customer
  left join [DataMartMisc].[dbo].[Customer_CLTV] clv on clv.Customer_Key = customer.Customer_Key

  where UserSegmentGroup = 'BTB' or UserSegmentGroup = 'BTI'"

df <- sqlQuery(channel,query)

df <- df %>%
  mutate(CVR = as.character(CVR),
         companyname = tolower(as.character(companyname)),
         companyname = str_replace_all(companyname,"[^[:alpha:]]",""),
         company_cvr = paste0(companyname,"_",CVR)) %>%
  select(companyname,CVR,company_cvr,everything()) %>%
  arrange(companyname) %>%
  filter(company_cvr != "_", !str_detect(company_cvr,"_NA|NA_"))


df <- df %>%
  group_by(company_cvr) %>%
  summarise_at(vars(starts_with("CLTV")),sum,na.rm=T) %>%
  ungroup()

df$company <- str_split(df$company_cvr,"_") %>% map_chr(1)
df$cvr <- str_split(df$company_cvr,"_") %>% map_chr(2)

company <- df %>%
  pull(company)


new_company <- company

for (i in 2:length(company)-1) {
  
  
  if (stringdist(company[i],company[i+1],method ="jaccard") <= .1) {
    
    new_company[i] = company[i+1]

  } else{
  
  new_company[i] = company[i]
}
  i = i + 1
}

dd<-cbind(company,new_company)

dd %>% as_tibble() %>%map(n_distinct)

df$company <- new_company



df <- df %>% 
  select(company,cvr,everything()) %>%
  select(-company_cvr)

write_csv2(df, "virksomheder_clv.csv")


# ----------------------------------------------------------------------------

df <- read_csv2("virksomheder_clv.csv")

library(stringdist)
library(tidystringdist)

companies <- df %>%
  pull(company) %>%
       tidy_comb_all(base=company)


kk %>% select(company) %>%
  mutate(company2 = if_else(stringdist(company,lag(company),method="jaccard") <= .9,lag(company),company))

d <- tidy_stringdist(companies,method="jaccard")
