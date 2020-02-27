
library(tidyverse)
library(RODBC)

credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

query <-
  
  "WITH CTE AS
(
  SELECT
  c.Customer_Key,
  cp.cvr,
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
  CTE.CompanyName,
  CTE.cvr,
  SUM(CTE.CLTVtodateProfit) AS CLTVtodateProfit,
  SUM(CTE.CLTVtogo540Profit) AS CLTVtogo540Profit,
  COUNT(DISTINCT CTE.Customer_Key) AS CustomerCount
  
  FROM
  CTE
  
  WHERE
  CTE.RowNo = 1
  
  GROUP BY
  CTE.Company_Key,
  CTE.CompanyName,
  CTE.cvr
)

SELECT
CTE2.Company_Key,
CTE2.CompanyName,
CTE2.cvr,
CTE2.CLTVtodateProfit,
CTE2.CLTVtogo540Profit


  FROM
   CTE2"

df <- sqlQuery(channel,query)

df <- df %>%
  arrange(CompanyName)

write_csv2(df, "clv_virksomheder_2.csv")
