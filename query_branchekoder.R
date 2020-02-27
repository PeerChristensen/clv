

query <-

  "WITH CTE AS
(
  SELECT
  c.Customer_Key,
  cp.cvr,
  f.Company_Key,
  c.CLTVtodateRevenue,
  c.CLTVtodateProfit,
  c.CLTVtogo360Revenue,
  c.CLTVtogo360Profit,
  cp.CompanyName,
  ROW_NUMBER() OVER (PARTITION BY c.Customer_Key ORDER BY f.DateOrdered_Key DESC) AS RowNo

  FROM
  EDW.dim.Customer c
  INNER JOIN EDW.fact.OrderFactCombined f ON f.Customer_Key = c.Customer_Key
  INNER JOIN EDW.dim.Company cp ON cp.Company_Key = f.Company_Key
  
  WHERE
  c.Customer_Key > 0
  AND f.Company_Key > 0
  
)

, CTE2 AS
(
  SELECT
  CTE.Company_Key,
  CTE.CompanyName,
  CTE.cvr,
  SUM(CTE.CLTVtodateRevenue) AS CLTVtodateRevenue,
  SUM(CTE.CLTVtodateProfit) AS CLTVtodateProfit,
  SUM(CTE.CLTVtogo360Revenue) AS CLTVtogo360Revenue,
  SUM(CTE.CLTVtogo360Profit) AS CLTVtogo360Profit,
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
CTE2.CLTVtodateRevenue,
CTE2.CLTVtodateProfit,
CTE2.CLTVtogo360Revenue,
CTE2.CLTVtogo360Profit,
c2c.WeightingFactor,
cc.*

  FROM
   CTE2
  INNER JOIN EDW.fact.Company2CompanyTradeCodeFact c2c ON c2c.Company_Key = CTE2.Company_Key
INNER JOIN EDW.dim.CompanyTradeCode cc ON cc.CompanyTradeCode_Key = c2c.CompanyTradeCode_Key"

df <- sqlQuery(channel,query)

df <- df %>%
  arrange(CompanyName)


  
  
