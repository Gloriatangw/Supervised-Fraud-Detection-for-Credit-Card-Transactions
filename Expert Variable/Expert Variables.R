install.packages("RSQLite")
library(RSQLite)
install.packages("sqldf")
library(sqldf)
install.packages("RODBC")
library(RODBC)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyr)


#Create three names for same dataset

a <- read.csv("cleanedData.csv")

a <- a %>% filter(Transtype == 'P')

b <- a
df<- a



#Create expert variables with SQL 

Cardnum = sqldf("SELECT a.Recordnum,
COUNT(CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Cardnum_1,
COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Cardnum_3,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Cardnum_5,
COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Cardnum_7,
COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Cardnum_14,
COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Cardnum_30,
COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Cardnum_diff_Merch_Description_1, 
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Cardnum_diff_Merch_Description_3,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Cardnum_diff_Merch_Description_5,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Cardnum_diff_Merch_Description_7,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Cardnum_diff_Merch_Description_14,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Cardnum_diff_Merch_Description_30,
COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_Cardnum_diff_amount_1, 
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_Cardnum_diff_amount_3,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_Cardnum_diff_amount_5,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_Cardnum_diff_amount_7,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_Cardnum_diff_amount_14,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_Cardnum_diff_amount_30,
COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_Cardnum_diff_Merchant_State_1,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_Cardnum_diff_Merchant_State_3,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_Cardnum_diff_Merchant_State_5,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_Cardnum_diff_Merchant_State_7,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_Cardnum_diff_Merchant_State_14,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_Cardnum_diff_Merchant_State_30,
COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_Cardnum_diff_Merchant_Zip_1,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_Cardnum_diff_Merchant_Zip_3,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_Cardnum_diff_Merchant_Zip_5,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_Cardnum_diff_Merchant_Zip_7,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_Cardnum_diff_Merchant_Zip_14,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_Cardnum_diff_Merchant_Zip_30
from a, b
WHERE a.Cardnum = b.Cardnum
GROUP BY 1")



amount = sqldf("SELECT a.Recordnum,
COUNT(CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_amount_1,
COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_amount_3,
COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_amount_5,
              COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_amount_7,
              COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_amount_14,
              COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_amount_30,
              COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_amount_diff_Merch_Description_1, 
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_amount_diff_Merch_Description_3,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_amount_diff_Merch_Description_5,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_amount_diff_Merch_Description_7,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_amount_diff_Merch_Description_14,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_amount_diff_Merch_Description_30,
              COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_amount_diff_Cardnum_1, 
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_amount_diff_Cardnum_3,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_amount_diff_Cardnum_5,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_amount_diff_Cardnum_7,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_amount_diff_Cardnum_14,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_amount_diff_Cardnum_30,
              COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_amount_diff_Merchant_State_1, 
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_amount_diff_Merchant_State_3,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_amount_diff_Merchant_State_5,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_amount_diff_Merchant_State_7,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_amount_diff_Merchant_State_14,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_amount_diff_Merchant_State_30,
              COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_amount_diff_Merchant_Zip_1, 
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_amount_diff_Merchant_Zip_3,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_amount_diff_Merchant_Zip_7,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_amount_diff_Merchant_Zip_5,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_amount_diff_Merchant_Zip_14,
              COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Merchant_Zip ELSE NULL END)  AS same_amount_diff_Merchant_Zip_30
              
from a, b
WHERE a.Amount = b.Amount
GROUP BY 1")

Merchant_Zip = sqldf("SELECT a.Recordnum,
COUNT(CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Merchant_Zip_1,
COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Merchant_Zip_3,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Merchant_Zip_5,
COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Merchant_Zip_7,
COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Merchant_Zip_14,
COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_Merchant_Zip_30,
COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Merchant_Zip_diff_Merch_Description_1, 
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Merchant_Zip_diff_Merch_Description_3,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Merchant_Zip_diff_Merch_Description_5,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Merchant_Zip_diff_Merch_Description_7,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Merchant_Zip_diff_Merch_Description_14,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_Merchant_Zip_diff_Merch_Description_30,
COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_Merchant_Zip_diff_Cardnum_1, 
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_Merchant_Zip_diff_Cardnum_3,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_Merchant_Zip_diff_Cardnum_5,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_Merchant_Zip_diff_Cardnum_7,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_Merchant_Zip_diff_Cardnum_14,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_Merchant_Zip_diff_Cardnum_30,
COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_amount_diff_Merchant_State_1, 
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_Merchant_Zip_diff_Merchant_State_3,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_Merchant_Zip_diff_Merchant_State_7,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_amount_diff_Merchant_State_5,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_Merchant_Zip_diff_Merchant_State_14,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Merchant_State ELSE NULL END)  AS same_Merchant_Zip_diff_Merchant_State_30,
COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_amount_diff_amount_1, 
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_Merchant_Zip_diff_amount_3,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_Merchant_Zip_diff_amount_7,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_amount_diff_amount_5,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_Merchant_Zip_diff_amount_14,
COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_Merchant_Zip_diff_amount_30

from a, b
WHERE a.Merchant_Zip = b.Merchant_Zip
GROUP BY 1")

state_amount_week_diff = sqldf("SELECT a.Recordnum,
COUNT(CASE WHEN a.Date = b.Date THEN a.Recordnum ELSE NULL END)  AS same_state_amount_week_diff_1,
                COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_state_amount_week_diff_3,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_state_amount_week_diff_5,
                COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_state_amount_week_diff_7,
                COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_state_amount_week_diff_14,
                COUNT(CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN a.Recordnum ELSE NULL END)  AS same_state_amount_week_diff_30,
                COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_state_amount_week_diff_diff_Merch_Description_1, 
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_state_amount_week_diff_diff_Merch_Description_3,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_state_amount_week_diff_diff_Merch_Description_5,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_state_amount_week_diff_diff_Merch_Description_7,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_state_amount_week_diff_diff_Merch_Description_14,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Merch_Description ELSE NULL END)  AS same_state_amount_week_diff_diff_Merch_Description_30,
                COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_state_amount_week_diff_diff_Cardnum_1, 
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_state_amount_week_diff_diff_Cardnum_3,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_state_amount_week_diff_diff_Cardnum_5,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_state_amount_week_diff_diff_Cardnum_7,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_state_amount_week_diff_diff_Cardnum_14,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Cardnum ELSE NULL END)  AS same_state_amount_week_diff_diff_Cardnum_30,
                COUNT(DISTINCT CASE WHEN a.Date = b.Date AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_state_amount_week_diff_diff_amount_1, 
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 2 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_state_amount_week_diff_diff_amount_3,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 6 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_state_amount_week_diff_diff_amount_7,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 4 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_amount_diff_amount_5,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 13 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_state_amount_week_diff_diff_amount_14,
                COUNT(DISTINCT CASE WHEN a.Date - b.Date BETWEEN 0 AND 29 AND a.Recordnum> b.Recordnum THEN b.Amount ELSE NULL END)  AS same_state_amount_week_diff_diff_amount_30
                
                from a, b
                WHERE a.Merchant_State = b.Merchant_State
                AND a.Amount_week_diff=b.Amount_week_diff
                GROUP BY 1")


#join datasets

df <- inner_join(df, Merchant_Zip, by ="Recordnum")
df <- inner_join(df, Cardnum, by ="Recordnum")
df <- inner_join(df, amount, by ="Recordnum")
df <- inner_join(df, state_amount_week_diff, by ="Recordnum")

saveRDS(df, file ="expert variables")

write.csv(df, file="expert varibles.csv")


