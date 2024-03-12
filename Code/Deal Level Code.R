
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plm)
library(lubridate)
library(DescTools)
library(broom)
library(zoo)
library(foreign)
library(readr)
library(data.table)

setwd("K:/National Culture and Project Finance/Profs Data/Workspace/R codes/Dealscan")

deal_level <- fread("filtered_dealscan.csv")
colnames(deal_level)
deal_level <- deal_level %>% 
              select(Borrower_Name, Country, SIC_Code, LPC_Deal_ID, Deal_Amount_Converted, 
                     Tranche_Amount_Converted, League_Table_Tranche_Date, Deal_Purpose, Project_Finance, LPC_Tranche_ID, 
                     Tranche_Type, Tranche_Maturity_Date, Country_Of_Syndication, 
                     Seniority_Type, Secured, Primary_Purpose,All_In_Spread_Drawn_bps)

#Create two new columns
deal_level <- deal_level %>%
             mutate(
             Log_Deal_Amount_Converted = log(Deal_Amount_Converted),
             Log_Tranche_Amount_Converted = log(Tranche_Amount_Converted)
             )
#Project Finance Variable - if the Primary_Purpose variable is Project Finance or Ship Finance or Aircraft & Ship Finance or Infrastructure, it is equal to 1, if the Primary _Purpose includes the words "Capital Expenditure", it is equal to 0, otherwise NA
deal_level <- deal_level %>% 
              mutate(pro_fin = if_else(grepl("Capital expenditure", Primary_Purpose), 0,
              if_else(Primary_Purpose == "Project Finance" | Primary_Purpose == "Ship finance" | 
              Primary_Purpose == "Aircraft & Ship finance" | Primary_Purpose == "Telecom Buildout" | 
              Primary_Purpose == "Infrastructure" , 1, NA_real_)))


#Capital Expenditure Variable - if Primary_purpose variable is Capital expenditure, it is equal to 1, else 0
deal_level <- deal_level %>% 
              mutate(cap_ex = if_else(Primary_Purpose == "Capital expenditure", 1, 0))


#Corporate Purpose term loan variable - if Deal_Purpose includes General Purpose, Tranche_Type includes Term Loan and Deal Amount is above 0.5 million - 1, else, 0
deal_level <- deal_level %>% 
              mutate(corp_pur = if_else(grepl("General Purpose", Primary_Purpose) & grepl("Term Loan", Tranche_Type) & Deal_Amount_Converted > 0.5, 1, 0))


#Deal Amount variable - Converted the Deal_Amount_Converted variable to billions
#deal_level <- deal_level %>% 
              #mutate(deal_amt = Deal_Amount_Converted/1000)


#All in spread variable - kept as it is
deal_level <- deal_level %>% 
              mutate(all_in_spr = All_In_Spread_Drawn_bps)


#Use the as.Date function to convert YYYYMMDD format to Date
starting_date <- as.Date(as.character(deal_level$Deal_Active_Date), format = "%Y%m%d")
ending_date <- as.Date(as.character(deal_level$Tranche_Maturity_Date), format = "%Y%m%d")

# Calculate the difference between the two dates in years
#date_diff_years <- as.numeric(difftime(ending_date, starting_date, units = "days") / 365.25)

#Store it in the mat variable
#deal_level <- deal_level %>% 
              #mutate( mat = date_diff_years)

#Secured variable - secured variable used as it is
deal_level <- deal_level %>% 
              mutate(sec = if_else(Secured == "Yes", 1, 0))

#Senior variable - as it is
deal_level <- deal_level %>% 
               mutate(sen = if_else(Seniority_Type == "Senior", 1, 0))



#arranging by country, borrower name, deal active date
deal_level <- deal_level %>% arrange (Country, Borrower_Name, League_Table_Tranche_Date)

#Save the file
fwrite(deal_level, "deal_level.csv")


