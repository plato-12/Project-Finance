
setwd("K:/National Culture and Project Finance/Profs Data/Workspace/R codes/Dealscan")
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
library(data.table)


dsdata <- fread('Dealscan data 30 June 2022-003.csv')
dsdata_fl <- dsdata %>% 
          select(-Lender_Parent_Name, -Lender_Parent_Id, -Lender_Name, -Lender_Id,
         -Primary_Role, -Lender_Commit, -Lender_Share, -Lender_Operating_Country, -Lender_Parent_Operating_Country,
         -Senior_Debt_to_EBITDA, -Total_Debt_to_EBITDA, -Lead_Arranger, -Bookrunner, -Number_of_Bookrunners, 
         -Top_Tier_Arranger, -Number_of_Top_Tier_Arrangers, -Lead_Manager, -Number_of_Lead_Managers, -All_Lenders,
         -Deal_Amount, -Deal_Currency, -Deal_Refinancing, -Purpose_Remark, -Sales_Size_at_Close,
         -Tranche_Amount, -League_Table_Amount, -League_Table_Amount_Converted, -Tranche_Currency, 
         -Market_Of_Syndication, -Deal_Active_Date, 
         -Collateral_Security_Type, -Project_Finance_Sponsor, -Closed_Date, -Completion_Date, -Assignment_Fee,
         -Base_Reference_Rate, -Margin_bps, -All_Base_Rate_Spread_Margin, -Base_Rate_Margin_bps, -Floor_bps,
         -All_in_Spread_Undrawn_bps, -Covenants, -EBITDA_Initial_Amount_USD, -EBITDA_Final_Amount_USD,
         -CAPEX_Initial_USD, -CAPEX_Final_USD, -Property_Plant_Equipment, -Multi_Currency,
         -Deal_Input_Date) %>% 
          distinct()

dsdata_fl <- dsdata_fl %>% 
             select(LPC_Tranche_ID, LPC_Deal_ID, League_Table_Tranche_Date , 
             All_In_Spread_Drawn_bps, Tenor_Maturity,
             everything())

colnames(dsdata_fl)

dsdata_fl <- dsdata_fl %>%
            arrange(LPC_Tranche_ID, League_Table_Tranche_Date)

dsdata_fl <- dsdata_fl %>%
             group_by(LPC_Tranche_ID) %>%
             # Selecting the first row for each League_Table_Tranche_Date within each group
             slice(1L) %>%
             ungroup()

dsdata_fl <- dsdata_fl %>%
             mutate(sic = substr(SIC_Code, 1, 2))

dsdata_fl <- dsdata_fl %>%
             select(sic, everything())

dsdata_fl <- dsdata_fl %>% 
             filter(!(sic >= 60 & sic <= 67))   

# Save the updated dataframe to a new CSV file using fwrite
fwrite(dsdata_fl, "filtered_dealscan.csv")

