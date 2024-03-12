
setwd("K:/National Culture and Project Finance/Profs Data/Workspace/R codes/EPU")
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


epu <- fread('All_Country_Data.csv')
colnames(epu)
annual_epu <- epu %>%
              group_by(Year) %>%
              summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Define a vector of new column names
new_column_names <- c("Year", "Month", "GEPU_current", "GEPU_ppp", 
                      "AUS", "BRA", "CAN", "CHL", 
                      "Hybrid CHN", "COL", "FRA", "DEU", 
                      "GRC", "IND", "IRL", "ITA", 
                      "JPN", "KOR", "NLD", "RUS", 
                      "ESP", "SGP", "GBR", "USA", 
                      "SCMP CHN", "Mainland CHN", "SWE", "MEX")

# Rename columns
names(annual_epu) <- new_column_names

annual_epu <- annual_epu %>%
              select(-Month)

annual_epu_lg <- gather(annual_epu, key = "Country", value = "EPU", -Year)

fwrite(annual_epu, "annual_epu_wide.csv")
fwrite(annual_epu_lg, "annual_epu_long.csv")
