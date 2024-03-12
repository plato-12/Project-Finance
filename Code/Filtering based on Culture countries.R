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

setwd("K:/National Culture and Project Finance/Profs Data/Workspace/R codes/National Culture and PF")

#Filtering of deal, industry, firm and country level data
#by the available Hofstede Culture countries
firms <- fread("firms_level.csv")
industry <- fread("industry_level.csv")
deal <- fread("deal_level.csv")
ctrylvl <- fread("country_level.csv")

# List of ISO codes of Hostede Culture
iso_list <- c("AFE", "AFW", "ALB", "ALG", "AND", "ARA", "ARG", "ARM", "AUL", "AUT",
              "AZE", "BAN", "BLR", "BEL", "BEF", "BEN", "BOS", "BRA", "BUL", "BUF",
              "CAN", "CAF", "CHL", "CHI", "COL", "COS", "CRO", "CYP", "CZE", "DEN",
              "DOM", "ECA", "EGY", "ETH", "SAL", "EST", "FIN", "FRA", "GEO", "GER",
              "GEE", "GHA", "GBR", "GRE", "GUA", "HOK", "HUN", "ICE", "IND", "IDO",
              "IRA", "IRQ", "IRE", "ISR", "ITA", "JAM", "JPN", "JOR", "KOR", "KYR",
              "LAT", "LIT", "LUX", "MAC", "MAL", "MLI", "MLT", "MEX", "MOL", "MNG",
              "MOR", "NET", "NZL", "NIG", "NOR", "PAK", "PAN", "PER", "PHI", "POL",
              "POR", "PUE", "ROM", "RUS", "RWA", "SAU", "SER", "SIN", "SLK", "SLV",
              "SAF", "SAW", "SPA", "SUR", "SWE", "SWI", "SWF", "SWG", "TAI", "TAN",
              "THA", "TRI", "TUR", "USA", "UGA", "UKR", "URU", "VEN", "VIE", "ZAM", "ZIM")

# Filtering of firm data
firms <- firms[firms$loc %in% iso_list, ]

#------------------- Deal Level----------------------------------

# Mapping table from country names to ISO codes
country_iso_mapping <- data.frame(
  
              country = c("Africa East", "Africa West", "Albania", "Algeria", "Andorra", "Arab countries", 
              "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bangladesh", 
              "Belarus", "Belgium", "Belgium French", "Belgium Netherl", "Bosnia", "Brazil", 
              "Bulgaria", "Burkina Faso", "Canada", "Canada French", "Chile", "China", 
              "Colombia", "Costa Rica", "Croatia", "Cyprus", "Czech Rep", "Denmark", 
              "Dominican Rep", "Ecuador", "Egypt", "Ethiopia", "El Salvador", "Estonia", 
              "Finland", "France", "Georgia", "Germany", "Germany East", "Ghana", 
              "Great Britain", "Greece", "Guatemala", "Hong Kong", "Hungary", "Iceland", 
              "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", 
              "Jamaica", "Japan", "Jordan", "Korea South", "Kyrgyz Rep", "Latvia", 
              "Lithuania", "Luxembourg", "Macedonia Rep", "Malaysia", "Mali", "Malta", 
              "Mexico", "Moldova", "Montenegro", "Morocco", "Netherlands", "New Zealand", 
              "Nigeria", "Norway", "Pakistan", "Panama", "Peru", "Philippines", "Poland", 
              "Portugal", "Puerto Rico", "Romania", "Russia", "Rwanda", "Saudi Arabia", 
              "Serbia", "Singapore", "Slovak Rep", "Slovenia", "South Africa", 
              "South Africa white", "Spain", "Suriname", "Sweden", "Switzerland", 
              "Switzerland French", "Switzerland German", "Taiwan", "Tanzania", "Thailand", 
              "Trinidad and Tobago", "Turkey", "U.S.A.", "Uganda", "Ukraine", "Uruguay", 
              "Venezuela", "Vietnam", "Zambia", "Zimbabwe"),
  
  iso = c("AFE", "AFW", "ALB", "ALG", "AND", "ARA", "ARG", "ARM", "AUL", "AUT", "AZE", 
          "BAN", "BLR", "BEL", "BEF", "BEN", "BOS", "BRA", "BUL", "BUF", "CAN", "CAF", 
          "CHL", "CHI", "COL", "COS", "CRO", "CYP", "CZE", "DEN", "DOM", "ECA", "EGY", 
          "ETH", "SAL", "EST", "FIN", "FRA", "GEO", "GER", "GEE", "GHA", "GBR", "GRE", 
          "GUA", "HOK", "HUN", "ICE", "IND", "IDO", "IRA", "IRQ", "IRE", "ISR", "ITA", 
          "JAM", "JPN", "JOR", "KOR", "KYR", "LAT", "LIT", "LUX", "MAC", "MAL", "MLI", 
          "MLT", "MEX", "MOL", "MNG", "MOR", "NET", "NZL", "NIG", "NOR", "PAK", "PAN", 
          "PER", "PHI", "POL", "POR", "PUE", "ROM", "RUS", "RWA", "SAU", "SER", "SIN", 
          "SLK", "SLV", "SAF", "SAW", "SPA", "SUR", "SWE", "SWI", "SWF", "SWG", "TAI", 
          "TAN", "THA", "TRI", "TUR", "USA", "UGA", "UKR", "URU", "VEN", "VIE", "ZAM", 
          "ZIM")
)

# Assuming your dataset is named 'deal' and the country names are under the 'Country' column
# Filter and convert the country names to ISO codes
deal_filtered <- merge(deal, country_iso_mapping, by.x = "Country", by.y = "country", all.x = TRUE)

# Drop rows with NA values in the "iso" column
deal_filtered <- deal_filtered[deal_filtered$iso %in% iso_list, ]


#------------Country Level-------------------------------------------------- 

#Filtering Country level data
# Filter and convert the country names to ISO codes
ctrylvl_filtered <- merge(ctrylvl, country_iso_mapping, by.x = "Country", by.y = "country", all.x = TRUE)

# Drop rows with NA values in the "iso" column
ctrylvl_filtered <- ctrylvl_filtered[ctrylvl_filtered$iso %in% iso_list, ]



fwrite(deal_filtered, "deal_filtered.csv")
fwrite(firms, "firms_filtered.csv")
fwrite(ctrylvl_filtered, "country_filtered.csv")


