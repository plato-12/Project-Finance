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
setwd("K:/National Culture and Project Finance/Profs Data/Workspace/R codes/Compustat")
firmsg <- fread("Quarterly Firm Data Compu Global 1986 to 2022.csv")
firmsn <- fread("Quarterly Firm Data Compu NA.csv")

#Remove columns from Compustat Global
firmsg <- firmsg %>% 
          select(-ceqq_dc,-cheq_dc, -chq_dc, -dlcq_dc, -dlttq_dc, 
                 -dpq_dc, -ppentq_dc, -req_dc, -capxy_dc, -dvy_dc, -fyr, -dvtq, -fic)

#Remove columns from Compustat North America
firmsn <- firmsn %>% 
          select(-curncdq, -datacqtr, -datafqtr)



#Arranges the following columns in 
firmsn <- firmsn %>% 
          arrange(gvkey, indfmt, consol, popsrc, datafmt, 
                  curcdq,fqtr, fyearq, datadate, actq, atq, 
                  ceqq, cheq, chq, dlcq, dlttq, dpq,ibq, 
                  lctq, ltq, ppentq, req, revtq, teqq, 
                  capxy, chechy, dvy,oancfy, conm, costat,
                  loc, sic)

#Retrieves the columns on firmsg and firmsn 
#(Both have same number and columns name)
names(firmsg)
names(firmsn)

#Create firms dataset
#This operation essentially stacks the rows of firmsg on top of the rows 
#of firmsn, assuming the columns of both datasets match (same names and types)
firms <- rbind(firmsg, firmsn) 


#Remove financial industries
firms <- firms %>%
         mutate(sic_2 = substr(sic, 1, 2))

firms <- firms %>%
         select(sic, sic_2, everything())

firms <- firms %>% 
         filter(!(sic_2 >= 60 & sic_2 <= 67))
#removes duplicate rows, keeping only the first occurrence of each unique value in the loc column
compucountries <- firms %>% 
                  select (loc) %>% 
                  distinct(loc, .keep_all = TRUE)

#update for countries whose EPU and Culture datas are available
#firms <- firms %>% 
         #mutate(ctry = case_when (loc == "AUS" ~ "Australia", loc == "BRA" ~ "Brazil", loc == "CAN" ~ "Canada",
                              #loc == "CHL" ~ "Chile", loc == "CHN" ~ "China", loc == "COL" ~ "Colombia", 
                              #loc == "FRA" ~ "France", loc == "DEU" ~ "Germany", loc == "GRC" ~ "Greece",
                              #loc == "IND" ~ "India", loc == "IRL" ~ "Ireland", loc == "ITA" ~ "Italy",
                              #loc == "JPN" ~ "Japan",  loc == "KOR" ~ "Korea", loc == "NLD" ~ "Netherlands",
                              #loc == "RUS" ~ "Russia", loc == "ESP" ~ "Spain", loc == "SGP" ~ "Singapore",
                              #loc == "MEX" ~ "Mexico", loc == "SWE" ~ "Sweden", loc == "GBR" ~ "UK",
                              #loc == "USA" ~ "USA")
                #)

#filters the dataset to keep only the rows where the ctry column does not contain missing values.
#firms <-  firms %>% 
          #filter(is.na(ctry) == FALSE)

firms <- firms %>%
         mutate (
                 #Variables from the Project Finance/Creditor rights paper and the ones explicitly asked to include
                 #Sales Growth is added later in the "df" dataframe
           
                 tang = ppentq/atq,                 #Asset Tangibility
                 fcfbyat = ((ibq+dpq)-(dvy/4)/atq), #Free Cash flow to Assets
                 ltlev =  dlttq/atq,                #Long Term Debt to Total Assets ratio
                 roa = ibq/atq,                     #Return on Assets
                 ebitdamar = (ibq+dpq)/revtq,       #EBITDA Margin
                 stlev = dlcq/atq,                  #Short Term Leverage
                 
                 #Other variabels from the old file.......................
                 
                 chbyat = chq/atq,  #Cash Holdings to Total Assets ratio
                 chebyat = cheq/atq,#Cash and Equivalents to Total Assets ratio
                 cfbyat = (ibq+dpq)/atq, #Cash Flow to Assets ratio
                 
                 #A dummy variable indicating positive cash flow to assets 
                 #(1 if cfbyat > 0, else 0)
                 poscfdum = if_else(cfbyat > 0, 1, 0),
                 
                 nwc = actq - lctq,  #Net Working Capital
                 nwcbyat = (actq-lctq)/atq,#Net Working Capital to Total Assets ratio
                 capexbyaty = capxy/(4*atq), #Capital Expenditure to Total Assets ratio
                 oancfytcapex = oancfy/capxy, #Cash flow from Operations to Capital Expenditure
                 lev1 = ltq/atq, #Total Leverage ratio
                 lev2 = (dlcq+dlttq)/atq, #Alternative Total Leverage ratio
                 divbaty = dvy/(4*atq), #Dividends to Total Assets ratio
                 
                 #A dummy variable indicating dividend payment
                 #(1 if dvy > 0 and not missing, else 0)
                 divdummy = if_else(dvy > 0 & is.na(dvy) == FALSE, 1, 0),
                 
                 lifecycle = req/teqq, #Lifecycle ratio
                 teqratio = teqq/atq,  #Total Equity to Total Assets ratio
                 ceqratio = ceqq/atq)  #Common Equity to Total Assets ratio

#Store unique gvkey in df
df <- firms %>%
      select (gvkey) %>%
      distinct(gvkey) 
      
#Replicate each gvkey value 140 times
df <- data.frame(gvkey= rep(df$gvkey,140))

#df will be sorted by gvkey, grouped by gvkey,
#each row within each group will have a corresponding qtrn value 
#representing its position within the group.
df <- df %>%
      arrange(gvkey) %>% 
      group_by(gvkey) %>% 
      mutate(qtrn = row_number())

#qtrn represent the quarter number relative to the year 1986
firms <- firms %>% 
         mutate(qtrn = (fyearq-1986)*4 + fqtr) %>% 
         select(gvkey, fqtr, fyearq, qtrn, everything()) %>% 
         arrange(gvkey, qtrn)


firms <- firms %>% 
         mutate(qtrem = (datadate %% 10000) %/% 100, #quarter extracted
                cyear = datadate %/% 10000) %>%      #year extracted
         select(gvkey, fqtr, fyearq, qtrn, qtrem, everything())


#firms dataset merged based on the matching values in the gvkey and qtrn col. 
#Rows in df that do not have matching values in firms will have missing values 
#in the newly added columns
df<-    df %>% 
        left_join(firms, by=c('gvkey','qtrn'))

df <- df %>% 
            mutate(lagcheq = dplyr::lag(cheq),#Lagged value of cash and equivalents
                   lagchq = dplyr::lag(chq),#Lagged value of cash holdings
                   lagatq = dplyr::lag(atq),#Lagged value of total assets
                   lagrevtq = dplyr::lag(revtq),#Lagged value of revenue
                   
                   nwc = actq-lctq,#Net working capital, current assets - current liabilities 
                   
                   lagnwc = dplyr::lag(nwc),#Lagged value of net working capital
                   
                   totaldebt = dlttq+dlcq,#Total debt, long-term debt and short-term debt 
                   
                   lagtotaldebt = dplyr::lag(totaldebt), #Lagged value of total debt
                   lagltq = dplyr::lag(ltq), #Lagged value of long-term debt
                   
                   deltachebyatq = (cheq - lagcheq)/lagatq, #Change in cash and equivalents 
                                                            #to total assets ratio
                   
                   deltachbyatq = (chq - lagchq)/lagatq,#Change in cash holdings to total assets ratio
                   deltarevtbyatq = (revtq - lagrevtq)/lagatq,#Change in revenue to total assets ratio
                   deltanwcbyatq = (nwc - lagnwc)/lagatq,#Change in net working capital to total assets ratio
                   deltalev1 = (ltq - lagltq)/lagatq,#Change in leverage ratio 1
                   deltalev2 = (totaldebt - lagtotaldebt)/lagatq, #Change in leverage ratio 2
                   
                   salgrwth = (revtq-lagrevtq)/lagrevtq)#Sales Growth
  

df <- df %>%  
      filter(is.na(datadate) == FALSE) %>% #filters out rows where datadate col is not missing
      mutate (sic2dig = sic %/% 100) %>% #(SIC) code truncated to the two-digit level
      arrange (loc, sic2dig, qtrn, gvkey ) %>% 
      select(gvkey, fqtr, fyearq,loc, sic, qtrn, everything()) %>% 
      group_by (loc,sic2dig, qtrn) %>% 
      mutate (revtsumq = sum(revtq, na.rm = TRUE)) %>% #calculates the sum of revenue 
                                                       #within each group (loc, sic2dig, qtrn)
      ungroup() %>% 
      mutate( revtshareq = revtq/revtsumq, #share of revenue relative to total revenue
              revtsharesqq = revtshareq ^ 2) %>% #square of revtshareq
      group_by (loc, sic2dig, qtrn) %>%
      mutate(hhiq = sum(revtsharesqq, na.rm = TRUE)) %>% #Herfindahl-Hirschman Index (HHI) within each group
      select(gvkey, fqtr, fyearq,loc,sic, sic2dig, qtrn, revtq,revtsumq, 
             revtshareq, revtsharesqq, hhiq, everything()) %>% 
      ungroup()

#rolling standard deviation roa (Return on Assets) for each gvkey
df <- df %>% 
      group_by(gvkey) %>%      
      mutate(roaqvol = rollapply(roa, width = 5, FUN = sd, fill = NA, align = "right")) %>% 
      ungroup()

       
df <- df %>% 
      mutate(cyear = datadate %/% 10000)

#Dataframe for industry level data
# Group by 'sic2dig' and 'qtrn', and calculated the median for numeric columns
#industry_data <- df %>%
  #group_by(sic2dig, qtrn) %>%
  #summarize(across(where(is.numeric), median, na.rm = TRUE))
industry_data <- df %>%
                 group_by(sic2dig, qtrn) %>%
                 summarize(across(where(is.numeric), \(x) median(x, na.rm = TRUE)), .groups = "drop")


#Save the file
fwrite(firms , "firms_level.csv")
fwrite(industry_data, "industry_level.csv")


