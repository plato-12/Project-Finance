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
setwd("K:/National Culture and Project Finance/Profs Data/Workspace/R codes/Country Level")

gfcf_wd <- fread("gross fixed capital formation.csv")
gfc_wd <- fread("gross fixed capital.csv")
inf_wd <- fread("inflation.csv")
mkcp_wd <- fread("market capitalization.csv")
unemp_wd <- fread("unemployment.csv")

prvcd_wd <- fread("Private credit.csv")
gdpgrw_wd <- fread("real gdp growth rate.csv")
gdpcp_wd <- fread("real gdp per capita.csv")

#Converting GFCF in long format
gfcf_wd <- gfcf_wd[-1, ]
colnames(gfcf_wd) <- c("Country", "1996", "1997", "1998", "1999", "2000", "2001", 
                       "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                       "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                       "2018", "2019", "2020", "2021", "2022")
gfcf_lg <- gather(gfcf_wd, key = "Year", value = "GFCF", -Country)

#Converting GFC in long format
gfc_wd <- gfc_wd[-1, ]
colnames(gfc_wd) <- c("Country", "1996", "1997", "1998", "1999", "2000", "2001", 
                       "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                       "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                       "2018", "2019", "2020", "2021", "2022")
gfc_lg <- gather(gfc_wd, key = "Year", value = "GFC", -Country)

#Converting Inflation in long format
inf_wd <- inf_wd[-1, ]
colnames(inf_wd) <- c("Country", "1996", "1997", "1998", "1999", "2000", "2001", 
                      "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                      "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                      "2018", "2019", "2020", "2021", "2022")
inf_lg <- gather(inf_wd, key = "Year", value = "Inflation", -Country)

#Converting Market Capitalization in long format
mkcp_wd <- mkcp_wd[-1, ]
colnames(mkcp_wd) <- c("Country", "1996", "1997", "1998", "1999", "2000", "2001", 
                      "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                      "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                      "2018", "2019", "2020", "2021", "2022")
mkcp_lg <- gather(mkcp_wd, key = "Year", value = "Market Capitalization", -Country)

#Converting Unemployment in long format
unemp_wd <- unemp_wd[-1, ]
colnames(unemp_wd) <- c("Country", "1996", "1997", "1998", "1999", "2000", "2001", 
                      "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                      "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                      "2018", "2019", "2020", "2021", "2022")
unemp_lg <- gather(unemp_wd, key = "Year", value = "Unemployment", -Country)

#Converting private credit in long format
prvcd_wd <- prvcd_wd[-1, ]
colnames(prvcd_wd) <- c("Country", "1996", "1997", "1998", "1999", "2000", "2001", 
                       "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                       "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                       "2018", "2019", "2020", "2021", "2022")
prvcd_lg <- gather(prvcd_wd, key = "Year", value = "Private Credit", -Country)

#Converting Real GDP growth rate in long format
gdpgrw_wd <- gdpgrw_wd[-1, ]
colnames(gdpgrw_wd) <- c("Country", "1996", "1997", "1998", "1999", "2000", "2001", 
                       "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                       "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                       "2018", "2019", "2020", "2021", "2022")
gdpgrw_lg <- gather(gdpgrw_wd, key = "Year", value = "Real Growth Rate", -Country)

#Converting Real GDP per capita in long format
gdpcp_wd <- gdpcp_wd[-1, ]
colnames(gdpcp_wd) <- c("Country", "1996", "1997", "1998", "1999", "2000", "2001", 
                       "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009",
                       "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017",
                       "2018", "2019", "2020", "2021", "2022")
gdpcp_lg <- gather(gdpcp_wd, key = "Year", value = "Real GDP per capita", -Country)

#save the long format dataset in files
fwrite(gfcf_lg, "gfcf_long.csv")
fwrite(gfc_lg, "gfc_long.csv")
fwrite(inf_lg, "inf_long.csv")
fwrite(mkcp_lg, "mkcp_long.csv")
fwrite(unemp_lg, "unemp_long.csv")

fwrite(prvcd_lg, "prvcd_long.csv")
fwrite(gdpgrw_lg, "gdpgrowth_long.csv")
fwrite(gdpcp_lg, "gdpcapita_long.csv")
