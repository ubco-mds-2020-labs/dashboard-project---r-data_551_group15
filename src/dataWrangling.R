# This file load&cleans the data.
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(reshape)
library(ggpubr)
library(cowplot)
library(here)

# load the all data sets
Canada_economic_indicators <- read.csv(here('data','raw','Canada_economic_indicators.csv'))
Canada_economic_indicators_pct <- read.csv(here('data','raw','Canada_economic_indicators_pct.csv'))
earning_pct <- read.csv(here('data','raw','earnings_pct.csv'))
earning <- read.csv(here('data','raw','earnings.csv'))
industry_gdp <- read.csv(here('data','raw','industry_gdp.csv'))
industry_gdp_pct <- read.csv(here('data','raw','industry_gdp_pct.csv'))

# data wranglings 
Canada_economic_indicators <- transform(Canada_economic_indicators, Year = as.character(Canada_economic_indicators$Year))
Canada_economic_indicators_pct <- transform(Canada_economic_indicators_pct, Year = as.character(Canada_economic_indicators$Year))
colnames(Canada_economic_indicators) <- c("Geography", "Year", "Expenditure based GDP (2012 chained C$)", "Expenditure based GDP (current C$)", "Income based GDP (current C$)", "Industry GDP (current C$)", "Industry GDP (chained C$)", "CPI all-items", "CPI gasoline", "Labour force", 'Employment', 'Unemployment', 'Unemployment rate', 'Population', "Expenditure based GDPpC (2012 chained C$)", "Expenditure based GDPpC (current C$)", "Income based GDPpC (current C$)", "Industry GDPpC (current C$)", "Industry GDPpC (chained C$)")
colnames(Canada_economic_indicators_pct) <- c("Geography", "Year", "Expenditure based GDP (2012 chained C$)", "Expenditure based GDP (current C$)", "Income based GDP (current C$)", "Industry GDP (current C$)", "Industry GDP (chained C$)", "CPI all-items", "CPI gasoline", "Labour force", 'Employment', 'Unemployment', 'Unemployment rate', 'Population', "Expenditure based GDPpC (2012 chained C$)", "Expenditure based GDPpC (current C$)", "Income based GDPpC (current C$)", "Industry GDPpC (current C$)", "Industry GDPpC (chained C$)")
Canada_economic_indicators$'Unemployment rate' <- Canada_economic_indicators$'Unemployment' / Canada_economic_indicators$"Labour force"
Canada_economic_indicators_pct$'Unemployment rate' <- Canada_economic_indicators$'Unemployment' / Canada_economic_indicators$"Labour force"


earning <- transform(earning, Year = as.character(earning$Year))
colnames(earning) <- c("Geography", "Year", "All industries", "Goods producing sector", "Service producing sector")
earning_pct <- transform(earning_pct, Year = as.character(earning$Year))
colnames(earning_pct) <- c("Geography", "Year", "All industries", "Goods producing sector", "Service producing sector")

industry_gdp <- transform(industry_gdp, Year = as.character(industry_gdp$Year))
colnames(industry_gdp) <- c("Geography", 'Industry', "Year", "Industry GDP (current C$)", "Industry GDP (chained C$)")
industry_gdp_pct <- transform(industry_gdp_pct, Year = as.character(industry_gdp$Year))
colnames(industry_gdp_pct) <- c("Geography", 'Industry', "Year", "Industry GDP (current C$)", "Industry GDP (chained C$)")

