# This file draws graphs
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(reshape)
library(ggpubr)
library(cowplot)
library(rstudioapi)

# loads all wrangled data
source("dataWrangling.R")

year = 2019
geo = 'British Columbia'

# GDP Analysis summary:
tab1_one <- function(year,geo){
  first_gdp <- Canada_economic_indicators %>%
    filter(Geography == geo, Year == year) %>%
    select(Year, "Expenditure based GDP (2012 chained C$)", "Expenditure based GDP (current C$)", "Income based GDP (current C$)", "Industry GDP (chained C$)") %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_col(alpha=FALSE, aes(fill = variable)) + facet_wrap(~variable, nrow = 1) + geom_text(aes(label=scales::dollar(value), size=40, vjust=3, color=variable, fontface = "bold")) +
    labs(title='GDP Amount', x='GDP (Dollars x 1,000,000)') +
    scale_y_continuous(labels = scales::label_dollar()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      strip.text.x = element_text(size = 18)) 
  
  first_gdp
}

# GDP Evolution:
tab1_two <- function(year,geo){
  two <- Canada_economic_indicators %>%
    filter(Geography == geo, Year <= year) %>%
    select(Year, "Expenditure based GDP (2012 chained C$)", "Expenditure based GDP (current C$)", "Income based GDP (current C$)", "Industry GDP (chained C$)") %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_area(aes(fill = variable, group = 1)) + facet_grid(variable~.) +
    labs(title='GDP Evolution', y='GDP (Dollars x 1,000,000)', x='Year') +
    scale_y_continuous(labels = scales::label_dollar()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.y=element_text(size=12),
      axis.text.x=element_text(size=12),
      axis.ticks.x=element_blank(),
      strip.text.y.right = element_blank()) 
  
  three <- Canada_economic_indicators_pct %>%
    filter(Geography == geo, Year <= year) %>%
    select(Year, "Expenditure based GDP (2012 chained C$)", "Expenditure based GDP (current C$)", "Income based GDP (current C$)", "Industry GDP (chained C$)") %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_col(aes(fill = variable)) + facet_grid(variable~.) + geom_text(aes(label=scales::percent(value), vjust=-1)) + 
    labs(title='GDP Growth Rate', y='Growth Rate %', x='Year') +
    scale_y_continuous(labels = scales::label_percent()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.y=element_text(size=12),
      axis.text.x=element_text(size=12),
      axis.ticks.x=element_blank(),
      strip.text.y.right = element_text(size = 15, angle = 0))
  
  second_gdp <- ggarrange(two, three, ncol = 2)
  second_gdp
}

# GDP geo compare:
tab1_three <- function(year,geo){
  
}

# Test:
# tab1_two(year,geo)
