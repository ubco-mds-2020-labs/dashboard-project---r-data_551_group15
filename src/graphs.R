# This file draws graphs
library(tidyverse) 
library(ggplot2)
library(dplyr)
library(reshape)
library(ggpubr)
library(cowplot)
library(rstudioapi)

# loads all wrangled data
source("src/dataWrangling.R")

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
      plot.title = element_text(size = 15, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      strip.text.x = element_text(size = 9)) 
  
  ggplotly(first_gdp, height = 300)
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
      plot.title = element_text(size = 15, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.y=element_text(size=8),
      axis.text.x=element_text(size=8),
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
      plot.title = element_text(size = 15, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.y=element_text(size=12),
      axis.text.x=element_text(size=12),
      axis.ticks.x=element_blank(),
      strip.text.y.right = element_text(size = 9, angle = 0))
  
  second_gdp <- subplot(ggplotly(three),ggplotly(two))
  second_gdp
}

# GDP geo compare:
tab1_three <- function(year,geo){
  twelve <- Canada_economic_indicators %>%
    filter(Year == year) %>%
    select(Geography, "Expenditure based GDP (current C$)") %>%
    gather(key = "variable", value = "value", -Geography) %>%
    ggplot() +
    aes(x = reorder(Geography, -value), y = value) +
    geom_col(aes(fill = ifelse(Geography == geo, "Highlighted", "Normal"))) +
    labs(title='GDP Amount Geography Comparison', y='GDP (Dollars x 1,000,000)') +
    scale_y_continuous(labels = scales::label_dollar()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_text(size = 10, angle=90),
      axis.ticks.x=element_blank())
  
  thirteen <- Canada_economic_indicators_pct %>%
    filter(Year == year) %>%
    select(Geography, "Expenditure based GDP (current C$)") %>%
    gather(key = "variable", value = "value", -Geography) %>%
    ggplot() +
    aes(x = reorder(Geography, -value), y = value) +
    geom_col(aes(fill = ifelse(Geography == geo, "Highlighted", "Normal"))) +
    labs(title='GDP Growth Rate Geography Comparison') +
    scale_y_continuous(labels = scales::label_percent()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.x=element_text(size = 10, angle=90),
      axis.ticks.x=element_blank())
  
  third_gdp <- subplot(ggplotly(twelve), ggplotly(thirteen))
  
  third_gdp
}

# GDP indestry compare:
tab1_four <- function(year,geo){
  eighteen <- industry_gdp %>%
    filter(Geography == geo, Year == year) %>%
    select(Industry, "Industry GDP (chained C$)") %>%
    gather(key = "variable", value = "value", -Industry) %>%
    ggplot() +
    aes(y = reorder(Industry, value), x = value) +
    geom_col(aes(fill=variable)) +
    labs(title='GDP Amount by Industry', y='GDP (Dollars x 1,000,000)') +
    scale_x_continuous(labels = scales::label_dollar()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_text(size = 12)) 
  
  nineteen <- industry_gdp_pct %>%
    filter(Geography == geo, Year == year) %>%
    select(Industry, "Industry GDP (chained C$)") %>%
    gather(key = "variable", value = "value", -Industry) %>%
    ggplot() +
    aes(y = reorder(Industry, value), x = value) +
    geom_col(aes(fill = ifelse(value <= 0, "Highlighted", "Normal"))) + geom_text(aes(label=scales::percent(value), hjust=1)) + 
    labs(title='GDP Growth Rate by Industry', y='GDP (Dollars x 1,000,000)') +
    scale_x_continuous(labels = scales::label_percent()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_text(size = 12)) 
  
  fourth_gdp <- subplot(ggplotly(eighteen), ggplotly(nineteen))
  
  fourth_gdp
}
# tab1 <- function(year,geo){
#   
#   first_gdp <- Canada_economic_indicators %>%
#     filter(Geography == geo, Year == year) %>%
#     select(Year, "Expenditure based GDP (2012 chained C$)", "Expenditure based GDP (current C$)", "Income based GDP (current C$)", "Industry GDP (chained C$)") %>%
#     gather(key = "variable", value = "value", -Year) %>%
#     ggplot() +
#     aes(x = Year, y = value) +
#     geom_col(alpha=FALSE, aes(fill = variable)) + facet_wrap(~variable, nrow = 1) + geom_text(aes(label=scales::dollar(value), size=40, vjust=3, color=variable, fontface = "bold")) +
#     labs(title='GDP Amount', x='GDP (Dollars x 1,000,000)') +
#     scale_y_continuous(labels = scales::label_dollar()) + 
#     theme_minimal() + theme( 
#       plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
#       legend.position = 'none',
#       panel.grid.major = element_blank(), 
#       panel.grid.minor = element_blank(), 
#       panel.background = element_blank(),
#       axis.ticks.y=element_blank(),
#       axis.title.y=element_blank(),
#       axis.text.y=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank(),
#       strip.text.x = element_text(size = 18)) 
#   
#   three <- Canada_economic_indicators_pct %>%
#     filter(Geography == geo, Year <= year) %>%
#     select(Year, "Expenditure based GDP (2012 chained C$)", "Expenditure based GDP (current C$)", "Income based GDP (current C$)", "Industry GDP (chained C$)") %>%
#     gather(key = "variable", value = "value", -Year) %>%
#     ggplot() +
#     aes(x = Year, y = value) +
#     geom_col(aes(fill = variable)) + facet_grid(variable~.) + geom_text(aes(label=scales::percent(value), vjust=-1)) + 
#     labs(title='GDP Growth Rate', y='Growth Rate %', x='Year') +
#     scale_y_continuous(labels = scales::label_percent()) + 
#     theme_minimal() + theme( 
#       plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
#       legend.position = 'none',
#       panel.grid.major = element_blank(), 
#       panel.grid.minor = element_blank(), 
#       panel.background = element_blank(),
#       axis.ticks.y=element_blank(),
#       axis.text.y=element_text(size=12),
#       axis.text.x=element_text(size=12),
#       axis.ticks.x=element_blank(),
#       strip.text.y.right = element_text(size = 15, angle = 0))
#   
#   twelve <- Canada_economic_indicators %>%
#     filter(Year == year) %>%
#     select(Geography, "Expenditure based GDP (current C$)") %>%
#     gather(key = "variable", value = "value", -Geography) %>%
#     ggplot() +
#     aes(x = reorder(Geography, -value), y = value) +
#     geom_col(aes(fill = ifelse(Geography == geo, "Highlighted", "Normal"))) +
#     labs(title='GDP Amount Geography Comparison', y='GDP (Dollars x 1,000,000)') +
#     scale_y_continuous(labels = scales::label_dollar()) + 
#     theme_minimal() + theme( 
#       plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
#       legend.position = 'none',
#       panel.grid.major = element_blank(), 
#       panel.grid.minor = element_blank(), 
#       panel.background = element_blank(),
#       axis.title.x=element_blank(),
#       axis.text.x=element_text(size = 10, angle=90),
#       axis.ticks.x=element_blank())
#   
#   thirteen <- Canada_economic_indicators_pct %>%
#     filter(Year == year) %>%
#     select(Geography, "Expenditure based GDP (current C$)") %>%
#     gather(key = "variable", value = "value", -Geography) %>%
#     ggplot() +
#     aes(x = reorder(Geography, -value), y = value) +
#     geom_col(aes(fill = ifelse(Geography == geo, "Highlighted", "Normal"))) +
#     labs(title='GDP Growth Rate Geography Comparison') +
#     scale_y_continuous(labels = scales::label_percent()) + 
#     theme_minimal() + theme( 
#       plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
#       legend.position = 'none',
#       panel.grid.major = element_blank(), 
#       panel.grid.minor = element_blank(), 
#       panel.background = element_blank(),
#       axis.title.x=element_blank(),
#       axis.title.y=element_blank(),
#       axis.text.x=element_text(size = 10, angle=90),
#       axis.ticks.x=element_blank())
#   
#   eighteen <- industry_gdp %>%
#     filter(Geography == geo, Year == year) %>%
#     select(Industry, "Industry GDP (chained C$)") %>%
#     gather(key = "variable", value = "value", -Industry) %>%
#     ggplot() +
#     aes(y = reorder(Industry, value), x = value) +
#     geom_col(aes(fill=variable)) +
#     labs(title='GDP Amount by Industry', x='GDP (Dollars x 1,000,000)') +
#     scale_x_continuous(labels = scales::label_dollar()) + 
#     theme_minimal() + theme( 
#       plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
#       legend.position = 'none',
#       panel.grid.major = element_blank(), 
#       panel.grid.minor = element_blank(), 
#       panel.background = element_blank(),
#       axis.title.y=element_blank(),
#       axis.text.y=element_text(size = 12)) 
#   
#   nineteen <- industry_gdp_pct %>%
#     filter(Geography == geo, Year == year) %>%
#     select(Industry, "Industry GDP (chained C$)") %>%
#     gather(key = "variable", value = "value", -Industry) %>%
#     ggplot() +
#     aes(y = reorder(Industry, value), x = value) +
#     geom_col(aes(fill = ifelse(value <= 0, "Highlighted", "Normal"))) + geom_text(aes(label=scales::percent(value), hjust=1)) + 
#     labs(title='GDP Growth Rate by Industry', x='GDP (Dollars x 1,000,000)') +
#     scale_x_continuous(labels = scales::label_percent()) + 
#     theme_minimal() + theme( 
#       plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
#       legend.position = 'none',
#       panel.grid.major = element_blank(), 
#       panel.grid.minor = element_blank(), 
#       panel.background = element_blank(),
#       axis.title.y=element_blank(),
#       axis.text.y=element_text(size = 12)) 
#   
#   subplot(ggplotly(first_gdp), ggplotly(three), ggplotly(twelve), ggplotly(thirteen), ggplotly(eighteen),ggplotly(nineteen), nrows = 3)
# }

# GDP per capita:
tab2_one <- function(year,geo){
  first_gdp_capita <- Canada_economic_indicators %>%
    filter(Geography == geo, Year == year) %>%
    select(Year, "Expenditure based GDPpC (2012 chained C$)", "Expenditure based GDPpC (current C$)", "Income based GDPpC (current C$)", "Industry GDPpC (chained C$)") %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_col(alpha=FALSE, aes(fill = variable)) + facet_wrap(~variable, nrow = 1) + geom_text(aes(label=scales::dollar(value), size=40, vjust=3, color=variable, fontface = "bold")) +
    labs(title='GDP per Capita') +
    scale_y_continuous(labels = scales::label_dollar()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 15, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      strip.text.x = element_text(size = 9)) 
  
  ggplotly(first_gdp_capita, height = 300)
}

# GDP per capita evolution:
tab2_two <- function(year,geo){
  four <- Canada_economic_indicators %>%
    filter(Geography == geo, Year <= year) %>%
    select(Year, "Expenditure based GDPpC (2012 chained C$)", "Expenditure based GDPpC (current C$)", "Income based GDPpC (current C$)", "Industry GDPpC (chained C$)") %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_area(aes(fill = variable, group = 1)) + facet_grid(variable~.) +
    labs(title='GDP per Capita Evolution', y='GDP per Capita', x='Year') +
    scale_y_continuous(labels = scales::label_dollar()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 15, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.y=element_text(size=12),
      axis.text.x=element_text(size=12),
      axis.ticks.x=element_blank(),
      strip.text.y.right = element_blank()) 
  
  five <- Canada_economic_indicators_pct %>%
    filter(Geography == geo, Year <= year) %>%
    select(Year, "Expenditure based GDPpC (2012 chained C$)", "Expenditure based GDPpC (current C$)", "Income based GDPpC (current C$)", "Industry GDPpC (chained C$)") %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_col(aes(fill = variable)) + facet_grid(variable~.) + geom_text(aes(label=scales::percent(value), vjust=-1)) + 
    labs(title='GDP per Capita Growth Rate', y='Growth Rate %', x='Year') +
    scale_y_continuous(labels = scales::label_percent()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 15, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.y=element_text(size=12),
      axis.text.x=element_text(size=12),
      axis.ticks.x=element_blank(),
      strip.text.y.right = element_text(size = 10, angle = 0)) 
  
  second_gdp_capita <- subplot(ggplotly(four, height =500), ggplotly(five, height = 500))
  second_gdp_capita
}

# GDP per capita geo compare:
tab2_three <- function(year,geo){
  fourteen <- Canada_economic_indicators %>%
    filter(Year == year) %>%
    select(Geography, "Expenditure based GDPpC (current C$)") %>%
    gather(key = "variable", value = "value", -Geography) %>%
    ggplot() +
    aes(x = reorder(Geography, -value), y = value) +
    geom_col(aes(fill = ifelse(Geography == geo, "Highlighted", "Normal"))) +
    labs(title='GDP per Capita Geography Comparison', y='Dollars') +
    scale_y_continuous(labels = scales::label_dollar()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_text(size = 10, angle=90),
      axis.ticks.x=element_blank())
  
  fifteen <- Canada_economic_indicators_pct %>%
    filter(Year == year) %>%
    select(Geography, "Expenditure based GDPpC (current C$)") %>%
    gather(key = "variable", value = "value", -Geography) %>%
    ggplot() +
    aes(x = reorder(Geography, -value), y = value) +
    geom_col(aes(fill = ifelse(Geography == geo, "Highlighted", "Normal"))) +
    labs(title='GDP per Capita Growth Rate Geography Comparison') +
    scale_y_continuous(labels = scales::label_percent()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.x=element_text(size = 10, angle=90),
      axis.ticks.x=element_blank())
  
  third_gdp_capita <- ggarrange(fourteen, fifteen, ncol = 2)
  
  third_gdp_capita
}

# CPI:
tab3_one <- function(year, geo){
  six <- Canada_economic_indicators %>%
    filter(Geography == geo, Year <= year) %>%
    select(Year, "CPI all-items", "CPI gasoline") %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_area(aes(fill = variable, group = 1)) + facet_grid(variable~.) + geom_text(aes(label=value, vjust=-1, fontface='bold')) +
    labs(title='CPI Evolution', y='2002 = 100%', x='Year') +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_minimal() + theme( 
      plot.title = element_text(size = 15, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.y=element_text(size=12),
      axis.text.x=element_text(size=12),
      axis.ticks.x=element_blank(),
      strip.text.x = element_text(size = 9))
  
  seven <- Canada_economic_indicators_pct %>%
    filter(Geography == geo, Year <= year) %>%
    select(Year, "CPI all-items", "CPI gasoline") %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_col(aes(fill = variable)) + facet_grid(variable~.) + geom_text(aes(label=scales::percent(value), vjust=-1)) + 
    labs(title='CPI Growth Rate', y='Growth Rate %', x='Year') +
    scale_y_continuous(labels = scales::label_percent()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 15, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.y=element_text(size=12),
      axis.text.x=element_text(size=12),
      axis.ticks.x=element_blank(),
      strip.text.x = element_text(size = 9)) 
  
  first_cpi <- subplot(ggplotly(six,height = 500), ggplotly(seven, height = 500))
  
  first_cpi
}

# CPI geo compare:
tab3_two <- function(year, geo){
  sixteen <- Canada_economic_indicators %>%
    filter(Year == year) %>%
    select(Geography, "CPI all-items") %>%
    gather(key = "variable", value = "value", -Geography) %>%
    ggplot() +
    aes(x = reorder(Geography, -value), y = value) +
    geom_col(aes(fill = ifelse(Geography == geo, "Highlighted", "Normal"))) +
    labs(title='CPI Geography Comparison', y='2002 = 100%') +
    scale_y_continuous(labels = scales::label_comma()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.text.x=element_text(size = 10, angle=90),
      axis.title.x=element_blank(),
      axis.ticks.x=element_blank())
  
  seventeen <- Canada_economic_indicators_pct %>%
    filter(Year == year) %>%
    select(Geography, "CPI all-items") %>%
    gather(key = "variable", value = "value", -Geography) %>%
    ggplot() +
    aes(x = reorder(Geography, -value), y = value) +
    geom_col(aes(fill = ifelse(Geography == geo, "Highlighted", "Normal"))) +
    labs(title='CPI Growth Rate Geography Comparison') +
    scale_y_continuous(labels = scales::label_percent()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.x=element_text(size = 10, angle=90),
      axis.ticks.x=element_blank())
  
  second_cpi <- plotly(ggplotly(sixteen), ggplotly(seventeen))
  
  second_cpi
}

# Employment:
tab4_one <- function(year, geo){
  eight <- Canada_economic_indicators %>%
    filter(Geography == geo, Year <= year) %>%
    select(Year, 'Unemployment', 'Employment') %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_col(aes(fill = variable)) + geom_text(aes(label=scales::comma(value), vjust=1.5, fontface='bold')) +
    labs(title='Labour Force Evolution', y='Persons x 1,000', x='Year') +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'bottom',
      legend.title = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.y=element_text(size=12),
      axis.text.x=element_text(size=12),
      axis.ticks.x=element_blank(),
      strip.text.x = element_text(size = 18))
  
  nine <- Canada_economic_indicators %>%
    filter(Geography == geo, Year <= year) %>%
    select(Year, "Unemployment rate") %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_area(aes(fill = variable, group = 1))  + geom_text(aes(label=scales::percent(value), vjust=-1, fontface='bold')) +
    labs(title='Unemploymnet Rate Evolution', x='Year') +
    scale_y_continuous(labels = scales::label_percent()) +
    theme_minimal() + theme( 
      plot.title = element_text(size = 30, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_text(size=12),
      axis.text.x=element_text(size=12),
      axis.ticks.x=element_blank(),
      strip.text.x = element_text(size = 18))
  
  first_employment <- subplot(ggplotly(eight), ggplotly(nine), ncol = 2)
  first_employment
}

# Employment geo compare:
tab4_two <- function(year, geo){
  second_employment <- Canada_economic_indicators %>%
    filter(Year == year) %>%
    select(Geography, "Unemployment rate") %>%
    gather(key = "variable", value = "value", -Geography) %>%
    ggplot() +
    aes(x = reorder(Geography, -value), y = value) +
    geom_col(aes(fill = ifelse(Geography == geo, "Highlighted", "Normal"))) +
    labs(title='Unemployment Rate Geography Comparison') +
    scale_y_continuous(labels = scales::label_percent()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 15, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.x=element_text(size = 10),
      axis.ticks.x=element_blank())
  
  ggplotly(second_employment, height = 500)
}

# Income:
tab5_one <- function(year, geo){
  ten <- earning %>%
    filter(Geography == geo, Year <= year) %>%
    select(Year, "All industries", "Goods producing sector", "Service producing sector") %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_area(aes(fill = variable, group = 1)) + facet_grid(variable~.) +  geom_text(aes(label=scales::dollar(value), vjust=-1, fontface='bold')) +
    labs(title='Average Weekly Earnings', y='Dollars', x='Year') +
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
  
  eleven <- earning_pct %>%
    filter(Geography == geo, Year <= year) %>%
    select(Year, "All industries", "Goods producing sector", "Service producing sector") %>%
    gather(key = "variable", value = "value", -Year) %>%
    ggplot() +
    aes(x = Year, y = value) +
    geom_col(aes(fill = variable)) + facet_grid(variable~.) + geom_text(aes(label=scales::percent(value), vjust=-1)) + 
    labs(title='Average Weekly Earnings Evolution', y='Growth Rate %', x='Year') +
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
  
  first_earning <- subplot(ggplotly(ten), ggplotly(eleven), ncol = 2)
  first_earning
}

# Income geo compare:
tab5_two <- function(year, geo){
  second_earning <- earning %>%
    filter(Year == year) %>%
    select(Geography, "All industries") %>%
    gather(key = "variable", value = "value", -Geography) %>%
    ggplot() +
    aes(x = reorder(Geography, -value), y = value) +
    geom_col(aes(fill = ifelse(Geography == geo, "Highlighted", "Normal"))) +
    labs(title='Average Weekly Earnings Geography Comparison', y = 'Dollars') +
    scale_y_continuous(labels = scales::label_dollar()) + 
    theme_minimal() + theme( 
      plot.title = element_text(size = 15, face= 'bold', hjust = 0.5),
      legend.position = 'none',
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.background = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_text(size = 10),
      axis.ticks.x=element_blank())
  ggplotly(second_earning, height = 500)
}
# Test:
tab2_three(year,geo)
