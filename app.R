# This file renders the layout of the app

# Import necessary libraries
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
# library(rstudioapi)

# import dataWrangling and graphs
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("src/dataWrangling.R")
source("src/graphs.R")

app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
app$layout(
  dbcNavbarSimple(
    brand = "Canada 1990-2010 GDP Analysis",
    color = "primary",
    dark = TRUE,
    brand_href = "#"
  )
)
app$run_server(dubug = T)

