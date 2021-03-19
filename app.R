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
source("src/prepare_dropdown_menu.R")

# App layout:
app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
app$layout(
  dbcContainer(
    children = list(
      # header nav:
      dbcNavbarSimple(
        #Data Sources:
        dbcDropdownMenu(
          children = list(
            dbcDropdownMenuItem("temp",href = "#")
          ),
          nav = TRUE,
          in_navbar = TRUE,
          label = "Data Sources"
        ),
        #Other Nav paras:
        brand = "Canada 2010-2020 GDP Analysis",
        color = "primary",
        dark = TRUE,
        brand_href = "#"
      ),
      dbcRow(
        list(
          dbcCol(
            # Dropdown:
            list(
              htmlLabel("Select Province:"),
              dccDropdown(
                id = "province",
                options = geos,
                value = "British Columbia"
              )
            )
          ),
          dbcCol(
            # Slider:
            list(
              htmlLabel("Select Year:"),
              dccSlider(id = "year", min = as.numeric(Years[[1]]), max = as.numeric(Years[[11]]), value = as.numeric(Years[[6]]), marks = list("2010" = 2010, "2015" = 2015, "2020" = 2020), tooltip = "top")
            )
          )
        )
      ),
      dbcRow(
        dbcCol(
          # Tabs:
          dbcTabs(
            children = list(
              dbcTab(label = "GDP"),
              dbcTab(label = "GDP per Capita"),
              dbcTab(label = "Conusmer Price Index(CPI)"),
              dbcTab(label = "Employment and Earnings")
              )
          )
        )
      )
    )
    ,style = list('max-width' = '85%')  # Change left/right whitespace for the container
  )
)
app$run_server(dubug = T)

