# This file renders the layout of the app

# Import necessary libraries
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)

# import dataWrangling and graphs

source("graphs.R")

app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
app$layout(
  dbcContainer(
    dbcRow(
      list(
        dbcCol(
          list(
            htmlLabel('Left'),
            dccDropdown(
              options = list(list(label = "New York City", value = "NYC"),
                             list(label = "San Francisco", value = "SF")),
              value = 'NYC'
            )
          )
        ),
        dbcCol(
          list(
            htmlLabel('Right'),
            dccDropdown(
              options = list(list(label = "New York City", value = "NYC"),
                             list(label = "San Francisco", value = "SF")),
              value = 'SF'
            )
          )
        )
      )
    ), style = list('max-width' = '85%')  # Change left/right whitespace for the container
  )
)
app$run_server(dubug = T)

