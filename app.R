# This file renders the layout of the app

# Import necessary libraries
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
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
        brand = "Canada 2010-2020 Economy Analysis",
        color = "primary",
        dark = TRUE,
        brand_href = "#"
      ),
      dbcRow(
        list(
          dbcCol(
            # Dropdown:
            list(
              htmlLabel(
                htmlH5("Select Province:")
              ),
              dccDropdown(
                id = "province",
                options = geos,
                value = "Canada"
              )
            )
          ),
          dbcCol(
            # Slider:
            list(
              htmlLabel(
                htmlH5("Select Year:")
              ),
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
              dbcTab(
                label = "GDP",
                children = list(
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t1_1")
                    )
                  ),
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t1_2")
                    )
                  ),
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t1_3")
                    )
                  ),
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t1_4")
                    )
                  )
                )
              ),
              dbcTab(
                label = "GDP per Capita",
                children = list(
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t2_1")
                    )
                  ),
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t2_2")
                    )
                  ),
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t2_3")
                    )
                  )
                )
              ),
              dbcTab(
                label = "Conusmer Price Index(CPI)",
                children = list(
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t3_1")
                    )
                  ),
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t3_2")
                    )
                  ),
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t3_3")
                    )
                  )
                )
              ),
              dbcTab(
                label = "Employment and Earnings",
                children = list(
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t4_1")
                    )
                  ),
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t4_2")
                    )
                  ),
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t4_3")
                    )
                  ),
                  dbcRow(
                    dbcCol(
                      dccGraph(id = "t4_4")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    ,style = list('max-width' = '85%')  # Change left/right whitespace for the container
  )
)

# call backs:
app$callback(
  list(output("t1_1","figure")),
  list(input("province", "value"), 
       input("year", "value")),
  function(province,year){
    g = tab1_one(year, province)
    return(list(g))
  }
)

app$callback(
  list(output("t1_2","figure")),
  list(input("province", "value"), 
       input("year", "value")),
  function(province,year){
    g = tab1_two(year, province)
    return(list(g))
  }
)

app$callback(
  list(output("t1_3","figure")),
  list(input("province", "value"), 
       input("year", "value")),
  function(province,year){
    g = tab1_three(year, province)
    return(list(g))
  }
)

# app$callback(
#   list(output("t1_4","figure")),
#   list(input("province", "value"), 
#        input("year", "value")),
#   function(province,year){
#     g = tab1_four(year, province)
#     return(list(g))
#   }
# )

app$callback(
  list(output("t2_1","figure")),
  list(input("province", "value"), 
       input("year", "value")),
  function(province,year){
    g = tab2_one(year, province)
    return(list(g))
  }
)

app$callback(
  list(output("t2_2","figure")),
  list(input("province", "value"), 
       input("year", "value")),
  function(province,year){
    g = tab2_two(year, province)
    return(list(g))
  }
)

app$callback(
  list(output("t3_1","figure")),
  list(input("province", "value"), 
       input("year", "value")),
  function(province,year){
    g = tab3_one(year, province)
    return(list(g))
  }
)

# app$callback(
#   list(output("t3_2","figure")),
#   list(input("province", "value"), 
#        input("year", "value")),
#   function(province,year){
#     g = tab3_two(year, province)
#     return(list(g))
#   }
# )

# app$callback(
#   list(output("t4_1","figure")),
#   list(input("province", "value"), 
#        input("year", "value")),
#   function(province,year){
#     g = tab4_one(year, province)
#     return(list(g))
#   }
# )

app$callback(
  list(output("t4_2","figure")),
  list(input("province", "value"), 
       input("year", "value")),
  function(province,year){
    g = tab4_two(year, province)
    return(list(g))
  }
)

# app$callback(
#   list(output("t4_3","figure")),
#   list(input("province", "value"), 
#        input("year", "value")),
#   function(province,year){
#     g = tab5_one(year, province)
#     return(list(g))
#   }
# )

app$callback(
  list(output("t4_4","figure")),
  list(input("province", "value"), 
       input("year", "value")),
  function(province,year){
    g = tab5_two(year, province)
    return(list(g))
  }
)

app$run_server(dubug = T)

