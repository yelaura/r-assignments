library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  titlePanel("San Francisco Pedestrian Safety"),
  mainPanel(
    dateRangeInput('dateRange',
                   label = 'Date Range Input: mm/dd/yyyy',
                   start = Sys.Date() - 2 * 365, end = Sys.Date(),
                   separator = " - ",
                   format = "mm/dd/yyyy"
    ),
    uiOutput("catSelector"),
    leafletOutput("showThePlot")
  )
))
