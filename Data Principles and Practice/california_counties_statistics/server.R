#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgdal)
library(spdplyr)
library(dplyr)
library(tidyverse)

library(leaflet)
library(lubridate)

options(shiny.sanitize.errors = TRUE)

counties <- readOGR("C:\\Users\\lye\\Documents\\GitHub\\r-assignments\\Data Principles and Practice\\gz_2010_us_050_00_20m.json",
                    verbose = TRUE)

#fix column names
source("C:\\Users\\lye\\Downloads\\ShinyIntro\\shiny_california_monthly_employment\\fix_column_names.R")

counties@data <- counties@data %>%
  fix_column_names

#filter out only california counties
ca_counties <- counties %>%
  filter(state == "06")

#import statistical data
library(dplyr)
library(tidyverse)
emp_data <- read_tsv("C:\\Users\\lye\\Downloads\\ShinyIntro\\shiny_california_monthly_employment\\california_counties_monthly_employment_2016.tsv")

#fix column names
emp_data <- emp_data %>%
  fix_column_names %>%
  select(fips_county, period, labor_force, employed, unemployed, unemployed_rate) %>%
  mutate(fips_county = as.factor(fips_county)) %>%
  glimpse

names(emp_data) <- c("fips_county", "period", "labor_force", "employment", "unemployment", "unemployment_rate")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$myMap <- renderLeaflet({
    
    plot_data <- emp_data %>% 
      filter(month.name[month(period)] == input$month) %>%  
      select(fips_county, period, snake_case(input$stat))
    
    #clean up column names
    names(plot_data) <- c("fips_county", "period", "data")
    
    #determine scale for chloropleth map
    scale_max <- ceiling(max(plot_data$data))
    scale_min <- floor(min(plot_data$data))
    
    #create color palette
    bins <- seq(from=scale_min,to=scale_max, by=ceiling((scale_min + scale_max)/8))
    pal <- colorBin("YlOrRd", domain = plot_data$data, bins = bins)

    leaflet(ca_counties, width = "65%") %>%
      setView(-120, 37.8, 5) %>%
      addTiles() %>% 
      addPolygons(
        fillColor = ~pal(plot_data$data),
        weight = 2,
        opacity = 1,
        color = "black",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE)) %>% 
      
      #Add a legend
      addLegend(pal = pal, 
                values = ~plot_data$data, 
                opacity = 0.7, 
                title = input$stat, 
                position = "bottomleft")
    
  })
  
})
