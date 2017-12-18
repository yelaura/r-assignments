library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(rgdal)
library(spdplyr)

cases_311 <- read_csv("311_Cases.csv")
incidents <- read_csv("Police_Department_Incidents.csv")
ped_traffic <- read_csv("Estimated_Yearly_Pedestrian_Volume_at_Intersections.csv")

source("fix_column_names.R")
cases_311 <- fix_column_names(cases_311)
incidents <- fix_column_names(incidents)
ped_traffic <- fix_column_names(ped_traffic)

keywords <- paste("sidewalk", "curb", "street", "construction", "pedestrian", "sign", "encampment", sep="|")

cases_311_clean <- cases_311 %>%
  select(case_id, opened, category, request_type, request_details, police_district, latitude, longitude) %>%
  rename(id=case_id, date=opened, descript=request_details) %>%
  filter(grepl(keywords, category, ignore.case=TRUE) | 
           grepl(keywords, request_type, ignore.case=TRUE) | 
           grepl(keywords, descript, ignore.case=TRUE)) %>%
  mutate(date=mdy(substring(date, first = 1, last = 10)))

incidents_clean <- incidents %>%
  select(incidnt_num, date, category, descript, pd_district, x, y) %>%
  rename(id=incidnt_num, police_district=pd_district, longitude=x, latitude=y) %>%
  filter(grepl(keywords, category, ignore.case=TRUE) | 
           grepl(keywords, descript, ignore.case=TRUE)) %>%
  mutate(date = mdy(date))

events <- full_join(incidents_clean, cases_311_clean) %>%
  mutate(police_district = as.factor(police_district),
         category = as.factor(category))

ped_traffic_clean <- ped_traffic %>%
  select(annual_pedestrian_volume, geom) %>%
  extract(geom,
          into=c("latitude", "longitude"),
          "(-?[0-9]*.[0-9]*),{1} {1}(-?[0-9]*.[0-9]*)",
          remove=TRUE) %>%
  mutate(longitude=as.numeric(longitude), latitude=as.numeric(latitude)) %>%
  sample_frac(0.8)

pd <- readOGR(dsn = "./Historical Police Districts.geojson",
              layer = "Historical Police Districts", verbose = FALSE)

pd@data <- pd@data %>% fix_column_names

shinyServer(function(input, output) {
   
  output$showThePlot <- renderLeaflet({
    #filter out events by category and date
    df <- events %>% 
      filter(
        category == input$catSelector,
        date >= ymd(input$dateRange[1]) & date <= ymd(input$dateRange[2])
      ) %>%
      group_by(police_district) %>% 
      summarise(total = n()) %>% 
      rename(district=police_district)

      pd@data <- left_join(pd@data, df)
      
      pal <- colorNumeric(
        palette = "Greens",
        domain=pd@data$total
      )
      
      pal_ped <- colorNumeric(
        palette = "Reds",
        domain=ped_traffic_clean$annual_pedestrian_volume
      )
      
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -122.4194, lat = 37.7749, zoom = 12) %>%
        addCircles(data=ped_traffic_clean,
                   lng = ~longitude, lat = ~latitude,
                   popup = paste(ped_traffic_clean$annual_pedestrian_volume, "Pedestrians/Year", sep=" "),
                   color=~pal_ped(ped_traffic_clean$annual_pedestrian_volume)) %>%
        addLegend(position = "topright", pal = pal_ped, values = ped_traffic_clean$annual_pedestrian_volume,
                  title = "Annual Pedestrian Volume", opacity=0.5) %>%
        
        addPolygons(
          data=pd,
          fillColor = pal(pd@data$total),
          weight = 1,
          opacity = 0.5,
          color = "black",
          dashArray = "3",
          fillOpacity = 0.5,
          popup = paste(pd@data$district, " : ", pd@data$total, "Events", sep=" "),
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.5,
            bringToFront = FALSE)) %>% 
        addLegend(pal = pal, 
                  values = pd@data$total, 
                  opacity = 0.7, 
                  title = "Total Events", 
                  position = "bottomleft")
    })
  
  output$catSelector <- renderUI({
    selectInput("catSelector", 
                "Choose Category of Events:", 
                as.list(levels(events$category)),
                selected = "BURGLARY") 
  })
  
})
