# Use the file wells.tsv
# 
# Generate a leaflet map showing the locations of each well as a circle marker
# 
# Extra: have the fill color and radius reflect the elevation of the well

library(tidyverse)
d <- read_tsv("C:\\Users\\lye\\Downloads\\wells.tsv")

library(leaflet)
m <- leaflet() %>%
  addCircleMarkers(data=d, fillColor=(d$GS_ELEVATION), weight=d$GS_ELEVATION) %>%
  addTiles()

m

# long format
# Yes

data_wide <- spread(df1, var, val)