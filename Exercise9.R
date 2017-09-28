filename = "C:\\Users\\lye\\Downloads\\state-CA.xml"

require(XML)
options(scipen=999)
drought_df <- xmlToDataFrame(filename)
droughtList <- xmlToList(filename)

df <- data.frame(Date=numeric(), 
                 Drought_Status=factor(levels=c(1,2,3,4,5,6), labels=c("Nothing","D0", "D1", "D2", "D3", "D4")), 
                 Percent_Area=numeric())

for (item in droughtList){
  d <- item$.attrs[[2]]
  D4 <- as.numeric(item$D4)
  D3 <- as.numeric(item$D3) - D4
  D2 <- as.numeric(item$D2) - D3 - D4
  D1 <- as.numeric(item$D1) - D2 - D3 - D4
  D0 <- as.numeric(item$D0) - D1 - D2 - D3 - D4
  Nothing <- as.numeric(item$Nothing)
  
  df[nrow(df)+1,] <- list(d, "D0", D0)
  df[nrow(df)+1,] <- list(d, "D1", D1)
  df[nrow(df)+1,] <- list(d, "D2", D2)
  df[nrow(df)+1,] <- list(d, "D3", D3)
  df[nrow(df)+1,] <- list(d, "D4", D4)
  df[nrow(df)+1,] <- list(d, "Nothing", Nothing)
}

df[,3] <- round(df[,3], 2)
df$Date <- as.Date(df$Date, "%m/%d/%Y")

# Getting ready to plot

require(ggplot2)
require(scales)
require(plyr)

df$Drought_Status <- ordered( df$Drought_Status, levels = c('Nothing', 'D0', 'D1', 'D2', 'D3', 'D4'))
df <- ddply(df, .(Drought_Status), transform, DomAreaByCat = order(Date))

ggplot(df, aes(Date, Percent_Area)) +
  labs(title = "Percentage of California in Drought WY2013-Present\n", x = "Time\n", y = "Percent Area\n", legend = "Drought Status\n") +
  geom_area(aes(fill= Drought_Status), position = 'stack', stat = "identity") + 
  scale_fill_manual(values=c("lightgoldenrod1", "gold", "goldenrod2", "orange", "red", "darkred"),
                    labels = c("No Drought", "Abnormally Dry", "Moderate Drought", "Severe Drought", "Extreme Drought", "Exceptional Drought"))