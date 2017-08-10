# 1. Downloads the data from this link and saves it as a data.frame

library("RCurl")
rawEQ <- getURL("https://raw.githubusercontent.com/fivethirtyeight/data/master/forecast-methodology/historical-senate-predictions.csv")
dfEQ <- read.csv(text = rawEQ)

# 2. Replaces the redundant 'Lose' level for the result column with 'Loss'

lvls <- factor(dfEQ$result)
levels(lvls) <- c("Loss", "Loss", "Win")

dfEQ$result <- lvls

# 3. Subsets the data to only the year 2012

dfEQ_2012 <- subset(dfEQ, year==2012)
 
# 4. Creates a box plot of forecast_prob per result
# 5. Saves the box plot as a .png file

win_2012 <- subset(dfEQ_2012, result=="Win")
lose_2012 <- subset(dfEQ_2012, result == "Loss")

win <- subset(dfEQ, result=="Win")
lose <- subset(dfEQ, result == "Loss")

png("boxplot_all.png")
boxplot(win$forecast_prob, lose$forecast_prob, names=c("Win", "Loss"), ylab="Forecast Probability")
dev.off()

png("boxplot_2012.png")
boxplot(win_2012$forecast_prob, lose_2012$forecast_prob, names=c("Win", "Loss"), ylab="Forecast Probability")
dev.off()

# 6. Saves the modified data.frame to a .rData file

save(dfEQ, file="dfEQ.rData")
save(dfEQ_2012, file="dfEQ_2012.rData")

# Checking that the png can be loaded

library(imager)
im <- load.image("boxplot.png")
plot(im)