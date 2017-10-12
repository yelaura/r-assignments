# Write an .R script that does the following:
#   
#   1. Defines a function that:
#   
#   creates a plot that shows a density distribution over a normalized histogram, 
# takes two arguments: a data vector and a string specifying the plotting style,
# has a conditional statement that executes different plotting functions depending on the plotting style,
# supports the base R, ggplot, and qplot plotting styles,
# defaults to using the base R plotting style,
# and returns no output.

dens_histo <- function(rawdata, string="R"){
  data <- density(rawdata)
  
  if (string == "R"){
    hist(diamonds$price, freq=FALSE)
    lines(data$x, data$y, col="red")
  } else if (string == "ggplot"){
    g <- ggplot(diamonds, aes(price, ..density..))
    g + geom_histogram(bins=30, aes(y=..density..)) + geom_density()
  } else if (string == "qplot"){
    qplot(rawdata, y=..density.., geom=c('histogram','density'))
  } else{
    print ("Could not find plotting style")
  }
}

# 2. Tests that function on the price column of the built-in data set called 'diamonds'. 
# Confirm that the result from using the default style and the base style are the same and that the result 
# from the ggplot and qplot style are the same. 

library(ggplot2)

dens_histo(diamonds$price)
dens_histo(diamonds$price, "R")

dens_histo(diamonds$price, "qplot")
dens_histo(diamonds$price, "ggplot")

# Example: Say you create the function histDens. Then histDens(x, "qplot") should create the histogram+density 
# plot of x using the qplot function. And histDens(x, "ggplot") should create the histogram+density plot of x 
# using the ggplot function. And histDens(x, "base") and histDens(x) should create the histogram+density plot 
# of x using the hist, density, and plot functions. 