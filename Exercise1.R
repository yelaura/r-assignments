data(mtcars)                           #Load mtcars data set
plot(mtcars$hp, mtcars$mpg,            #Plot hp as x, and mpg as y
     xlab="Horsepower",                #Define X-axis label
     ylab="Gas Mileage (mpg)",         #Define Y-axis label
     col = "blue",                     #Define color of data points
     main = "Gas Mileage vs HP",       #Define main title of plot
     sub = "Gas mileage is in MPG",    #Define sub title of plot
     pch=6)                            #We want the symbol to be triangles (pch=6)

# Observations:
# Plot shows a downward exponential trend. 
# As the horsepower increases the gas mileage decreases.