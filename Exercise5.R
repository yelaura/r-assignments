# Using the builtin faithful dataset 
# (describes the waiting time until certain eruption time lengths for the Old Faithful geyser), make a plot that

data(faithful)

plot(faithful$waiting, faithful$eruptions, 
     main="Waiting time until Old Faithful eruptions",
     # has x-axis limits from 30 to 110 minutes,
     xlim=c(30,110),
     # has y-axis limits that match the minimum and maximum values of the intervals,
     ylim=c(min(predI[,"lwr"]),max(predI[,"upr"]))
)

# shows the regression line with waiting time as the independent variable,

fit<-lm(eruptions ~ waiting, faithful)
abline(fit, col="red")

# shows the 95% confidence interval lines,

new <- data.frame(waiting=seq(30,110))
conI <- predict(fit, new, interval="confidence")
lines(new$waitin,conI[,"lwr"],lty=3, col="purple")
lines(new$waiting,conI[,"upr"],lty=3, col="purple")

# shows the 95% prediction interval lines,

predI <- predict(fit, new, interval="prediction")
lines(new$waitin,predI[,"lwr"],lty=3, col="blue")
lines(new$waiting,predI[,"upr"],lty=3, col="blue")

# prints text on the plot (check out the text function) to show the linear model equation, and
text(50, 6, paste(as.character(coef(fit[1]))[1], " + ", as.character(coef(fit[1]))[2], "x = y"))

# has a legend (see legend function) denoting what each line represents. 
legend(80,2, legend = c("Data","Fit", "Confidence Interval (95%)", "Prediction Intervals (95%)"),
       col=c("black","red","purple", "blue"),
       pch=1)

# Please upload your .R script for credit. 

dev.copy(jpeg, filename="Faithful.jpg")
dev.off()
