# # One of the assumptions of calculating confidence intervals in linear regression (as we did in last week's exercise) 
# is that the residuals (the difference between observed and predicted y-values) are 
# normally distributed with zero mean.

# For this exercise, we will confirm that this assumption is valid for the regression we performed last week:
# 1. Fit a linear model of eruptions ~ waiting (as we did last week). Store the result from lm. 
# That result will have the residuals calculated for you under yourLmFit$residuals. 

data(faithful)
fit<-lm(eruptions ~ waiting, faithful)

# 2. Plot a histogram and QQ-plot for these residuals. Do they appear normally distributed? 
# Answer: From the histogram and QQ-plot, they look almost normally distributed. The QQ-plot is almost a straight line.
hist(fit$residuals)

qqnorm(fit$residuals)
qqline(fit$residuals)

# 3. Calculate the sample mean. Is it (effectively) zero? 
# Answer: Yes, the calculated sample mean is effectively zero.

sum(fit$residuals)/length(fit$residuals)

# 4. Perform a Shapiro-Wilk test for normality. What is the conclusion?
# Answer: p-value is >0.05 so we fail to reject the null hypothesis which means that we can conclude that the mean is 0.

shapiro.test(fit$residuals)

# 5. Perform a t-test with the null hypothesis being that the true mean is zero. What is the conclusion?
# Answer: p-value is >0.05 so we fail to reject the null hypothesis which means that we can conclude that the mean is 0.

t.test(fit$residuals)

# 6. Does our regression from last week meet the distributional assumptions for calculating confidence intervals?
# Answer: Since the residuals of the lineaer regression we did last week are normally distributed with zero mean (as seen from above),
# the regression from last week meets the distributional assumptions for confidence interval calculations.