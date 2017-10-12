test_fun <- function(x, y, tol=0.5, alpha=0.05, 
                     x_pred = data.frame(x=seq(min(x), max(x), length.out = 100))){
# data(mtcars)
# x <- mtcars$hp
# y <- mtcars$mpg
# tol <- 0.5
# alpha <- 0.05
# x_pred <- data.frame(x=seq(min(x), max(x), length.out = 100))

  res <- chisq.test(x,y)
  
  if (res$statistic < tol){
    print("Correlation is too low.")
    return (NULL)
  }
  
  fit<-lm(y ~ x)
  
  shap_test <- shapiro.test(fit$residuals)
  
  if (shap_test$p.value < alpha){
    print("Assumption that residuals of the fit is normally distributed is not met. Returning result of lm function.")
    return (fit)
  }
  
  t_test <- t.test(fit$residuals)
  
  if (t_test$p.value < alpha) {
    print ("Assumption that mean of the residuals does not equal 0. Returning result of lm function.")
    return (fit)
  }
  
  predI <- predict(fit, x_pred, interval="prediction")
  
  predI <- as.data.frame(predI)
  predI$x <- x_pred$x
  
  names(predI) <- c("x", "fit", "lwr", "upr")
  
  return (predI)
  
}

data("faithful")
data("mtcars")

test_fun(faithful$waiting, faithful$eruptions)
test_fun(mtcars$hp, mtcars$mpg)
