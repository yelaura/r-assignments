# We saw in lecture that LDA could predict the Iris species with near 100% accuracy 
# when using 80% of the data set as training data. For this exercise, your job is to 
# look at how the accuracy changes for LDA for lower and lower percentage of training data:

library(caret)  
library(e1071)  
library(randomForest)  

data(iris)  

get_acc <- function(pct, n){
  # function that returns a vector of n accuracies given the pct partition of data for an lda fit
  acc = c()
  for (i in 1:n){
    training_id <- createDataPartition(iris$Species, p=pct, list=FALSE)
    training <- iris[training_id,]   
    testing <- iris[-training_id,]
    
    lda_fit <- train(Species~., data=training, method="lda")
  
  # Repeat the training and accuracy recording 50 times for each percentage 
  # (hint: get the workflow down with like 3 repeats first, then increase to 50 when satisfied 
  # since it will take several minutes to calculate).
  
    pred <- predict(lda_fit, testing)
    cM <- confusionMatrix(pred, testing$Species) 
    # For each training, record the accuracy (hint: you can extract it from the Confusion Matrix)
    acc <- append(acc, cM$overall[1])
  }
  
  return (acc)
}

# Try 30%, 50%, 70%, and 90% of the iris data set for training

acc_3 <- get_acc(0.3, 50)
acc_5 <- get_acc(0.5, 50)
acc_7 <- get_acc(0.7, 50)
acc_9 <- get_acc(0.9, 50)

# Construct a plot of boxplots that show the distribution of accuracy for each of the training percentages.

boxplot(acc_3, acc_5, acc_7, acc_9, names=c('30%', '50%', '70%', '90%'))