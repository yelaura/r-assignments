data("mtcars")

nums <- seq(1,10)

res <- c()

for (n in nums){
  ans <- kmeans(cbind(mtcars$hp, mtcars$mpg), n)
  res <- append(res, ans$tot.withinss)
}

res <- res/max(res)
res_diff <- diff(res)

threshold <- 0.05
under_threshold <- abs(res_diff) < 0.05

num_clusters <- min(which(under_threshold))

print(paste("The ideal number of clusters to use is", num_clusters))