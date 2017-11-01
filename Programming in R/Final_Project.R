# Update 2

# name of the directories of the files

setwd('C:\\Users\\lye\\Downloads\\R folder\\Final Project')

files <- c("C:\\Users\\lye\\Downloads\\R folder\\Final Project\\set_a.csv")
# read all the csvs

df_a <- read.csv(files[1])
#df_a_timing <- read.csv(files[2])

# read all of the wav files
# use tuneR.readWave to interpret wav files into a vector of integers
# append the vector to a matrix

library(tuneR)

set_a <- c()

for (item in df_a$fname){
  wave <- readWave(paste('C:\\Users\\lye\\Downloads\\R folder\\Final Project\\',item, sep=""))
  set_a <- rbind(set_a,wave@left)
}

set_a <- as.data.frame(set_a)

# Update 3 : Technically Correct data

# replace empty cells with NA in df_a, df_b, df_a_timing
# set_a and set_b do not need this treatment because they are numerical vectors/matrices already

levels(df_a$label) <- c(NA, "artifact", "extrahls", "murmur", "normal")

# Coerce numeric columns into strings

df_a_timing$cycle <- as.numeric(df_a_timing$cycle)
df_a_timing$location <- as.numeric(df_a_timing$location)

# Update 4: Exploratory data analysis

# Separate out sound data from each class

artifacts <- set_a[df_a$label=="artifact",]
extrahls <- set_a[df_a$label=="extrahls",]
murmur <- set_a[df_a$label=="murmur",]
normal <- set_a[df_a$label=="normal",]
labelled <- set_a[df_a$label != "",]

# Plot raw data

matplot(t(murmur), type = "l")
dev.copy(jpeg,filename="murmur.png");
dev.off ()

matplot(t(normal), type = "l")
dev.copy(jpeg,filename="normal.png");
dev.off ()

matplot(t(extrahls), type = "l")
dev.copy(jpeg,filename="extrahs.png");
dev.off ()

matplot(t(artifacts), type = "l")
dev.copy(jpeg,filename="artifacts.png");
dev.off ()

# Plot rowMeans (sorted)

plot(sort(rowMeans(murmur*murmur)), main="Sorted rowMeans")
points(sort(rowMeans(artifacts*artifacts)), col="blue")
points(sort(rowMeans(normal)), col="green")
points(sort(rowMeans(extrahls)), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg,filename="Sorted_rowMeans.png")
dev.off()

# Using Rowmeans does not show definitive separation

plot(sort(rowMeans(murmur*murmur)), main="Sorted rowMeans of Squares")
points(sort(rowMeans(artifacts*artifacts)), col="blue")
points(sort(rowMeans(normal*normal)), col="green")
points(sort(rowMeans(extrahls*extrahls)), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="Sorted_rowMeans_Squares.png")
dev.off()

# Taking the square of the vectors and then using rowmeans does not show definitive separation

# Finding the index of the max location

plot(sort(apply(murmur, 1, which.max)), main="Index of maximum value")
points(sort(apply(artifacts, 1, which.max)), col="blue")
points(sort(apply(normal, 1, which.max)), col="green")
points(sort(apply(extrahls, 1, which.max)), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="Sorted_max_index.png")
dev.off()

# Finding the index of the min location

plot(sort(apply(murmur, 1, which.min)), main="Index of minimum value")
points(sort(apply(artifacts, 1, which.min)), col="blue")
points(sort(apply(normal, 1, which.min)), col="green")
points(sort(apply(extrahls, 1, which.min)), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="Sorted_min_index.png")
dev.off()

#Finding the value of the max

plot(sort(apply(murmur, 1, max)), main="Maximum value")
points(sort(apply(artifacts, 1, max)), col="blue")
points(sort(apply(normal, 1, max)), col="green")
points(sort(apply(extrahls, 1, max)), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="Sorted_MAX.png")
dev.off()

#Finding the value of the max^2

plot(sort(apply(murmur, 1, max)^2), main="Maximum value^2")
points(sort(apply(artifacts, 1, max)^2), col="blue")
points(sort(apply(normal, 1, max)^2), col="green")
points(sort(apply(extrahls, 1, max)^2), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="Sorted_MAX^2.jpg")
dev.off()

#Finding the value of the min

plot(sort(apply(murmur, 1, min)), main="minimum value")
points(sort(apply(artifacts, 1, min)), col="blue")
points(sort(apply(normal, 1, min)), col="green")
points(sort(apply(extrahls, 1, min)), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="Sorted_min.png")
dev.off()

#Finding the value of the mode

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

plot(sort(apply(murmur, 1, mode)), main="mode value")
points(sort(apply(artifacts, 1, mode)), col="blue")
points(sort(apply(normal, 1, mode)), col="green")
points(sort(apply(extrahls, 1, mode)), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="Sorted_mode.png")
dev.off()

#Finding the value of the median

plot(sort(apply(murmur, 1, median)), main="median value")
points(sort(apply(artifacts, 1, median)), col="blue")
points(sort(apply(normal, 1, median)), col="green")
points(sort(apply(extrahls, 1, median)), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="Sorted_median.png")
dev.off()

# Plot to see if correlations exist

par(mfcol=c(1,1))

plot(rowMeans(murmur), rowMeans(murmur*murmur), main="rowMeans(x) vs rowMeans(x^2)")
points(rowMeans(artifacts), rowMeans(artifacts*artifacts), col="blue")
points(rowMeans(normal), rowMeans(normal*normal), col="green")
points(rowMeans(extrahls), rowMeans(extrahls*extrahls), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="rowMeans vs rowMeans(x^2).jpg")
dev.off()

plot(rowMeans(murmur), apply(murmur, 1, which.min), main="rowMeans(x) vs index of min(x)")
points(rowMeans(artifacts), apply(artifacts, 1, which.min), col="blue")
points(rowMeans(normal), apply(normal, 1, which.min), col="green")
points(rowMeans(extrahls), apply(extrahls, 1, which.min), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="rowMeans vs index of min.jpg")
dev.off()

plot(rowMeans(murmur), apply(murmur, 1, which.max), main="rowMeans(x) vs index of max(x)")
points(rowMeans(artifacts), apply(artifacts, 1, which.max), col="blue")
points(rowMeans(normal), apply(normal, 1, which.max), col="green")
points(rowMeans(extrahls), apply(extrahls, 1, which.max), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="rowMeans vs index of max.jpg")
dev.off()

plot(rowMeans(murmur), apply(murmur, 1, max), main="rowMeans(x) vs max(x)")
points(rowMeans(artifacts), apply(artifacts, 1, max), col="blue")
points(rowMeans(normal), apply(normal, 1, max), col="green")
points(rowMeans(extrahls), apply(extrahls, 1, max), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="rowMeans vs max.jpg")
dev.off()

plot(rowMeans(murmur), apply(murmur, 1, min), main="rowMeans(x) vs min(x)")
points(rowMeans(artifacts), apply(artifacts, 1, min), col="blue")
points(rowMeans(normal), apply(normal, 1, min), col="green")
points(rowMeans(extrahls), apply(extrahls, 1, min), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="rowMeans vs min.jpg")
dev.off()

### Second characteristic

plot(rowMeans(murmur*murmur), apply(murmur, 1, which.min), main="rowMeans(x^2) vs index of min(x)")
points(rowMeans(artifacts*artifacts), apply(artifacts, 1, which.min), col="blue")
points(rowMeans(normal*normal), apply(normal, 1, which.min), col="green")
points(rowMeans(extrahls*extrahls), apply(extrahls, 1, which.min), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="rowMeans(x^2) vs index of min.jpg")
dev.off()

plot(rowMeans(murmur*murmur), apply(murmur, 1, which.max), main="rowMeans(x^2) vs index of max(x)")
points(rowMeans(artifacts*artifacts), apply(artifacts, 1, which.max), col="blue")
points(rowMeans(normal*normal), apply(normal, 1, which.max), col="green")
points(rowMeans(extrahls*extrahls), apply(extrahls, 1, which.max), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="rowMeans(x^2) vs index of max.jpg")
dev.off()

plot(rowMeans(murmur*murmur), apply(murmur, 1, max), main="rowMeans(x^2) vs max(x)")
points(rowMeans(artifacts*artifacts), apply(artifacts, 1, max), col="blue")
points(rowMeans(normal*normal), apply(normal, 1, max), col="green")
points(rowMeans(extrahls*extrahls), apply(extrahls, 1, max), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="rowMeans(x^2) vs max.jpg")
dev.off()

plot(rowMeans(murmur*murmur), apply(murmur, 1, min), main="rowMeans(x^2) vs min(x)")
points(rowMeans(artifacts*artifacts), apply(artifacts, 1, min), col="blue")
points(rowMeans(normal*normal), apply(normal, 1, min), col="green")
points(rowMeans(extrahls*extrahls), apply(extrahls, 1, min), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="rowMeans(x^2) vs min.jpg")
dev.off()

### Third characteristic

plot(apply(murmur, 1, which.min), apply(murmur, 1, which.max), main="index of min(x) vs index of max(x)")
points(apply(artifacts,1, which.min), apply(artifacts, 1, which.max), col="blue")
points(apply(normal,1, which.min), apply(normal, 1, which.max), col="green")
points(apply(extrahls,1, which.min), apply(extrahls, 1, which.max), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="index of min(x) vs index of max.jpg")
dev.off()

plot(apply(murmur, 1, which.min), apply(murmur, 1, max), main="index of min(x) vs max(x)")
points(apply(artifacts,1, which.min), apply(artifacts, 1, max), col="blue")
points(apply(normal,1, which.min), apply(normal, 1, max), col="green")
points(apply(extrahls,1, which.min), apply(extrahls, 1, max), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="index of min(x) vs max.jpg")
dev.off()

plot(apply(murmur, 1, which.min), apply(murmur, 1, min), main="index of min(x) vs min(x)")
points(apply(artifacts,1, which.min), apply(artifacts, 1, min), col="blue")
points(apply(normal,1, which.min), apply(normal, 1, min), col="green")
points(apply(extrahls,1, which.min), apply(extrahls, 1, min), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="index of min(x) vs min.jpg")
dev.off()

### Fourth characteristic

plot(apply(murmur, 1, which.max), apply(murmur, 1, max), main="index of max(x) vs max(x)")
points(apply(artifacts,1, which.max), apply(artifacts, 1, max), col="blue")
points(apply(normal,1, which.max), apply(normal, 1, max), col="green")
points(apply(extrahls,1, which.max), apply(extrahls, 1, max), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="index of max(x) vs max.jpg")
dev.off()

plot(apply(murmur, 1, which.max), apply(murmur, 1, min), main="index of max(x) vs min(x)")
points(apply(artifacts,1, which.max), apply(artifacts, 1, min), col="blue")
points(apply(normal,1, which.max), apply(normal, 1, min), col="green")
points(apply(extrahls,1, which.max), apply(extrahls, 1, min), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="index of max(x) vs min.jpg")
dev.off()

### Fifth characteristic

plot(apply(murmur, 1, max), apply(murmur, 1, min), main="max(x) vs min(x)")
points(apply(artifacts,1, max), apply(artifacts, 1, min), col="blue")
points(apply(normal,1, max), apply(normal, 1, min), col="green")
points(apply(extrahls,1, max), apply(extrahls, 1, min), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="max(x) vs min.jpg")
dev.off()

# Update 6: Reduce dimensionality

PCA <- prcomp(set_a)

plot(PCA, type="l")
dev.copy(jpeg, filename="vars_PCA.jpg")
dev.off()

summary(PCA)

plot(predict(PCA, set_a), xlim=c(-500000, 0), ylim=c(-1e6, 1e6) )
dev.copy(jpeg, filename="pca.jpg")
dev.off()

plot(predict(PCA, murmur), main="PCA plot (color-coded)")
points(predict(PCA, artifacts), col="blue")
points(predict(PCA, normal), col="green")
points(predict(PCA, extrahls), col="red")
legend(x="topleft",
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg, filename="PCA_colorcoded.jpg")
dev.off()

library(scatterplot3d)
s3d <- scatterplot3d(predict(PCA, murmur), main="PCA plot 3d (color-coded)", highlight.3d = TRUE)
s3d$points3d(predict(PCA,artifacts), pch=2)
s3d$points3d(predict(PCA,normal), pch=3)
s3d$points3d(predict(PCA,extrahls), pch=4)

legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'black', 'black', 'black'), 
       pch=c(1,2,3,4))

dev.copy(jpeg, filename="PCA_3d_colorcoded.jpg")
dev.off()

# Update 6: Probability distribution / hypothesis testing

# Checking if characteristics of each data set are normally distributed using Shapiro-Wilk test
# Will check for each category and also all labeled points

# Check

toTest <- list(labelled, normal, murmur, extrahls, artifacts)

print("p-value for rowMeans(x) are:")
for (x in toTest){
  print(shapiro.test(rowMeans(x)))
  hist(rowMeans(x))
  Sys.sleep(5)
}
# p-value for all rowMeans(x) are <0.05 which means data is normally distributed
# hist graph for all the normally distributed data


print("p-value for rowMeans(x*x) are:")
for (x in toTest){
  print(shapiro.test(rowMeans(x*x))$p.value)
  hist(rowMeans(x*x))
  Sys.sleep(5)
}
# Once again, p-value for all rowMeans(x*x) are <0.05 which means data is normally distributed

print("p-value for apply(x,1,which.min) are:")
for (x in toTest){
  print(shapiro.test(apply(x,1,which.min))$p.value)
  hist(apply(x,1,which.min))
  Sys.sleep(5)
}
# normal and murmur datasets do not have index of min normally distributed, but the others do (labelled, extrahls, artifacts)

print("p-value for apply(x,1,which.max) are:")
for (x in toTest){
  print(shapiro.test(apply(x,1,which.max))$p.value)
  hist(apply(x,1,which.max))
  Sys.sleep(5)
}
# normal, murmur & extrahls datasets do not have index of max normally distributed but the others do

print("p-value for apply(x,1,min) are:")
for (x in toTest){
  print(shapiro.test(apply(x,1,min))$p.value)
  hist(apply(x,1,min))
  Sys.sleep(5)
}
# all p-values are <0.05 therefore all the min values are normally distributed

print("p-value for apply(x,1,max) are:")
for (x in toTest){
  print(shapiro.test(apply(x,1,max))$p.value)
  hist(apply(x,1,max))
  Sys.sleep(5)
}
# all p-values are <0.05 therefore all the max values are normally distributed

# More hypothesis testing

# Check if characteristics are dependent using chi-squared tests

# Check characteristics for labelled dataset

chisq.test(rowMeans(labelled), rowMeans(labelled*labelled), correct=FALSE)
chisq.test(rowMeans(labelled), apply(labelled, 1, which.min), correct=FALSE)
chisq.test(rowMeans(labelled), apply(labelled, 1, which.max), correct=FALSE)
chisq.test(rowMeans(labelled), apply(labelled, 1, min), correct=FALSE)
chisq.test(rowMeans(labelled), apply(labelled, 1, max), correct=FALSE)

chisq.test(rowMeans(labelled*labelled), apply(labelled, 1, which.min), correct=FALSE)
chisq.test(rowMeans(labelled*labelled), apply(labelled, 1, which.max), correct=FALSE)
chisq.test(rowMeans(labelled*labelled), apply(labelled, 1, min), correct=FALSE)
chisq.test(rowMeans(labelled*labelled), apply(labelled, 1, max), correct=FALSE)

chisq.test(apply(labelled, 1, which.min), apply(labelled, 1, which.max), correct=FALSE) #p-value = 0.08
chisq.test(apply(labelled, 1, which.min), apply(labelled, 1, min), correct=FALSE)
chisq.test(apply(labelled, 1, which.min), apply(labelled, 1, max), correct=FALSE)

chisq.test(apply(labelled, 1, which.max), apply(labelled, 1, min), correct=FALSE)
chisq.test(apply(labelled, 1, which.max), apply(labelled, 1, max), correct=FALSE)

chisq.test(apply(labelled, 1, min), apply(labelled, 1, max), correct=FALSE) #p-value = 0.08

# Check characteristics for normal dataset

chisq.test(rowMeans(normal), rowMeans(normal*normal), correct=FALSE)
chisq.test(rowMeans(normal), apply(normal, 1, which.min), correct=FALSE)
chisq.test(rowMeans(normal), apply(normal, 1, which.max), correct=FALSE)
chisq.test(rowMeans(normal), apply(normal, 1, min), correct=FALSE)
chisq.test(rowMeans(normal), apply(normal, 1, max), correct=FALSE)

chisq.test(rowMeans(normal*normal), apply(normal, 1, which.min), correct=FALSE)
chisq.test(rowMeans(normal*normal), apply(normal, 1, which.max), correct=FALSE)
chisq.test(rowMeans(normal*normal), apply(normal, 1, min), correct=FALSE)
chisq.test(rowMeans(normal*normal), apply(normal, 1, max), correct=FALSE)

chisq.test(apply(normal, 1, which.min), apply(normal, 1, which.max), correct=FALSE) 
chisq.test(apply(normal, 1, which.min), apply(normal, 1, min), correct=FALSE)
chisq.test(apply(normal, 1, which.min), apply(normal, 1, max), correct=FALSE)

chisq.test(apply(normal, 1, which.max), apply(normal, 1, min), correct=FALSE)
chisq.test(apply(normal, 1, which.max), apply(normal, 1, max), correct=FALSE)

chisq.test(apply(normal, 1, min), apply(normal, 1, max), correct=FALSE)
#X-squared = 930, df = 900, p-value = 0.2373 consistently

# Check characteristics for murmur dataset

chisq.test(rowMeans(murmur), rowMeans(murmur*murmur), correct=FALSE)
chisq.test(rowMeans(murmur), apply(murmur, 1, which.min), correct=FALSE)
chisq.test(rowMeans(murmur), apply(murmur, 1, which.max), correct=FALSE)
chisq.test(rowMeans(murmur), apply(murmur, 1, min), correct=FALSE)
chisq.test(rowMeans(murmur), apply(murmur, 1, max), correct=FALSE)

chisq.test(rowMeans(murmur*murmur), apply(murmur, 1, which.min), correct=FALSE)
chisq.test(rowMeans(murmur*murmur), apply(murmur, 1, which.max), correct=FALSE)
chisq.test(rowMeans(murmur*murmur), apply(murmur, 1, min), correct=FALSE)
chisq.test(rowMeans(murmur*murmur), apply(murmur, 1, max), correct=FALSE)

chisq.test(apply(murmur, 1, which.min), apply(murmur, 1, which.max), correct=FALSE) 
chisq.test(apply(murmur, 1, which.min), apply(murmur, 1, min), correct=FALSE)
chisq.test(apply(murmur, 1, which.min), apply(murmur, 1, max), correct=FALSE)

chisq.test(apply(murmur, 1, which.max), apply(murmur, 1, min), correct=FALSE)
chisq.test(apply(murmur, 1, which.max), apply(murmur, 1, max), correct=FALSE)

chisq.test(apply(murmur, 1, min), apply(murmur, 1, max), correct=FALSE)
# X-squared = 1122, df = 1089, p-value = 0.2375 consistently

# Check characteristics for extrahls dataset

chisq.test(rowMeans(extrahls), rowMeans(extrahls*extrahls), correct=FALSE)
chisq.test(rowMeans(extrahls), apply(extrahls, 1, which.min), correct=FALSE)
chisq.test(rowMeans(extrahls), apply(extrahls, 1, which.max), correct=FALSE)
chisq.test(rowMeans(extrahls), apply(extrahls, 1, min), correct=FALSE)
chisq.test(rowMeans(extrahls), apply(extrahls, 1, max), correct=FALSE)

chisq.test(rowMeans(extrahls*extrahls), apply(extrahls, 1, which.min), correct=FALSE)
chisq.test(rowMeans(extrahls*extrahls), apply(extrahls, 1, which.max), correct=FALSE)
chisq.test(rowMeans(extrahls*extrahls), apply(extrahls, 1, min), correct=FALSE)
chisq.test(rowMeans(extrahls*extrahls), apply(extrahls, 1, max), correct=FALSE)

chisq.test(apply(extrahls, 1, which.min), apply(extrahls, 1, which.max), correct=FALSE) 
chisq.test(apply(extrahls, 1, which.min), apply(extrahls, 1, min), correct=FALSE)
chisq.test(apply(extrahls, 1, which.min), apply(extrahls, 1, max), correct=FALSE)

chisq.test(apply(extrahls, 1, which.max), apply(extrahls, 1, min), correct=FALSE)
chisq.test(apply(extrahls, 1, which.max), apply(extrahls, 1, max), correct=FALSE)

chisq.test(apply(extrahls, 1, min), apply(extrahls, 1, max), correct=FALSE)
# X-squared = 342, df = 3224, p-value = 0.2356 consistently

# Check characteristics for artifacts dataset

chisq.test(rowMeans(artifacts), rowMeans(artifacts*artifacts), correct=FALSE)
chisq.test(rowMeans(artifacts), apply(artifacts, 1, which.min), correct=FALSE)
chisq.test(rowMeans(artifacts), apply(artifacts, 1, which.max), correct=FALSE)
chisq.test(rowMeans(artifacts), apply(artifacts, 1, min), correct=FALSE)
chisq.test(rowMeans(artifacts), apply(artifacts, 1, max), correct=FALSE)

chisq.test(rowMeans(artifacts*artifacts), apply(artifacts, 1, which.min), correct=FALSE)
chisq.test(rowMeans(artifacts*artifacts), apply(artifacts, 1, which.max), correct=FALSE)
chisq.test(rowMeans(artifacts*artifacts), apply(artifacts, 1, min), correct=FALSE)
chisq.test(rowMeans(artifacts*artifacts), apply(artifacts, 1, max), correct=FALSE)

chisq.test(apply(artifacts, 1, which.min), apply(artifacts, 1, which.max), correct=FALSE) 
chisq.test(apply(artifacts, 1, which.min), apply(artifacts, 1, min), correct=FALSE)
chisq.test(apply(artifacts, 1, which.min), apply(artifacts, 1, max), correct=FALSE)

chisq.test(apply(artifacts, 1, which.max), apply(artifacts, 1, min), correct=FALSE)
chisq.test(apply(artifacts, 1, which.max), apply(artifacts, 1, max), correct=FALSE)

chisq.test(apply(artifacts, 1, min), apply(artifacts, 1, max), correct=FALSE) # p-value = 0.08
# X-squared = 1520, df = 1444, p-value = 0.24 for the most part 

# Update 7: Reflection

library(caret)  # The main package 
library(e1071)  # A helper package caret depends on
library(randomForest)  # A helper package caret depends on

# To begin, we want to split our data into two subsets: one for 
# training, and one for testing. Since this is a common step
# in supervised learning, the caret package provides a function
# to do so. 

# Add label column to labelled dataframe

with_labels <- as.data.frame(labelled)
with_labels[, 1:396900] <- sapply(with_labels[,1:396900], as.numeric)
with_labels$label <- as.factor(df_a$label[1:124])

training_id <- createDataPartition(with_labels$label, p=0.90, list=FALSE)
# This says return back 90% of the element IDs.
# You could also acheive this with sample.int

# Since that returns element ids, we can use that to subset our data set
training <- with_labels[training_id,]   
# use the remaining 10% of the data for testing later on
testing <- with_labels[-training_id,]    #removes training_id

training <- droplevels(training)
testing <- droplevels(testing)
# caret provides a straightforward way to apply these methods to your
# training set with the train function. It accepts the formula notation,
# and you can use the format like:
# dependent ~ . 
# to mean you want all of the other columns to be independent variables
# for the learning. Then you give the data.frame you are taking these
# columns from, and then specify the method name - matching that long list
# we saw above. 

# LDA
lda_fit <- train(label~., data=training, method="lda")

# CART
cart_fit <- train(label~., data=training, method="rpart")

# RF
rf_fit <- train(label~., data=training, method="rf")

# kNN
knn_fit <- train(label~., data=training, method="knn")

# SVM
svm_fit <- train(label~., data=training, method="svmRadial")

#kmeans


# This is doing some internal cross-validation to see how well
# each method did. This is a relative test, i.e. for comparing
# the performance between alternative models. 
results <- resamples(list(lda=lda_fit, 
                          cart=cart_fit, 
                          rf=rf_fit,
                          knn=knn_fit, 
                          svm=svm_fit))

# We can check out the comparison numerically or visually:
summary(results)
dotplot(results)

# In this example, LDA looks the best. Let's go ahead and predict
# the species of the testing data set
pred <- predict(lda_fit, testing)
# We can use a confusion matrix to see how well the prediction did:
confusionMatrix(pred, testing$Species)
# Accuracy was 100%! (or will be nearly, there is slight differences 
# when randomly samples) 

##################### START TS MODEL #############################################

# Step 1: apply ts to all rows
ts_model <- t(apply(labelled,1,ts))

# Step 2: PCA decomposition to n dimensions
#conduct PCA
PCA_ts <- prcomp(ts_model)
#select number of components
freq <- data.frame()
for (n in c(2,3,4,5,10,15,20,25,50,100)){
  new_ts <- PCA_ts$x[,1:n]
  cluster <- kmeans(new_ts,4)
  freq <- rbind(freq, as.data.frame(table(cluster$cluster))$Freq)
}
names(freq) = 1:4
freq$PC <- c(2,3,4,5,10,15,20,25,50,100)
freq <- subset(freq, select=c(5,1:4))


### for evaluating accuracy of random Forest ###
acc <- data.frame()
for (n in seq(from=2,to=100, by=5)){
  new_ts <- PCA_ts$x[,1:n]
  with_labels <- as.data.frame(new_ts)
  with_labels$label <- as.factor(df_a$label[1:124])
  
  # Split samples: test/training
  training_id <- createDataPartition(with_labels$label, p=0.90, list=FALSE)
  
  # Since that returns element ids, we can use that to subset our data set
  training <- with_labels[training_id,]   
  # use the remaining 10% of the data for testing later on
  testing <- with_labels[-training_id,]    #removes training_id
  
  training <- droplevels(training)
  testing <- droplevels(testing)
  
  rf_fit <- train(label~., data=training, method="rf")
  
  pred <- predict(rf_fit, testing)
  CM <- confusionMatrix(pred, testing$label)
  
  acc <- rbind (acc, CM$overall[1])
}

plot(c(2:68), acc$X0.454545454545455,
     type="l",
     ylab="Accuracy", 
     xlab="No. of Principal Components",
     main="Accuracy vs PCs for Random Forest")

n <- 100
new_ts <- PCA_ts$x[,1:n]

#kmeans
cluster <- kmeans(new_ts,4)

table(cluster$cluster)

# Step 3: start ML

with_labels <- as.data.frame(new_ts)
with_labels$label <- as.factor(df_a$label[1:124])

# Split samples: test/training
training_id <- createDataPartition(with_labels$label, p=0.90, list=FALSE)

# Since that returns element ids, we can use that to subset our data set
training <- with_labels[training_id,]   
# use the remaining 10% of the data for testing later on
testing <- with_labels[-training_id,]    #removes training_id

training <- droplevels(training)
testing <- droplevels(testing)
# caret provides a straightforward way to apply these methods to your
# training set with the train function. It accepts the formula notation,
# and you can use the format like:
# dependent ~ . 
# to mean you want all of the other columns to be independent variables
# for the learning. Then you give the data.frame you are taking these
# columns from, and then specify the method name - matching that long list
# we saw above. 

# LDA
lda_fit <- train(label~., data=training, method="lda")

# CART
cart_fit <- train(label~., data=training, method="rpart")

# RF
rf_fit <- train(label~., data=training, method="rf")

# kNN
knn_fit <- train(label~., data=training, method="knn")

# SVM
svm_fit <- train(label~., data=training, method="svmRadial")

# bayes - something's wrong
bn_fit <- train(label~., data=training, method="naive_bayes")

#neural network
nn_fit <- train(label~., data=training, method="nnet")

# This is doing some internal cross-validation to see how well
# each method did. This is a relative test, i.e. for comparing
# the performance between alternative models. 
results <- resamples(list(lda=lda_fit, 
                          cart=cart_fit, 
                          rf=rf_fit,
                          knn=knn_fit, 
                          svm=svm_fit,
                          bn=bn_fit,
                          nn=nn_fit))

# We can check out the comparison numerically or visually:
dotplot(results, main=paste("PC=", n, sep=""))
