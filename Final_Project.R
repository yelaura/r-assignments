# Update 2

# name of the directories of the files

setwd('C:\\Users\\lye\\OneDrive\\Documents\\GitHub\\r-assignments')

files <- c('.\\Final Project\\set_a.csv',
           '.\\Final Project\\set_a_timing.csv')

# read all the csvs

df_a <- read.csv(files[1])
df_a_timing <- read.csv(files[2])

# read all of the wav files
# use tuneR.readWave to interpret wav files into a vector of integers
# append the vector to a matrix

library(tuneR)

set_a <- c()

for (item in df_a$fname){
  wave <- readWave(paste('.\\Final Project\\',item, sep=""))
  set_a <- rbind(set_a,wave@left)
}

# Update 3 : Technically Correct data

# replace empty cells with NA in df_a, df_b, df_a_timing
# set_a and set_b do not need this treatment because they are numerical vectors/matrices already

levels(df_a$label) <- c(NA, "artifact", "extrahls", "murmur", "normal")

# Coerce numeric columns into strings

df_a_timing$cycle <- as.numeric(df_a_timing$cycle)
df_a_timing$location <- as.numeric(df_a_timing$location)

# Update 4: Exploratory data analysis

# Separate out sound data from each class

all_a <- df_a$label[1:124] == "artifact"
num_a <- sum(all_a)
artifacts <- set_a[all_a]
artifacts <- matrix(artifacts, nrow=num_a)

all_e <- df_a$label[1:124] == "extrahls"
num_e <- sum(all_e)
extrahls <- set_a[all_e]
extrahls <- matrix(extrahls, nrow=num_e)

all_m <- df_a$label[1:124] == "murmur"
num_m <- sum(all_m)
murmur <- set_a[all_m]
murmur <- matrix(murmur, nrow=num_m)

all_n <- df_a$label[1:124] == "normal"
num_n <- sum(all_n)
normal <- set_a[all_n]
normal <- matrix(normal, nrow=num_n)

# Take deviation from the mean

# mean_set_a <- colMeans(set_a)
# artifacts <- artifacts-mean_set_a
# extrahls <- extrahls-mean_set_a
# murmur <- murmur-mean_set_a
# normal <- normal-mean_set_a

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

plot(sort(rowMeans(murmur)), main="Sorted rowMeans")
points(sort(rowMeans(artifacts)), col="blue")
points(sort(rowMeans(normal)), col="green")
points(sort(rowMeans(extrahls)), col="red")
legend(x="topleft", 
       legend=c('Murmur', 'Artifacts', 'Normal', 'Extrahls'), 
       col=c('black', 'blue', 'green', 'red'), 
       pch=1)

dev.copy(jpeg,filename="Sorted_rowMeans.png")
dev.off()

# Using Rowmeans does not show definitive separation


