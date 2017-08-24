# Update 2

# name of the directories of the files

setwd('C:\\Users\\lye\\OneDrive\\Documents\\GitHub\\r-assignments')

files <- c('.\\Final Project\\set_a.csv',
           '.\\Final Project\\set_b.csv',
           '.\\Final Project\\set_a_timing.csv')

# read all the csvs

df_a <- read.csv(files[1])
df_b <- read.csv(files[2])
df_a_timing <- read.csv(files[3])

# read all of the wav files
# use tuneR.readWave to interpret wav files into a vector of integers
# append the vector to a matrix

library(tuneR)

set_a <- c()
set_b <- c()

for (item in df_a$fname){
  wave <- readWave(paste('.\\Final Project\\',item, sep=""))
  set_a <- rbind(set_a,wave@left)
}

for (item in df_b$fname){
  wave <- readWave(paste('.\\Final Project\\',item, sep=""))
  set_b <- rbind(set_b,wave@left)
}

# Update 3 : Technically Correct data

# replace empty cells with NA in df_a, df_b, df_a_timing
# set_a and set_b do not need this treatment because they are numerical vectors/matrices already

levels(df_a$label) <- c(NA, "artifact", "extrahls", "murmur", "normal")
levels(df_b$label) <- c(NA, "extrastole", "murmur", "normal")
levels(df_b$sublabel) <- c(NA, "noisy murmur", "noisy normal")

# Coerce numeric columns into strings

df_a_timing$cycle <- as.numeric(df_a_timing$cycle)
df_a_timing$location <- as.numeric(df_a_timing$location)
