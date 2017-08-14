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
  
# 
# Wobj <- readWave('.\\Final Project\\set_a\\artifact__201105040918.wav')
# Wobj
# play(Wobj)
# 
# Wobj@left
# 
# fft(Wobj@left)
# 
# plot(Re(fft(Wobj@left)))
# plot(Wobj@left, pch=16)
