files <- c('.\\Final Project\\set_a.csv',
           '.\\Final Project\\set_b.csv',
           '.\\Final Project\\set_a_timing.csv')

df_a <- read.csv(files[1])
df_b <- read.csv(files[2])
df_a_timing <- read.csv(files[3])

library(tuneR)

Wobj <- readWave('.\\Final Project\\set_a\\artifact__201105040918.wav')
Wobj
play(Wobj)

Wobj@stereo

plot(Re(fft(Wobj@left)))
plot(Wobj@left, pch=16)
