#Set to wherever your files are stored
setwd("../Results")
library(dplyr)
library(magrittr)
# Define RawData and SteadyData files to read from
# If X signal is delayed, define signalDelay to match number of minutes
signalDelay <- 0
RawData <- read.csv(file="I1_AND_10x_Sx0.csv")
SteadyData <- read.csv(file="I1_AND_10x_SteadyXY.csv")

# Makes an array of Particle Number and Time to create the mean graph
zt <- by(RawData$Z.ParticleNumber, RawData$Time, mean)
zm <- array(zt)
plot(1:201,zm, type = "l", xlab="Time(min)",ylab="Z Particle Number")

# Adds in trial numbers to a new data set for all simulations
zt <- list()
for (i in 1:1000) {
  zt <- c(zt,rep.int(i,201))
}
RawData$Trial <- zt
RawData %>% group_by(Trial) %>% top_n(1, Z.ParticleNumber)

# Use the steady state file (of any length) to compute the equilibrium (SteadyMean)
# and the standard deviation (SteadySD) from that mean
SteadyMean <- mean(SteadyData$Z.ParticleNumber)
SteadySD <- sd(SteadyData$Z.ParticleNumber)

# Trims each trial to first 50 minutes to see if a pulse occured
# within the first 50 minutes
p <- 0
Zmax <- data.frame()
for (i in 1:1000) {
  t <- subset(RawData,RawData$Trial==i)
  t <- subset(t,t$Time<=(50+signalDelay))
  m <- max(t$Z.ParticleNumber)
  b <- t[min(which(t$Z.ParticleNumber==m)),]
  Zmax <- rbind(Zmax,b)
}
for (i in 1:1000){
  if(Zmax$Z.ParticleNumber[i]>(SteadyMean+2*SteadySD))
    p=p+1
}
PercentPulse <- p/10


