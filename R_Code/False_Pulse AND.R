#Set to wherever your files are stored
setwd("../Results")
library(dplyr)
library(magrittr)
# Reads data from the Result Files folder
SteadyData <- read.csv(file="C1_AND_SteadyXY.csv")
RawData <- read.csv(file="C1_AND_SteadyXY1000.csv")

# Makes an array of Particle Number and Time to create the mean graph
zt <- by(RawData$Z.ParticleNumber, RawData$Time, mean)
zm <- array(zt)
# in the following line, adjust the range to either 201 or 1001
plot(1:1001,zm, type = "l")

#Adds in trial numbers to a new data set for all 1000 simulations
zt <- list()
for (i in 1:1000) {
  # in the following line, adjust the range to either 201 or 1001
  zt <- c(zt,rep.int(i,1001))
}
RawData$Trial <- zt
RawData %>% group_by(Trial) %>% top_n(1, Z.ParticleNumber)

#Computes the maximum Z value and stores each trial's max in a data frame
Zmax <- data.frame()
for (i in 1:1000) {
  t <- subset(RawData,RawData$Trial==i)
  m <- max(t$Z.ParticleNumber)
  b <- t[min(which(t$Z.ParticleNumber==m)),]
  Zmax <- rbind(Zmax,b)
}

#Uses the steady state file (of any length) to compute the equilibrium (SteadyMean)
#and the standard deviation (SteadySD) from that mean
SteadyMean <- mean(SteadyData$Z.ParticleNumber)
SteadySD <- sd(SteadyData$Z.ParticleNumber)

p <- 0
for (i in 1:1000){
  if(Zmax$Z.ParticleNumber[i]>(SteadyMean+2*SteadySD))
    p=p+1
}
FalsePulse <- p/10

