# Script to find residuals from deterministic 
# counterpart and sum them
#### SETUP ####
#Set to results and whichever model you are working on
wd <- "../Results"
setwd(wd)

library(dplyr)
library(magrittr)

# Reads data from the Result Files folder and adds a z component to 
# count through things
RawData <- read.csv(file="C1_OR_SRG_10x_SxOFF200.csv")
DetData <- read.csv(file="C1_OR_SRG_10x_SxOFF200_Det.csv")

# Correlate each stochastic run and then take the average of those
# correlation coefficients

# First add trial numbers in
zt <- list()
for (i in 1:1000) {
  zt <- c(zt,rep.int(i,201))
}
RawData$Trial <- zt
RawData %>% group_by(Trial) %>% top_n(1, Z.ParticleNumber)

# For each trial number store the coefficient in a data frame
CC <- data.frame()
for (i in 1:1000) {
  t <- subset(RawData,RawData$Trial==i)
  c <- cor(t$Z.ParticleNumber, DetData$Z.ParticleNumber)
  CC <- rbind(CC, c)
}
names(CC) <- 'cc'
mean(CC$cc)
sd(CC$cc)
hist(CC$cc)


