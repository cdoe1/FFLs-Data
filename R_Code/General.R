#### SETUP ####
#Set to results and whichever model you are working on
model <- 'C1_OR_SRG'
wd = "../Results"
setwd(wd)

library(dplyr)
library(magrittr)
library(ggplot2)

#### STEAD STATE CALCULATIONS ####
# Reads data from the Result Files folder
SteadyData <- read.csv(file="C1_OR_SRG_SteadyXY.csv")
#Uses the steady state file (of any length) to compute the equilibrium (SteadyMean)
#and the standard deviation (SteadySD) from that mean
SteadyMean <- mean(SteadyData$Z.ParticleNumber)
SteadySD <- sd(SteadyData$Z.ParticleNumber)

# if running OR results, enter the second steady state here
SteadyData2 <- read.csv(file="C1_OR_SRG_SteadyYnoX.csv")
SteadyMean2 <- mean(SteadyData2$Z.ParticleNumber)
SteadySD2 <- sd(SteadyData2$Z.ParticleNumber)

#### ZHALF SETUP ####
signalDelay <- 0
RawData <- read.csv(file="C1_OR_SRG_SxOFF200.csv")
DetData <- read.csv(file="C1_OR_SRG_SxOFF200_Det.csv")

# Makes an array of Particle Number and Time to create the mean graph
zt <- by(RawData$Z.ParticleNumber, RawData$Time, mean)
zm <- array(zt)
#plot(1:201,zm, type = "l", xlab="Time(min)",ylab="Z Particle Number")

#Adds in trial numbers to a new data set for all 1000 simulations
zt <- list()
for (i in 1:1000) {
  zt <- c(zt,rep.int(i,201))
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
#### ZHALF CALCULATIONS ####
#Calculates the time at which the a simulation hits 1/2 of the eventual steady state
Zhalf <- data.frame()
for (i in 1:1000) {
  t <- subset(RawData,RawData$Trial==i)
  # In the next section, the following T1/2 times can be calculated:
  
  # OR version
  # Time to SteadyXY - 1/2(SteadyXY - SteadyYnoX) with X off step at 200 use less than sign:
  # use this and both steady state files
  b <- t[min(which( t$Z.ParticleNumber<(SteadyMean - (SteadyMean - SteadyMean2)/2) )),]
  Zhalf <- rbind(Zhalf,b)
  
  # AND version
  # Time to 1/2 of SteadyXY from an X signal ON step use greater than sign:
  # >SteadyMean/2 to see the first value greater than half the steady mean
  # and use SteadyXY state for final steady state
  # b <- t[min(which( t$Z.ParticleNumber > SteadyMean/2 )),]
  # Zhalf <- rbind(Zhalf,b)
}

#and takes out any values from simulations that failed to reach that 1/2 mark
Zhalf <- na.omit(Zhalf)
#then calculates the mean time and standard deviation for Zhalf
ZhalfMean <- mean(Zhalf$Time)
ZhalfSD <- sd(Zhalf$Time)


#### FIGURES ####
df <- data.frame(time = Zhalf$Time, par = Zhalf$Z.ParticleNumber)
head(df)
# Change line color and fill color
ggplot(df, aes(x=time))+
  geom_histogram(color="darkblue", fill="lightblue")

#hist(Zmax$Z.ParticleNumber, xlab = "Time(min)", ylab="Z Particles")
#plot(Zmax$Time, Zmax$Z.ParticleNumber,xlim=c(0,200), xlab = "Time(min)", ylab="Z Particles")
# plot all the maxes
#ggplot() + geom_point(data=Zmax, aes(x=Time, y=Z.ParticleNumber), shape=1)
# plot the deterministic run and the average of the stochastic runs together

zt <- by(RawData$Z.ParticleNumber, RawData$Time, mean)
zm <- array(zt)

av = data.frame(max=zm, time=c(0:200))

run1 <- subset(RawData,RawData$Trial==1)
run2 <- subset(RawData,RawData$Trial==2)
run3 <- subset(RawData,RawData$Trial==3)


df2 <- data.frame(groups=rep(c("Stochastic Mean", "Deterministic", "Stochastic Trial 1", "Stochastic Trial 2", "Stochastic Trial 3"), each=201),
                  Time=rep(c(0:200),5),
                  max=c(av$max, DetData$Z.ParticleNumber, run1$Z.ParticleNumber, run2$Z.ParticleNumber, run3$Z.ParticleNumber))


#David's new version of time series figures.
#First one in each set has a legend.
p<-ggplot(data=df2, aes(x=Time, y=max)) +
  geom_line(aes(linetype=groups,color=groups,linewidth=groups)) +
  scale_color_manual(values=c("blue", "red","black","black","black")) +
  scale_linetype_manual(values=c(1,2,1,2,3)) +
  scale_linewidth_manual(values=c(1,1,.5,.5,.7))
p <- p + theme_bw()+ theme(legend.title = element_blank())
p <- p + xlab("Time (min)") + ylab("Z Particle Number")
p + coord_cartesian(xlim = c(0,200), ylim = c(0,100)) +
  theme(legend.position = c(.98,.98), legend.justification = c("right", "top"))

#Other three in each set don't have a legend.
p<-ggplot(data=df2, aes(x=Time, y=max)) +
  geom_line(aes(linetype=groups,color=groups,linewidth=groups)) +
  scale_color_manual(values=c("blue", "red","black","black","black")) +
  scale_linetype_manual(values=c(1,2,1,2,3)) +
  scale_linewidth_manual(values=c(1,1,.5,.5,.7))
p <- p + theme_bw()+ theme(legend.title = element_blank())
p <- p + xlab("Time (min)") + ylab("Z Particle Number")
p + coord_cartesian(xlim = c(0,200), ylim = c(0,100)) +
  theme(legend.position = "none")
