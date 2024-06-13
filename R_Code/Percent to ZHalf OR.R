#Set to wherever your files are stored
setwd("../Results")

# change this for each folder of zhalf responses
loop1 = "C1_OR"
loop2 = "C1_OR_10x"
loop3 = "C1_OR_SRG"
loop4 = "C1_OR_SRG_10x"
dir1 = "../Results/C1_OR"
dir2 = "../Results/C1_OR_10x"
dir3 = "../Results/C1_OR_SRG"
dir4 = "../Results/C1_OR_SRG_10x"
library(dplyr)
library(magrittr)
library(ggplot2)
# Reads data from the Results folder and adds a z component to 
# count through things

files1<-list.files(path=dir1, full.names = TRUE)
files2<-list.files(path=dir2, full.names = TRUE)
files3<-list.files(path=dir3, full.names = TRUE)
files4<-list.files(path=dir4, full.names = TRUE)


signalDelay <- 0
SteadyData1 <- read.csv(file=paste(loop1,"_SteadyXY.csv", sep=""))
SteadyData1b <- read.csv(file=paste(loop1,"_SteadyYnoX.csv", sep=""))
SteadyData2 <- read.csv(file=paste(loop2,"_SteadyXY.csv", sep=""))
SteadyData2b <- read.csv(file=paste(loop2,"_SteadyYnoX.csv", sep=""))
SteadyData3 <- read.csv(file=paste(loop3,"_SteadyXY.csv", sep=""))
SteadyData3b <- read.csv(file=paste(loop3,"_SteadyYnoX.csv", sep=""))
SteadyData4 <- read.csv(file=paste(loop4,"_SteadyXY.csv", sep=""))
SteadyData4b <- read.csv(file=paste(loop4,"_SteadyYnoX.csv", sep=""))

#Uses the steady state file (of any length) to compute the equilibrium (SteadyMean)
#and the standard deviation (SteadySD) from that mean
SteadyMean1 <- mean(SteadyData1$Z.ParticleNumber)
SteadyMean1b <- mean(SteadyData1b$Z.ParticleNumber)
SteadySD1 <- sd(SteadyData1$Z.ParticleNumber)

SteadyMean2 <- mean(SteadyData2$Z.ParticleNumber)
SteadyMean2b <- mean(SteadyData2b$Z.ParticleNumber)
SteadySD2 <- sd(SteadyData2$Z.ParticleNumber)

SteadyMean3 <- mean(SteadyData3$Z.ParticleNumber)
SteadyMean3b <- mean(SteadyData3b$Z.ParticleNumber)
SteadySD3 <- sd(SteadyData3$Z.ParticleNumber)

SteadyMean4 <- mean(SteadyData4$Z.ParticleNumber)
SteadyMean4b <- mean(SteadyData4b$Z.ParticleNumber)
SteadySD4 <- sd(SteadyData4$Z.ParticleNumber)

zhalf_percents1 <- c()
zhalf_times1 <- c()
zhalf_percents2 <- c()
zhalf_times2 <- c()
zhalf_percents3 <- c()
zhalf_times3 <- c()
zhalf_percents4 <- c()
zhalf_times4 <- c()

for (file in files1){
  RawData <- read.csv(file)
  file <- gsub("../Results/",'',file)
  file <- gsub("/",'',file)
  file <- gsub(loop1,'',file)
  file <- gsub("SxOFF",'',file)
  file <- gsub("_",'',file)
  file <- gsub("min",'',file)
  file <- gsub(".csv",'',file)
  print(file)
  time = as.numeric(file)
  zhalf_times1 <- append(zhalf_times1, time)
  # Makes an array of Particle Number and Time to create the mean graph
  zt <- by(RawData$Z.ParticleNumber, RawData$Time, mean)
  zm <- array(zt)
  
  #Adds in trial numbers to a new data set for all 1000 simulations
  zt <- list()
  for (i in 1:1000) {
    zt <- c(zt,rep.int(i,201))
  }
  RawData$Trial <- zt
  RawData %>% group_by(Trial) %>% top_n(1, Z.ParticleNumber)
  p <- 0
  Zmin <- data.frame()
  for (i in 1:1000) {
    t <- subset(RawData,RawData$Trial==i)
    t <- subset(t,t$Time<=250)
    m <- min(t$Z.ParticleNumber)
    b <- t[min(which(t$Z.ParticleNumber==m)),]
    Zmin <- rbind(Zmin,b)
  }
  for (i in 1:1000){
    if(Zmin$Z.ParticleNumber[i]<(SteadyMean1 - (SteadyMean1 - SteadyMean1b)/2))
      p=p+1
  }
  PercentZHalf <- p/10
  zhalf_percents1 <- append(zhalf_percents1, PercentZHalf)
}

zhalfs1 <- tibble(zhalf_times1, zhalf_percents1)

for (file in files2){
  RawData <- read.csv(file)
  file <- gsub("../Results/",'',file)
  file <- gsub("/",'',file)
  file <- gsub(loop2,'',file)
  file <- gsub("SxOFF",'',file)
  file <- gsub("_",'',file)
  file <- gsub("min",'',file)
  file <- gsub(".csv",'',file)
  print(file)
  time = as.numeric(file)
  zhalf_times2 <- append(zhalf_times2, time)
  # Makes an array of Particle Number and Time to create the mean graph
  zt <- by(RawData$Z.ParticleNumber, RawData$Time, mean)
  zm <- array(zt)
  
  #Adds in trial numbers to a new data set for all 1000 simulations
  zt <- list()
  for (i in 1:1000) {
    zt <- c(zt,rep.int(i,201))
  }
  RawData$Trial <- zt
  RawData %>% group_by(Trial) %>% top_n(1, Z.ParticleNumber)
  p <- 0
  Zmin <- data.frame()
  for (i in 1:1000) {
    t <- subset(RawData,RawData$Trial==i)
    t <- subset(t,t$Time<=250)
    m <- min(t$Z.ParticleNumber)
    b <- t[min(which(t$Z.ParticleNumber==m)),]
    Zmin <- rbind(Zmin,b)
  }
  for (i in 1:1000){
    if(Zmin$Z.ParticleNumber[i]<(SteadyMean2 - (SteadyMean2 - SteadyMean2b)/2))
      p=p+1
  }
  PercentZHalf <- p/10
  zhalf_percents2 <- append(zhalf_percents2, PercentZHalf)
}

zhalfs2 <- tibble(zhalf_times2, zhalf_percents2)

for (file in files3){
  RawData <- read.csv(file)
  file <- gsub("../Results/",'',file)
  file <- gsub("/",'',file)
  file <- gsub(loop3,'',file)
  file <- gsub("SxOFF",'',file)
  file <- gsub("_",'',file)
  file <- gsub("min",'',file)
  file <- gsub(".csv",'',file)
  print(file)
  time = as.numeric(file)
  zhalf_times3 <- append(zhalf_times3, time)
  # Makes an array of Particle Number and Time to create the mean graph
  zt <- by(RawData$Z.ParticleNumber, RawData$Time, mean)
  zm <- array(zt)
  
  #Adds in trial numbers to a new data set for all 1000 simulations
  zt <- list()
  for (i in 1:1000) {
    zt <- c(zt,rep.int(i,201))
  }
  RawData$Trial <- zt
  RawData %>% group_by(Trial) %>% top_n(1, Z.ParticleNumber)
  p <- 0
  Zmin <- data.frame()
  for (i in 1:1000) {
    t <- subset(RawData,RawData$Trial==i)
    t <- subset(t,t$Time<=250)
    m <- min(t$Z.ParticleNumber)
    b <- t[min(which(t$Z.ParticleNumber==m)),]
    Zmin <- rbind(Zmin,b)
  }
  for (i in 1:1000){
    if(Zmin$Z.ParticleNumber[i]<(SteadyMean3 - (SteadyMean3 - SteadyMean3b)/2))
      p=p+1
  }
  PercentZHalf <- p/10
  zhalf_percents3 <- append(zhalf_percents3, PercentZHalf)
}

zhalfs3 <- tibble(zhalf_times3, zhalf_percents3)

for (file in files4){
  RawData <- read.csv(file)
  file <- gsub("../Results/",'',file)
  file <- gsub("/",'',file)
  file <- gsub(loop4,'',file)
  file <- gsub("SxOFF",'',file)
  file <- gsub("_",'',file)
  file <- gsub("min",'',file)
  file <- gsub(".csv",'',file)
  print(file)
  time = as.numeric(file)
  zhalf_times4 <- append(zhalf_times4, time)
  # Makes an array of Particle Number and Time to create the mean graph
  zt <- by(RawData$Z.ParticleNumber, RawData$Time, mean)
  zm <- array(zt)
  
  #Adds in trial numbers to a new data set for all 1000 simulations
  zt <- list()
  for (i in 1:1000) {
    zt <- c(zt,rep.int(i,201))
  }
  RawData$Trial <- zt
  RawData %>% group_by(Trial) %>% top_n(1, Z.ParticleNumber)
  p <- 0
  Zmin <- data.frame()
  for (i in 1:1000) {
    t <- subset(RawData,RawData$Trial==i)
    t <- subset(t,t$Time<=250)
    m <- min(t$Z.ParticleNumber)
    b <- t[min(which(t$Z.ParticleNumber==m)),]
    Zmin <- rbind(Zmin,b)
  }
  for (i in 1:1000){
    if(Zmin$Z.ParticleNumber[i]<(SteadyMean4 - (SteadyMean4 - SteadyMean4b)/2))
      p=p+1
  }
  PercentZHalf <- p/10
  zhalf_percents4 <- append(zhalf_percents4, PercentZHalf)
}

zhalfs4 <- tibble(zhalf_times4, zhalf_percents4)

ld<-ggplot()
hd<-ggplot()
ld<-ld+geom_point(data=zhalfs1, size=4, aes(x=zhalf_times1,y=zhalf_percents1,colour = "CFFL1 OR",shape="CFFL1 OR"),stroke=1)
ld<-ld+geom_point(data=zhalfs3, size=4, aes(x=zhalf_times3,y=zhalf_percents3,colour = "SRG",shape="SRG"),stroke=1)

hd<-hd+geom_point(data=zhalfs2, size=4, aes(x=zhalf_times2,y=zhalf_percents2,colour = "CFFL1 OR 10x",shape="CFFL1 OR 10x"),stroke=1)
hd<-hd+geom_point(data=zhalfs4, size=4, aes(x=zhalf_times4,y=zhalf_percents4,colour = "SRG 10x",shape="SRG 10x"),stroke=1)

ld<- ld+labs(shape = "", colour = "")+ scale_colour_manual(values = c("red", "blue"))+scale_shape_manual(values = c(1,3))
ld + theme(plot.title = element_text(hjust = 0.5)) + ylim(0, 100)+theme_bw()+
  xlab("Duration of Signal Interruption (min)") + ylab("Probability of 'Adequate Response'")

hd<- hd+labs(shape = "", colour = "")+ scale_colour_manual(values = c("red", "blue"))+scale_shape_manual(values = c(1,3))
hd + theme(plot.title = element_text(hjust = 0.5)) + ylim(0, 100)+theme_bw()+
  xlab("Duration of Signal Interruption (min)") + ylab("Probability of 'Adequate Response'")

