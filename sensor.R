##################### ---READ ALL FILES ----#######################
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  l= length(filenames)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  k=list()
  for (i in 1:144){
    m=data.frame(datalist[i])
    m$id = i
    k[i] = list(m)
  }
  Reduce(function(x,y) {rbind(x,y)}, k)
}
####### -----Reading Detector 3 ----- ###############
mymergeddata = multmerge("/Data/Detector3/")
View(mymergeddata)
dim(mymergeddata)
p <- list()
for (i in 1:105505) {p[i]=3}
mymergeddata$detectorid <- p
############# -------- Reading Detector 4 -------- ###############
detector4 <- multmerge("/Data/Detector4/")
View(detector4) 
dim(detector4)
p <- list()
for (i in 1:105036) {p[i]=4}
detector4$detectorid <- p
######## -----Merging Detector 3 and Dtector 4----------##########
merged <- rbind(mymergeddata,detector4)
############## ---- CALCULATE MEAN AT EVERY 5 MINUTES ------ ###########
speed3 <- aggregate(mymergeddata$Speed..mph. ~ mymergeddata$id, mymergeddata, mean)
speed4 <- aggregate(detector4$Speed..mph. ~ detector4$id, detector4, mean)
############## ---- CALCULATE OCCUPANCY OF SPEED AT EVERY 5 MINUTES ------ ###########
occupancy3 <- aggregate(mymergeddata$Occupancy..s. ~ mymergeddata$id, mymergeddata, sum)
occupancy3 <- occupancy3$`mymergeddata$Occupancy..s.`/300 
occupancy3 <- lapply(occupancy3, function(x){if(x>1){x=1*100}else{x*100}})
####----Occupancy for sensor 4------######
occupancy4 <- aggregate(detector4$Occupancy..s. ~ detector4$id, detector4, sum)
occupancy4 <- occupancy4$`detector4$Occupancy..s.`/300 
occupancy4 <- lapply(occupancy4, function(x){if(x>1){x=1*100}else{x*100}})
############## ---- CALCULATE FLOW AT EVERY 5 MINUTES ------ ###########
c=list()
for (i in 1:144) {
  s= subset(mymergeddata,mymergeddata$id == i)
  c[i]= length(unique(na.omit(s$Vehicle.Id)))*12
}

speed3$occupancy3 <- occupancy3
speed3$Flow <- c
View(speed3)
detector3 <- speed3[,c("mymergeddata$Speed..mph.","Flow","occupancy3")]
detector3<-setNames(detector3,c("Speed","Flow","Occupancy"))
View(detector3)
##########---------Flow for Detector 4 ------##############
v=list()
for (i in 1:144) {
  s= subset(detector4,detector4$id == i)
  v[i]= length(unique(na.omit(s$Vehicle.Id)))*12
}

speed4$occupancy4 <- occupancy4
speed4$Flow <- v
View(speed4)
detector4n <- speed4[,c("detector4$Speed..mph.","Flow","occupancy4")]
detector4n<-setNames(detector4n,c("Speed","Flow","Occupancy"))
View(detector4n)
########--------- Calculate Density for detector 3-------################
Density <- 5280*(as.numeric(detector3$Occupancy))/(28+6.6)
detector3$Density<- Density/100
View(detector3)
dev.new(width=8.4, height=5.0)
par(mfrow=c(2,1))

#########-----------Density for detector 4 --------###############
Density <- 5280*(as.numeric(detector4n$Occupancy))/(28+6.6)

detector4n$Density<- Density/100
View(detector4n)

#########------- Speed / Density for detector  3 with Limits-------#########
dev.new(width=8.4, height=5.0)
par(mfrow=c(2,2))
plot(detector3$Speed,detector3$Density,xlab = 'Speed',ylab = "Density",pch=1,col="orange",main = "Detector 3 Speed Density",xlim = c(0,80))
#########------- Speed / Density for detector 4 with Limits -------#########
plot(detector4n$Speed,detector4n$Density,xlab = 'Speed',ylab = "Density",main = "Detector 4 Speed Density",pch=1,col="orange",xlim = c(0,80))
#########------- Density/Flow for detector 3 with Limits -------#########
plot(detector3$Density,detector3$Flow,xlab = 'Density',ylab = "Flow",pch=1,col="orange",xlim = c(0,80),ylim = c(0,2200))
#########------- Density/Flow for detector 4 with Limits-------#########
plot(detector4n$Density,detector4n$Flow,xlab = 'Density',ylab = "Flow",pch=1,col="orange",xlim = c(0,80),ylim = c(0,2200))
########### -------- CALCULATE K JAM Detector 4 -------#################
vf = 65
kjam = (vf*detector3$Density)/(vf- detector3$Speed)
detector3$kjam <- kjam
View(set1)
View(detector3)
####### ---------- 95 Percentile ---------############
percentile <- quantile(detector3$kjam, c(.95))
######## -------- calculate Velocity --------- ##########

vdash1 <- (vf/percentile)
vdash <- vf- vdash1*detector4n$Density
set1 <- detector4n
set1$vdash <- vdash
qdash <- set1$Density* set1$vdash
set1$qdash <- qdash
########## 1-c Velosity-Flow---#######
dev.new(width=8.4, height=5.0)
par(mfrow=c(2,1))
plot(set1$vdash,set1$qdash,main = "Estimated Speed Flow",xlab= "Speed",ylab = "Flow",col="skyblue",pch=19)
plot(set1$qdash,set1$vdash,main = "Estimated Flow Speed",xlab= "Flow",ylab = "Speed",col="skyblue",pch=19)
##### ---- 1-d Speed-Density-Flow with with Limit------#######
dev.new(width=8.4, height=5.0)
par(mfrow=c(2,2))
plot(detector4n$Speed,detector4n$Density,col="maroon",main = "Estimated Speed Density(withLimits)",xlab= "Speed",ylab = "Density",pch=19,xlim = c(0,80),ylim =c(0,250))
points(vdash,detector4n$Density,col="skyblue",pch=19, ylim=c(0,250))
plot(detector4n$Density,detector4n$Flow,col="maroon",main = "Estimated Density Flow(with limits)",xlab= "Density",ylab = "Flow",pch=19,xlim = c(0,250),ylim = c(0,2200))
points(detector4n$Density,qdash,col="skyblue",pch=19, xlim=c(0,250))
##### ---- Speed-Density-Flow with without Limit------
plot(detector4n$Speed,detector4n$Density,col="maroon",main = "Estimated Speed Density (without Limits)",xlab= "Speed",ylab = "Density",pch=19)
points(vdash,detector4n$Density,col="skyblue",pch=19)
plot(detector4n$Density,detector4n$Flow,col="maroon",main = "Estimated Density Flow (without limits)",xlab= "Density",ylab = "Flow",pch=19)
points(detector4n$Density,qdash,col="skyblue",pch=19)

plot(detector4n$Density,qdash,main = "Estimated Density Flow",xlab= "Density",ylab = "Flow",col="skyblue",pch=19)
######### ------Time creation Code -----#######
timesequence<-seq(
  from=as.POSIXct("0:00","%H:%M", tz="GMT"),
  to=as.POSIXct("12:00", "%H:%M", tz="GMT"),
  by=5
) 
timesequence=timesequence[2:145]

df <- format(timesequence,"%M:%S")

xaxis<-sapply(strsplit(df,":"),
              function(x) {
                x <- as.numeric(x)
                x[1]+x[2]/60
              }
)
xaxis
df
######## ------------plotting with given limits--------##############
######-----Detector 3 plots ----############

dev.new(width=8.4, height=7.0)
par(mfrow=c(2,3))
plot(detector3$Flow,detector3$Speed,main = "Flow vs Speed",xlab = "Flow",ylab="Speed",
     col='red',xlim = c(0,2200),ylim = c(0,80))
legend("topright",c("Vehicles"),col=c('red'),pch=1,cex=0.8)
#### --- flow v svg occupancy ---###
plot(detector3$Flow,detector3$Occupancy,main = "Flow vs Occupancy",xlab = "Flow",ylab="Occupancy",
     col='red',xlim=c(0,2200),ylim = c(0,70))
legend("topleft",c("Vehicles"),col=c('red'),pch=1,cex=0.8)

###---- Speed vs Average Occupancy--- ####
plot(detector3$Speed,detector3$Occupancy,main = "Speed vs Average Occupancy",xlab = "Speed",ylab="Occpancy",
     col='red',xlim = c(0,80),ylim = c(0,70))
legend("topright",c("Vehicles"),col=c('red'),pch=1,cex=0.8)

####---plotting time stams --- ##########
plot(xaxis,detector3$Flow,type = "l",ylim = c(0,2200),main ="Flow vs Time",pch=16,col='red',xaxt = "n",xlab = "Time",ylab = "Flow")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)
plot(type = "l",xaxis,detector3$Speed,ylim = c(0,80),main ="Speed vs Time",pch=16,col='red',xaxt = "n",xlab = "Time",ylab = "Speed")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)
plot(type = "l",xaxis,detector3$Occupancy,ylim = c(0,70),main ="Occupancy vs Time",pch=16,col='red',xaxt = "n",xlab = "Time",ylab = "Occupancy")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)
###############------Detector 4 ---------##################
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,3))
###--flow vs speed --###
plot(detector4n$Flow,detector4n$Speed,main = "Detector 4 Flow vs Speed",xlab = "Flow",ylab="Speed",
     col='red',xlim = c(0,2200),ylim = c(0,80))
legend("topright",c("Vehicles"),col=c('red'),pch=1,cex=0.8)

#### --- flow v avg occupancy ---###
plot(detector4n$Flow,detector4n$Occupancy,main = "Detector 4 Flow vs Occupancy",xlab = "Flow",ylab="Occupancy",
     col='red',xlim=c(0,2200),ylim = c(0,70))
legend("topleft",c("Vehicles"),col=c('red'),pch=1,cex=0.8)

###---- Speed vs Average Occupancy--- ####
plot(detector4n$Speed,detector4n$Occupancy,main = "Detector 4 Speed vs Average Occupancy",xlab = "Speed",ylab="Occpancy",
     col='red',xlim = c(0,80),ylim = c(0,70))
legend("topright",c("Vehicles"),col=c('red'),pch=1,cex=0.8)

######---Timestamps for homework ----#########
plot(type = "l",xaxis,detector4n$Flow,ylim = c(0,2200),main = "Flow vs Time",pch=16,col='red',xaxt = "n",xlab = "Time",ylab = "Flow")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)
plot(type = "l",xaxis,detector4n$Speed,ylim = c(0,80),pch=16,main = "Speed vs Time",col='red',xaxt = "n",xlab = "Time",ylab = "Speed")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)
plot(type = "l",xaxis,detector4n$Occupancy,ylim = c(0,70),pch=16,main = "Occupancy vs Time",col='red',xaxt = "n",xlab = "Time",ylab = "Occupancy")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)

######################------------Plots Without any limits --------############

#############---- detector 3 ----############
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,3))
plot(detector3$Flow,detector3$Speed,main = "Flow vs Speed",xlab = "Flow",ylab="Speed",
     col='red')
legend("topright",c("Vehicles"),col=c('red'),pch=1,cex=0.8)
#### --- flow v avg occupancy ---###
plot(detector3$Flow,detector3$Occupancy,main = "Flow vs Occupancy",xlab = "Flow",ylab="Occupancy",
     col='red')
legend("topleft",c("Vehicles"),col=c('red'),pch=1,cex=0.8)

###---- Speed vs Average Occupancy--- ####
plot(detector3$Speed,detector3$Occupancy,main = "Speed vs Average Occupancy",xlab = "Speed",ylab="Occpancy",
     col='red')
legend("topright",c("Vehicles"),col=c('red'),pch=1,cex=0.8)

####---plotting time stams --- ##########
plot(xaxis,detector3$Flow,type = "l",main ="Flow vs Time",pch=16,col='red',xaxt = "n",xlab = "Time",ylab = "Flow")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)
plot(type = "l",xaxis,detector3$Speed,main ="Speed vs Time",pch=16,col='red',xaxt = "n",xlab = "Time",ylab = "Speed")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)
plot(type = "l",xaxis,detector3$Occupancy,main ="Occupancy vs Time",pch=16,col='red',xaxt = "n",xlab = "Time",ylab = "Occupancy")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)
###############------Detector 4 ---------##################
dev.new(width=8.4, height=7.0)
par(mfrow=c(2,3))
###--flow vs speed --###
plot(detector4n$Flow,detector4n$Speed,main = "Detector 4 Flow vs Speed",xlab = "Flow",ylab="Speed",
     col='red',xlim = c(0,2200),ylim = c(0,80))
legend("topright",c("Vehicles"),col=c('red'),pch=1,cex=0.8)

#### --- flow v avg occupancy ---###
plot(detector4n$Flow,detector4n$Occupancy,main = "Detector 4 Flow vs Occupancy",xlab = "Flow",ylab="Occupancy",
     col='red',xlim=c(0,2200),ylim = c(0,70))
legend("topleft",c("Vehicles"),col=c('red'),pch=1,cex=0.8)

###---- Speed vs Average Occupancy--- ####
plot(detector4n$Speed,detector4n$Occupancy,main = "Detector 4 Speed vs Average Occupancy",xlab = "Speed",ylab="Occpancy",
     col='red',xlim = c(0,80),ylim = c(0,70))
legend("topright",c("Vehicles"),col=c('red'),pch=1,cex=0.8)

######---Timestamps for homework ----#########
plot(type = "l",xaxis,detector4n$Flow,ylim = c(0,2200),main = "Flow vs Time",pch=16,col='red',xaxt = "n",xlab = "Time",ylab = "Flow")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)
plot(type = "l",xaxis,detector4n$Speed,ylim = c(0,80),pch=16,main = "Speed vs Time",col='red',xaxt = "n",xlab = "Time",ylab = "Speed")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)
plot(type = "l",xaxis,detector4n$Occupancy,ylim = c(0,70),pch=16,main = "Occupancy vs Time",col='red',xaxt = "n",xlab = "Time",ylab = "Occupancy")
ax <- axis(side = 1,
           at = xaxis,
           labels = df,
           tck=-.1,
           tcl = -0.5,
           cex.axis=1.05,
           col.axis="black",
           font.axis=5)
##################------ Part 2 a-------------------################

plot(xaxis, cumsum(detector3$Flow)/10000,xlab = "Time",ylab = "Cumulative Sum",col="red",type='l',main = "Cumulative sum/10000")
lines(xaxis, cumsum(detector4n$Flow)/10000,col='blue',lty=2,lw=2)
legend("topleft",c("Arrivals","Departures"),col=c("red", "blue"),lty=1,cex=0.8)

############------------------ 2c --------------------------###########
plot(xaxis,(cumsum(detector3$Flow)- cumsum(detector4n$Flow)),ylab = "Difference in upstream and downstream",
     xlab = "time",main = "Start Time When Upstream flow is 2000 more than downstream",col="brown",pch=19)
diff<-cumsum(detector3$Flow)- cumsum(detector4n$Flow)
m= data.frame(diff,xaxis)
m =setNames(m,c("CumsumDifference","Time"))
n = subset(m,m$CumsumDifference>2000)
c2=min(n$Time)
c2
abline(v=c(c2),col='red')

###############------------------ 2d --------------------#################
diff<- rnorm(144, mean = 50, sd = 150)
m$Randomdifference <- diff
m$flow <- detector3$Flow
m$estimatedDeparture <- as.numeric(m$flow) + m$Randomdifference 
plot(m$Time,m$estimatedDeparture,type ='l',col='darkgreen',ylab = "Departure Time", xlab = "Time",
        main = "Arrival and the Estimated Departure Curve",lwd=2)
lines(m$Time,m$flow,lty=2,col= 'darkred',lwd=2)
legend("bottomright",inset=.05, c("arrival","departure"),lty = c(1,2),
       horiz=TRUE,lwd=c(2.5,2.5),col=c("darkgreen","darkred"))
