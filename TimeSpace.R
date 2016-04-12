#############################################

###GM Car Following Model
GMCarFollowing<-function( period)
{
  for(i in 2:period)
  {
    Ant[i]<-LeadingCar[i,13]
    Vnt[i] <- LeadingCar[i,12]
    Xnt[i] <- LeadingCar[i,20]
    
    Vn1t[i] <- Result[i-1,5] + Result[i-1,4]*dt
    Xn1t[i] <- Result[i-1,6] + Result[i-1,5]  * dt + 0.5*Result[i-1,4] *(dt)^2
    if(abs((Result[max(i-DeltT/dt,1),2]-Result[max(i-DeltT/dt,1),5]))>0)
    {
      An1t[i] <- (Alm* (Vn1t[i])^m /(Result[max(i-DeltT/dt,1),3]-Result[max(i-DeltT/dt,1),6])^L) * (Result[max(i-DeltT/dt,1),2]  - Result[max(i-DeltT/dt,1),5])
    }else{
      An1t[i] <- 0
    }
    Time[i]<-FollowingCar[i,19]
    Result<-rbind(Result,cbind(Ant[i],Vnt[i],Xnt[i],An1t[i],Vn1t[i],Xn1t[i],Time[i]))
  }
  return(Result)
}
#############################################
Rawdata<- read.table("/I101-trajectories-0750am-0805am.txt",header=FALSE,sep="")
minTime<-min(Rawdata[,4])
RefX<-min(Rawdata[,5])
RefY<-min(Rawdata[,6])
##Select vehciles using Lane = 1 (Mainlane: 1= farthest left lane; 5= farthest right lane)
TrajLane1<-subset(Rawdata,Rawdata[,14]==1)
dim(TrajLane1)
#Get the vehicle ID
VehID<-as.numeric(as.vector(data.frame(table(TrajLane1[,1]))$Var1))
TotalNumVeh<-length(unique(VehID))

#Get a pair of car-following vehicles
VehID
TrajLane1$V1
LeadingCar<-TrajLane1[which(TrajLane1$V1==1554),]
Time<-(LeadingCar[,4]-minTime)/1000
TravelDistance<-sqrt((LeadingCar[,5]-RefX)^2+(LeadingCar[,6]-RefY)^2)
LeadingCar<-cbind(LeadingCar,Time,TravelDistance)
FollowingCar<-TrajLane1[which(TrajLane1$V1==1556),]
Time<-(FollowingCar[,4]-minTime)/1000
TravelDistance<-sqrt((FollowingCar[,5]-RefX)^2+(FollowingCar[,6]-RefY)^2)
FollowingCar<-cbind(FollowingCar,Time,TravelDistance)
LeadingCar<-LeadingCar[which(LeadingCar$Time>=420 & LeadingCar$Time<=485.1),]
FollowingCar<-FollowingCar[which(FollowingCar$Time>=420 & FollowingCar$Time<=485.1),]
#############################################

#Assumed Parameters
dt<-0.1     #Time interval
DeltT<-1.5  #Reaction time
m<-0.1    # from -2 to 2
L<-1        # from -1 to 4
Alm<-10     #sensitivity coefficient

##Initial Values

#Leading vehicle
Xnt<-LeadingCar[1,20] #Initial position
Vnt<-LeadingCar[1,12] #Initial speed
Ant<-LeadingCar[1,13] #Initial acceleration
#Following vehicle
Xn1t <- FollowingCar[1,20]
Vn1t <- FollowingCar[1,12]
An1t <- FollowingCar[1,13]
Time<-FollowingCar[1,19]
Result<-cbind(Ant,Vnt,Xnt, An1t,Vn1t,Xn1t,Time)

##Running the car following model
Result<-GMCarFollowing(length(FollowingCar[,19]))
#############################################
dev.new(width=8.4, height=5.0)
par(mfrow=c(1,2))
##Trajectory
plot(NA,NA,xlim=c(420,486),ylim=c(0,2200),xlab="Time (Interval=0.1 s)",ylab="Location(ft)", main="Time Space Diagram",cex.main=0.7,tck=0.01,cex.lab=0.8,xaxt='n',yaxt='n',xaxs="i",yaxs="i")
axis(1, at=seq(420,486,10), labels=seq(420,486,10),lty=1, col=1, tck=-0.01,cex.axis=0.7, las=1)
axis(2, at=seq(0,2200,100),labels=seq(0,2200,100),lty=1, col=1, tck=-0.01,cex.axis=0.7)
lines(Result[,7],Result[,3],lty=1,lwd=2,col=1) ##Observed results of leading vehicle
lines(Result[,7],FollowingCar[,20],lty=1,lwd=2,col=5)  ##Observed results of following vehicle
lines(Result[,7],Result[,6],lty=1,lwd=2,col=2) ##Modeled resultsof following vehicle
legend("topleft",c("Leading Vehicle","Following Vehicle Observed","Following Vehicle Modeled"),col=c(1,5,2),lty=1,lwd=2,cex=0.8)
#############################################
##Error
plot(Result[,7],(FollowingCar[,20]- Result[,6]),type = 'l',xlab = "Time Interval",ylab = "Difference in predicted and Actual Model",lty=1,lwd=2,col='green')

##Manipulating parameters

dt<-0.1     #Time interval
DeltT<-1.5  #Reaction time
m<-0.2    # from -2 to 2
L<-1        # from -1 to 4
Alm<-20 
#Leading vehicle
Xnt<-LeadingCar[1,20] #Initial position
Vnt<-LeadingCar[1,12] #Initial speed
Ant<-LeadingCar[1,13] #Initial acceleration
#Following vehicle
Xn1t <- FollowingCar[1,20]
Vn1t <- FollowingCar[1,12]
An1t <- FollowingCar[1,13]
Time<-FollowingCar[1,19]
Result<-cbind(Ant,Vnt,Xnt, An1t,Vn1t,Xn1t,Time)

##Running the car following model
Result<-GMCarFollowing(length(FollowingCar[,19]))

#Improved Trajectory
plot(NA,NA,xlim=c(420,486),ylim=c(0,2200),xlab="Time (Interval=0.1 s)",ylab="Location(ft)", main="Time Space Diagram (Improved)",cex.main=0.7,tck=0.01,cex.lab=0.8,xaxt='n',yaxt='n',xaxs="i",yaxs="i")
axis(1, at=seq(420,486,10), labels=seq(420,486,10),lty=1, col=1, tck=-0.01,cex.axis=0.7, las=1)
axis(2, at=seq(0,2200,100),labels=seq(0,2200,100),lty=1, col=1, tck=-0.01,cex.axis=0.7)
lines(Result[,7],Result[,3],lty=1,lwd=2,col=1) ##Observed results of leading vehicle
lines(Result[,7],FollowingCar[,20],lty=1,lwd=2,col=5)  ##Observed results of following vehicle
lines(Result[,7],Result[,6],lty=1,lwd=2,col=2) ##Modeled resultsof following vehicle
lines(Result[,7],(FollowingCar[,20]- Result[,6]),type = 'l',lty=1,lwd=2,col='green')
legend("topleft",c("Leading Vehicle","Following Vehicle Observed","Following Vehicle Modeled"),col=c(1,5,2),lty=1,lwd=2,cex=0.8)

#Error2
lines(Result[,7],(FollowingCar[,20]- Result[,6]),type = 'l',lty=2,lwd=2,col='red')
legend("topleft",c("Sestitivity=10, m=0.1","Sestitivity=20,m=0.2"),col=c("green","red"),lty=c(1,2),lwd=2,cex=0.8)
