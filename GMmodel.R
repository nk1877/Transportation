StartRead=Sys.time() 
Rawdata<-read.table("/I101-trajectories-0805am-0820am.txt",header=FALSE,sep="")
dim(Rawdata)
EndRead=Sys.time() 
EndRead-StartRead

#Reference Start Time and Locations
lanes <- function(laneno, speed,all){
  
  minTime<-min(Rawdata[,4])
  RefX<-min(Rawdata[,5])
  RefY<-min(Rawdata[,6])
  
  if(all==T){
    TrajLane1 <- Rawdata
    laneno<-0
  }
  
  else{
    TrajLane1<-subset(Rawdata,Rawdata[,14]==laneno)
    all=0
  }
  VehID<-unique(TrajLane1[,1])
  TotalNumVeh<-length(VehID)
  plot(NA,NA,xlim=c(0,900),ylim=c(0,2200),xlab="Time (second)",ylab="Location (ft)",cex=0.8,cex.lab=0.9,tck=-0.01)
  SelectedVehs<-1:TotalNumVeh
  for(i in SelectedVehs)
  {
    SingleVeh<-subset(TrajLane1,TrajLane1[,1]==VehID[i]) 
    TravelDistance<-sqrt((SingleVeh[,5]-RefX)^2+(SingleVeh[,6]-RefY)^2)
    lines((SingleVeh[,4]-minTime)/1000,TravelDistance,lty=1,col=3)
    index<-which(SingleVeh[,12]<speed)  # possible shockwave area
    points((SingleVeh[index,4]-minTime)/1000,TravelDistance[index],pch=19,col="red",cex=0.9)
  }
  
}
##Plots for all the vehicles
lanes(0,10,T)
