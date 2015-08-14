ref<-read.csv('growth data/Recaptured Wild WST Ref Sheet.csv')
ENBR<-read.csv('growth data/Recaptured Wild WST ENBR.csv',stringsAsFactors=FALSE)
colnames(ENBR)<-c("ID","Date","Mass","Length","Loc")
ENBR$Date<-as.POSIXct(ENBR$Date,format="%d/%m/%Y")
ENBR_LW<-na.omit(ENBR)
with(ENBR_LW,plot(Mass~Length))

ENBR_LW<-ENBR_LW[!ENBR_LW$Mass>2000,] # get rid of mass outlier
with(ENBR_LW,plot(Mass~Length))
ENBR_LW<-subset(ENBR_LW,Length<150) # get rid of length outliers
with(ENBR_LW,plot(Mass~Length))
write.csv(ENBR_LW,'growth data/ENBR_LW.csv')

IDs<-unique(ENBR$ID)
agg<-aggregate(ENBR$Mass,by=list(ENBR$ID),FUN=length)
IDs<-agg[order(-agg$x),1]
par(mfrow = c(2,1)) # set up for 2 plots in 1 column
for(i in 1:20){
  data<-subset(ENBR,ID==IDs[i])
  if(nrow(data)>15){
  with(data,plot(Mass~Date,ylim=c(0,500),main=IDs[i]))
  with(data,plot(Length~Date,ylim=c(0,200),main=IDs[i]))
  }
}