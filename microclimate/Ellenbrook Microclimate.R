# script to set up Ellenbrook microclimate files for a particular period of time for ectotherm model, and to test 
# wetland model predictions

microdir<-'microclimate/Ellenbrook/'

ystart<-1911
yfinish<-2014
if(!require(geonames)){
  stop('package "geonames" is required.')
}
tzone<-paste("Etc/GMT-10",sep="") # doing it this way ignores daylight savings!

wetlandTemps_all<-as.data.frame(read.csv(paste(microdir,'wetlandTemps',ystart,'_',yfinish,'.csv',sep=""))[-1])
wetlandDepths_all<-as.data.frame(read.csv(paste(microdir,'wetlandDepths',ystart,'_',yfinish,'.csv',sep=""))[-1])
metout_all<-as.data.frame(read.csv(paste(microdir,'metout',ystart,'_',yfinish,'.csv',sep=""))[-1])
shadmet_all<-as.data.frame(read.csv(paste(microdir,'shadmet',ystart,'_',yfinish,'.csv',sep=""))[-1])
soil_all<-as.data.frame(read.csv(paste(microdir,'soil',ystart,'_',yfinish,'.csv',sep=""))[-1])
shadsoil_all<-as.data.frame(read.csv(paste(microdir,'shadsoil',ystart,'_',yfinish,'.csv',sep=""))[-1])
soilmoist_all<-as.data.frame(read.csv(paste(microdir,'soilmoist',ystart,'_',yfinish,'.csv',sep=""))[-1])
shadmoist_all<-as.data.frame(read.csv(paste(microdir,'shadmoist',ystart,'_',yfinish,'.csv',sep=""))[-1])
humid_all<-as.data.frame(read.csv(paste(microdir,'humid',ystart,'_',yfinish,'.csv',sep=""))[-1])
shadhumid_all<-as.data.frame(read.csv(paste(microdir,'shadhumid',ystart,'_',yfinish,'.csv',sep=""))[-1])
soilpot_all<-as.data.frame(read.csv(paste(microdir,'soilpot',ystart,'_',yfinish,'.csv',sep=""))[-1])
shadpot_all<-as.data.frame(read.csv(paste(microdir,'shadpot',ystart,'_',yfinish,'.csv',sep=""))[-1])
rainfall_all<-as.data.frame(read.csv(paste(microdir,'rainfall',ystart,'_',yfinish,'.csv',sep=""))[-1])

DEP <- c(0.,2.,  5.,  10,  15,  20.,  30.,  60.,  90.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature

metout.names<-c("JULDAY","TIME","TALOC","TAREF","RHLOC","RH","VLOC","VREF","SOILMOIST3","POOLDEPTH","TDEEP","ZEN","SOLR","TSKYC","DEW","FROST","SNOWFALL","SNOWDEP")
colnames(metout_all)<-metout.names
colnames(shadmet_all)<-metout.names
soil.names<-c("JULDAY","TIME",paste("D",DEP,"cm", sep = ""))
colnames(soil_all)<-soil.names
colnames(shadsoil_all)<-soil.names
moist.names<-c("JULDAY","TIME",paste("WC",DEP,"cm", sep = ""))
humid.names<-c("JULDAY","TIME",paste("RH",DEP,"cm", sep = ""))
pot.names<-c("JULDAY","TIME",paste("PT",DEP,"cm", sep = ""))
colnames(soilmoist_all)<-moist.names
colnames(shadmoist_all)<-moist.names
colnames(humid_all)<-humid.names
colnames(shadhumid_all)<-humid.names
colnames(soilpot_all)<-pot.names
colnames(shadpot_all)<-pot.names


dates<-seq(ISOdate(1911,1,1,tz=tzone)-3600*12, ISOdate(2015,1,1,tz=tzone)-3600*13, by="hours") 
dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
dates<-unique(dates)
dates2<-seq(ISOdate(1911,1,1,tz=tzone)-3600*12, ISOdate(2015,1,1,tz=tzone)-3600*13, by="days") 
dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years

metout_all<-cbind(dates,metout_all)
shadmet_all<-cbind(dates,shadmet_all)
soil_all<-cbind(dates,soil_all)
shadsoil_all<-cbind(dates,shadsoil_all)
soilmoist_all<-cbind(dates,soilmoist_all)
shadmoist_all<-cbind(dates,shadmoist_all)
humid_all<-cbind(dates,humid_all)
shadhumid_all<-cbind(dates,shadhumid_all)
soilpot_all<-cbind(dates,soilpot_all)
shadpot_all<-cbind(dates,shadpot_all)
wetlandTemps_all<-cbind(dates,wetlandTemps_all)
wetlandDepths_all<-cbind(dates,wetlandDepths_all)
rainfall2_all<-as.data.frame(cbind(dates2,rainfall_all))
colnames(rainfall2_all)<-c('dates','rainfall')

# choose a period
dstart<-as.POSIXct(strptime('01/01/1996 00:00:00', "%d/%m/%Y %H:%M:%S"),tz=tzone)
dfinish<-as.POSIXct(strptime('31/12/2014 23:00:00', "%d/%m/%Y %H:%M:%S"),tz=tzone)

# subset data for that period
plotsoilmoist<-subset(soilmoist_all,  soilmoist_all$dates >= dstart & soilmoist_all$dates <= dfinish )
plothumid<-subset(humid_all,  humid_all$dates >= dstart & humid_all$dates <= dfinish )
plotsoilpot<-subset(soilpot_all,  soilpot_all$dates >= dstart & soilpot_all$dates <= dfinish )
plotsoil<-subset(soil_all,  soil_all$dates >= dstart & soil_all$dates <= dfinish )
plotmetout<-subset(metout_all,  metout_all$dates >= dstart & metout_all$dates <= dfinish )
plotshadmoist<-subset(shadmoist_all,  shadmoist_all$dates >= dstart & shadmoist_all$dates <= dfinish )
plotshadhumid<-subset(shadhumid_all,  shadhumid_all$dates >= dstart & shadhumid_all$dates <= dfinish )
plotshadpot<-subset(shadpot_all,  shadpot_all$dates >= dstart & shadpot_all$dates <= dfinish )
plotshadsoil<-subset(shadsoil_all,  shadsoil_all$dates >= dstart & shadsoil_all$dates <= dfinish )
plotshadmet<-subset(shadmet_all,  shadmet_all$dates >= dstart & shadmet_all$dates <= dfinish )
juldays<-rep(seq(1:(nrow(plotmetout)/24)),24) # need to create a new vector of juldays because they are out of order from separate sims
juldays<-juldays[order(juldays)]
plotsoilmoist$JULDAY<-juldays
plothumid$JULDAY<-juldays
plotsoilpot$JULDAY<-juldays
plotsoil$JULDAY<-juldays
plotmetout$JULDAY<-juldays
plotshadmoist$JULDAY<-juldays
plotshadhumid$JULDAY<-juldays
plotshadpot$JULDAY<-juldays
plotshadsoil$JULDAY<-juldays
plotshadmet$JULDAY<-juldays
plotrainfall<-subset(rainfall2_all,  rainfall2_all$dates >= dstart & rainfall2_all$dates <= dfinish )
plotwetlandTemps<-subset(wetlandTemps_all,  wetlandTemps_all$dates >= dstart & wetlandTemps_all$dates <= dfinish )
plotwetlandDepths<-subset(wetlandDepths_all,  wetlandDepths_all$dates >= dstart & wetlandDepths_all$dates <= dfinish )
MAXSHADES<-rep(as.data.frame(read.csv(paste(microdir,'MAXSHADES1911_2014.csv',sep=""))[-1])[1,1],nrow(plotrainfall))
ectoin<-as.data.frame(read.csv(paste(microdir,'ectoin1911_2014.csv',sep=""))[-1])
ectoin[7,1]<-as.numeric(format(dstart,"%Y"))
ectoin[8,1]<-as.numeric(format(dfinish,"%Y"))


# format output for ectotherm model
metout<-as.matrix(plotmetout[,-1])
soil<-as.matrix(plotsoil[,-1])
soilpot<-as.matrix(plotsoilpot[,-1])
humid<-as.matrix(plothumid[,-1])
soilmoist<-as.matrix(plotsoilmoist[,-1])
shadmet<-as.matrix(plotshadmet[,-1])
shadsoil<-as.matrix(plotshadsoil[,-1])
shadhumid<-as.matrix(plotshadhumid[,-1])
shadpot<-as.matrix(plotshadpot[,-1])
shadmoist<-as.matrix(plotshadmoist[,-1])
rainfall<-as.matrix(plotrainfall[,-1])
wetlandTemps<-as.matrix(plotwetlandTemps[,-1])
wetlandDepths<-as.matrix(plotwetlandDepths[,-1])
ectoin<-as.matrix(ectoin)
DEP<-as.matrix(DEP)
MAXSHADES<-as.matrix(MAXSHADES)
RAINFALL<-rainfall

# write output for ectotherm model
write.csv(plotmetout[,-1],paste(microdir,'metout.csv',sep=""))
write.csv(plotsoil[,-1],paste(microdir,'soil.csv',sep=""))
write.csv(plotsoilpot[,-1],paste(microdir,'soilpot.csv',sep=""))
write.csv(plothumid[,-1],paste(microdir,'humid.csv',sep=""))
write.csv(plotsoilmoist[,-1],paste(microdir,'soilmoist.csv',sep=""))
write.csv(plotshadmet[,-1],paste(microdir,'shadmet.csv',sep=""))
write.csv(plotshadsoil[,-1],paste(microdir,'shadsoil.csv',sep=""))
write.csv(plotshadhumid[,-1],paste(microdir,'shadhumid.csv',sep=""))
write.csv(plotshadpot[,-1],paste(microdir,'shadpot.csv',sep=""))
write.csv(plotshadmoist[,-1],paste(microdir,'shadmoist.csv',sep=""))
write.csv(plotrainfall[,-1],paste(microdir,'rainfall.csv',sep=""))
write.csv(plotwetlandTemps[,-1],paste(microdir,'wetlandTemps.csv',sep=""))
write.csv(plotwetlandDepths[,-1],paste(microdir,'wetlandDepths.csv',sep=""))
write.csv(ectoin,paste(microdir,'ectoin.csv',sep=""))
write.csv(DEP,paste(microdir,'DEP.csv',sep=""))
write.csv(MAXSHADES,paste(microdir,'MAXSHADES.csv',sep=""))


# plots and tests against observations

# function for transparent colours
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

# read in observed wetland data
obs.dep<-read.csv("c:/NicheMapR_Working/projects/WST/Water_Levels.csv")
obs.dep$Date<-as.POSIXct(as.Date(obs.dep$Date,format="%d/%m/%Y"))
obs.dep$Level<-obs.dep$Level*10
obs.temp<-read.csv("c:/NicheMapR_Working/projects/WST/obs_temps2008_2009.csv")
obs.temp$date<-strptime(obs.temp$date, format="%d/%m/%Y %H:%M", tz = tzone)
obs.dep2<-read.csv("c:/git/wst/microclimate/ellenbrook.csv",stringsAsFactors=FALSE)
obs.dep2$date<-strptime(obs.dep2$date, format="%d/%m/%Y", tz = tzone)
obs.dep2<-obs.dep2[order(obs.dep2$date),]

# water depth data
par(mfrow=c(5,2))
    par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
    par(mar = c(3,3,1,1) + 0.1) # margin spacing stuff 

for(i in 1971:2014){
  startdate<-paste('01/01/',i,sep='')
  enddate<-paste('31/12/',i,sep='')
dstart1<-as.POSIXct(as.Date(startdate, "%d/%m/%Y"))-3600*11
dfinish1<-as.POSIXct(as.Date(enddate, "%d/%m/%Y"))-3600*10+3600*24
data<-subset(plotwetlandDepths,  plotwetlandDepths$dates > dstart1 & plotwetlandDepths$dates < dfinish1 )
plot(data$x~data$dates,type='l',col = "black",lty=1,ylab='pond depth (mm)',xlab='date',ylim=c(0,700),main=i)
points(obs.dep2$date,obs.dep2$depth*10,type='p',col='blue')
}

# water temp data
par(mfrow=c(2,1))
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar = c(3,3,1,1) + 0.1) # margin spacing stuff 

for(i in 2008:2009){
  startdate<-paste('01/01/',i,sep='')
  enddate<-paste('31/12/',i,sep='')
dstart1<-as.POSIXct(as.Date(startdate, "%d/%m/%Y"))-3600*11
dfinish1<-as.POSIXct(as.Date(enddate, "%d/%m/%Y"))-3600*10+3600*24
data<-subset(plotwetlandDepths,  plotwetlandDepths$dates > dstart1 & plotwetlandDepths$dates < dfinish1 )
plot(data$x/10~data$dates,type='l',col = "black",lty=1,ylab='pond depth (cm)',xlab='date',ylim=c(0,70),main=i)
points(obs.dep2$date,obs.dep2$depth,type='p',col='blue')
points(rainfall$dates,rainfall$rainfall*10,type='h',lty=1,col=addTrans("blue",70))
data<-subset(plotwetlandTemps,  plotwetlandTemps$dates > dstart1 & plotwetlandTemps$dates < dfinish1 )
points(data$dates, data$x,type='l',col = "grey",lty=1,ylim = c(0,80),main=i,ylab='temperature (C)',xlab='date')
points(obs.temp$date,obs.temp$WaterTemp,type='l',col=addTrans("red",70))
abline(28,0,lty=2)
}

# general plots of outputs
par(mfrow=c(1,1))
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar = c(3,3,1,1) + 0.1) # margin spacing stuff 

plot(plotsoilmoist$dates, plotsoilmoist[,4]*100,type='l',col = "red",lty=1,ylim = c(0,50),ylab='moisture (% vol)',xlab='date')
points(plotsoilmoist$dates, plotsoilmoist[,5]*100,type='l',col = 3,lty=1,ylim = c(0,50),ylab='moisture (% vol)',xlab='date')
points(plotsoilmoist$dates, plotsoilmoist[,6]*100,type='l',col = 4,lty=1,ylim = c(0,50),ylab='moisture (% vol)',xlab='date')
points(plotsoilmoist$dates, plotsoilmoist[,7]*100,type='l',col = 5,lty=1,ylim = c(0,50),ylab='moisture (% vol)',xlab='date')
points(plotsoilmoist$dates, plotsoilmoist[,8]*100,type='l',col = 6,lty=1,ylim = c(0,50),ylab='moisture (% vol)',xlab='date')
points(plotsoilmoist$dates, plotsoilmoist[,9]*100,type='l',col = 7,lty=1,ylim = c(0,50),ylab='moisture (% vol)',xlab='date')
points(plotsoilmoist$dates, plotsoilmoist[,10]*100,type='l',col = 8,lty=1,ylim = c(0,50),ylab='moisture (% vol)',xlab='date')
points(plotsoilmoist$dates, plotsoilmoist[,11]*100,type='l',col = 9,lty=1,ylim = c(0,50),ylab='moisture (% vol)',xlab='date')
points(plotsoilmoist$dates, plotsoilmoist[,12]*100,type='l',col = 10,lty=1,ylim = c(0,50),ylab='moisture (% vol)',xlab='date')
points(plotsoilmoist$dates, plotsoilmoist[,13]*100,type='l',col = 11,lty=1,ylim = c(0,50),ylab='moisture (% vol)',xlab='date')
points(plotrainfall$rainfall~plotrainfall$dates,type='h',col='dark blue')

plot(plothumid$dates, plothumid[,4]*100,type='l',col = "red",lty=1,ylim = c(0,100),ylab='relative humdity (%)',xlab='date')
points(plothumid$dates, plothumid[,5]*100,type='l',col = 3,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plothumid$dates, plothumid[,6]*100,type='l',col = 4,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plothumid$dates, plothumid[,7]*100,type='l',col = 5,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plothumid$dates, plothumid[,8]*100,type='l',col = 6,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plothumid$dates, plothumid[,9]*100,type='l',col = 7,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plothumid$dates, plothumid[,10]*100,type='l',col = 8,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plothumid$dates, plothumid[,11]*100,type='l',col = 9,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plothumid$dates, plothumid[,12]*100,type='l',col = 10,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plothumid$dates, plothumid[,13]*100,type='l',col = 11,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')

plot(plotsoilpot$dates, plotsoilpot[,4],type='l',col = "red",lty=1,ylim = c(-5000,0),ylab='water potential (J/kg)',xlab='date')
points(plotsoilpot$dates, plotsoilpot[,5],type='l',col = 3,lty=1,ylim = c(-300,0),ylab='water potential (J/kg)',xlab='date')
points(plotsoilpot$dates, plotsoilpot[,6],type='l',col = 4,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoilpot$dates, plotsoilpot[,7],type='l',col = 5,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoilpot$dates, plotsoilpot[,8],type='l',col = 6,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoilpot$dates, plotsoilpot[,9],type='l',col = 7,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoilpot$dates, plotsoilpot[,10],type='l',col = 8,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoilpot$dates, plotsoilpot[,11],type='l',col = 9,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoilpot$dates, plotsoilpot[,12],type='l',col = 10,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoilpot$dates, plotsoilpot[,13],type='l',col = 11,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')

plot(plotsoil$dates, plotsoil[,4],type='l',col = "red",lty=1,ylim = c(-10,80),ylab='temperature (C)',xlab='date')
points(plotsoil$dates, plotsoil[,5],type='l',col = 3,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoil$dates, plotsoil[,6],type='l',col = 4,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoil$dates, plotsoil[,7],type='l',col = 5,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoil$dates, plotsoil[,8],type='l',col = 6,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoil$dates, plotsoil[,9],type='l',col = 7,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoil$dates, plotsoil[,10],type='l',col = 8,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoil$dates, plotsoil[,11],type='l',col = 9,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoil$dates, plotsoil[,12],type='l',col = 10,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')
points(plotsoil$dates, plotsoil[,13],type='l',col = 11,lty=1,ylim = c(0,50),ylab='relative humdity (%)',xlab='date')


