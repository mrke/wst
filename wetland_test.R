load('micro_wetland.Rda')
ystart <- 1971# start year
yfinish <- 2014# end year
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model

micro<-micro_wetland

tzone=paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates=seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
dates=subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
dates2=seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days") 
dates2=subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years

metout<-as.data.frame(cbind(dates,as.data.frame(micro$metout))) # above ground microclimatic conditions, min shade
shadmet<-as.data.frame(cbind(dates,as.data.frame(micro$shadmet))) # above ground microclimatic conditions, max shade
soil<-as.data.frame(cbind(dates,as.data.frame(micro$soil))) # soil temperatures, minimum shade
shadsoil<-as.data.frame(cbind(dates,as.data.frame(micro$shadsoil))) # soil temperatures, maximum shade
soilmoist<-as.data.frame(cbind(dates,as.data.frame(micro$soilmoist))) # soil water content, minimum shade
shadmoist<-as.data.frame(cbind(dates,as.data.frame(micro$shadmoist))) # soil water content, maximum shade
humid<-as.data.frame(cbind(dates,as.data.frame(micro$humid))) # soil humidity, minimum shade
shadhumid<-as.data.frame(cbind(dates,as.data.frame(micro$shadhumid))) # soil humidity, maximum shade
soilpot<-as.data.frame(cbind(dates,as.data.frame(micro$soilpot))) # soil water potential, minimum shade
shadpot<-as.data.frame(cbind(dates,as.data.frame(micro$shadpot))) # soil water potential, maximum shade
rainfall<-as.data.frame(cbind(dates2,as.data.frame(micro$RAINFALL)))
wetlandDepths<-as.data.frame(cbind(dates,as.data.frame(metout$POOLDEP)))
wetlandTemps<-as.data.frame(cbind(dates,as.data.frame(soil$D0cm)))
#wetlandTemps[metout$POOLDEP==0,2]<-soil[metout$POOLDEP==0,4]
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
filename='wetland_depth_test.pdf'
pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
par(mfrow=c(5,2))
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar = c(3,3,1,1) + 0.1) # margin spacing stuff 

for(i in 1971:2014){
  startdate<-paste('01/01/',i,sep='')
  enddate<-paste('31/12/',i,sep='')
  dstart1<-as.POSIXct(as.Date(startdate, "%d/%m/%Y"))-3600*10
  dfinish1<-as.POSIXct(as.Date(enddate, "%d/%m/%Y"))-3600*11+3600*24
  data<-subset(wetlandDepths,  wetlandDepths$dates > dstart1 & wetlandDepths$dates < dfinish1 )
  plot(data[,2]~data[,1],type='l',col = "black",lty=1,ylab='pond depth (mm)',xlab='date',ylim=c(0,700),main=i)
  points(obs.dep2$date,obs.dep2$depth*10,type='p',col='blue')
}

dev.off()

# water temp data
filename='wetland_temperature_test.pdf'
pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
par(mfrow=c(2,1))
par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
par(mar = c(3,3,1,1) + 0.1) # margin spacing stuff 

for(i in 2008:2009){
  startdate<-paste('01/01/',i,sep='')
  enddate<-paste('31/12/',i,sep='')
  dstart1<-as.POSIXct(as.Date(startdate, "%d/%m/%Y"))-3600*10
  dfinish1<-as.POSIXct(as.Date(enddate, "%d/%m/%Y"))-3600*11+3600*24
  data<-subset(wetlandDepths,  wetlandDepths$dates > dstart1 & wetlandDepths$dates < dfinish1 )
  plot(data[,2]/10~data[,1],type='l',col = "black",lty=1,ylab='pond depth (cm)',xlab='date',ylim=c(0,70),main=i)
  points(obs.dep2$date,obs.dep2$depth,type='p',col='blue')
  points(rainfall[,1],rainfall[,2]*10,type='h',lty=1,col=addTrans("blue",70))
  data<-subset(wetlandTemps,  wetlandTemps$dates > dstart1 & wetlandTemps$dates < dfinish1 )
  points(data[,2]~data[,1],type='l',col = "grey",lty=1,ylim = c(0,80),main=i,ylab='temperature (C)',xlab='date')
  points(obs.temp$date,obs.temp$WaterTemp,type='l',col=addTrans("red",70))
  abline(28,0,lty=2)
}
dev.off()