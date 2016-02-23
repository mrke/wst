library(NicheMapR)
load('micro_wetland.Rda')
load('micro.Rda')
micro_wetland2=micro_wetland
micro2=micro
longlat <- c(116.032883,-31.7567)
loc<-longlat

# function to choose specific time period of environmental data
runyear<-function(yr=8,micro=micro2,micro_wetland=micro_wetland2){
  
  ystart <- 1971# start year
  yfinish <- 2014# end year
  nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model
  # append dates
  tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
  dates<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
  dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
  dates2<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days") 
  dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
  
  if(yr>=2){
    # subset years
    ystart <- 1970+yr# start year
    yfinish <- 2014# end year
    nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model
    ind<-which(dates %in% subset(dates, dates>=as.POSIXct(paste(ystart,"-01-01 00:00:00",sep=""),format="%Y-%m-%d %H:%M:%S") & dates<as.POSIXct(paste(yfinish+1,"-01-01 00:00:00",sep=""),format="%Y-%m-%d %H:%M:%S")))
    ind2<-which(dates2 %in% subset(dates2, dates2>=as.POSIXct(paste(ystart,"-01-01",sep=""),format="%Y-%m-%d") & dates2<=as.POSIXct(paste(yfinish+1,"-01-01",sep=""),format="%Y-%m-%d")))
    micro_wetland$metout<-micro_wetland$metout[ind+1,]
    micro_wetland$soil<-micro_wetland$soil[ind+1,]
    micro$metout<-micro$metout[ind+1,]
    micro$shadmet<-micro$shadmet[ind+1,]
    micro$soil<-micro$soil[ind+1,]
    micro$shadsoil<-micro$shadsoil[ind+1,]
    micro$soilmoist<-micro$soilmoist[ind+1,]
    micro$shadmoist<-micro$shadmoist[ind+1,]
    micro$soilpot<-micro$soilpot[ind+1,]
    micro$shadpot<-micro$shadpot[ind+1,]
    micro$humid<-micro$humid[ind+1,]
    micro$shadhumid<-micro$shadhumid[ind+1,]
    micro$RAINFALL<-micro$RAINFALL[ind2]
    micro$MAXSHADES<-micro$MAXSHADES[ind2]
    micro$nyears=nyears
    micro$dim=length(ind2)
    # append dates
    tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
    dates<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
    dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
    dates2<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days") 
    dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
  }

  metout<-micro_wetland$metout
  soil<-micro_wetland$soil
  wetlandDepths<-metout[,10] # pool depth
  wetlandTemps<-soil[,4] # make water temp soil surface temp (under water)

  return(list(dates=dates,dates2=dates2,micro=micro,wetlandTemps=wetlandTemps,wetlandDepths=wetlandDepths,ystart=ystart,yfinish=yfinish,nyears=nyears))
} # end of function to choose period for environmental data

# load wild growth data
ref<-read.csv('growth data/Recaptured Wild WST Ref Sheet.csv')
ENBR<-read.csv('growth data/Recaptured Wild WST ENBR.csv',stringsAsFactors=FALSE)
colnames(ENBR)<-c("ID","Date","Mass","Length","Loc")
ENBR$Date<-as.POSIXct(ENBR$Date,format="%d/%m/%Y")
ENBR_LW<-read.csv('growth data/ENBR_LW.csv')
agg<-aggregate(ENBR$Mass,by=list(ENBR$ID),FUN=length)
IDs<-agg[order(-agg$x),1]

# now loop through turtles and run for particular start years and basking constraints
yrstrt=c(14,12,36,12,28,28,28,28,28,24,4,20,20,13,5,1,15,36,36,36) # starting year
minshads=c(80,75,20,70,85,90,40,50,10,100,60,60,100,70,45,65,40,50,10,10) # minimum shade (imposes basking constraints)

for(turtle in 1:20){
  
  subyear<-runyear(yr=yrstrt[turtle]) # choose environmental time period
  micro=subyear$micro
  wetlandDepths=subyear$wetlandDepths
  wetlandTemps=subyear$wetlandTemps
  ystart=subyear$ystart
  yfinish=subyear$yfinish
  nyears=subyear$nyears
  dates=subyear$dates
  dates2=subyear$dates2
  
  debpars=as.data.frame(read.csv('DEB model/DEB_pars_Pseudemydura_umbrina.csv',header=FALSE))$V1
  minshade=minshads[turtle]
  
  # ensure a couple of the turtles start as almost adult by 1971
  if(turtle %in% c(12,13)){
    v_init = (debpars[26]*.95)^3
    stage = 3
    E_H_init = debpars[21]+5
  }else{
    v_init = debpars[25]^3
    stage = 1
    E_H_init = debpars[20]+5
  }
  
  # run ectotherm model
  nicheout=ectotherm(ABSMAX = 0.9, ABSMIN = 0.9, VTMAX = 28, VTMIN = 14, write_input = 0, EMISAN = 1, gutfill = 101,
    TBASK = 14, TEMERGE = 14, ctmax = 41.9, ctmin = 1, TPREF = 27,  peyes = 0.0, ptcond = 0.1, skinwet = 0, nocturn = 0, crepus = 0,
    shdburrow = 2, minwater = 15, maxshades = micro$MAXSHADES*0+90, minshade = minshade, mindepth = 2, maxdepth = 9, container = 1, wetmod = 1, DEB = 1, z=debpars[8], del_M=debpars[9], F_m = 13290,  
    kap_X=debpars[11],   v=debpars[13]/24, kap=debpars[14], p_M=debpars[16]/24, 
    E_G=debpars[19],   kap_R=debpars[15], k_J=debpars[18]/24, E_Hb=debpars[20],
    E_Hp=debpars[21], h_a=debpars[22]*10^-1/(24^2),   s_G=debpars[23],   E_0=debpars[24],  
    T_REF = debpars[1]-273, TA = debpars[2], TAL = debpars[5], TAH = debpars[6], TL = debpars[3], TH = debpars[4], 
    E_sm=186.03*6, K = 500, X = 11.7, plantsim = 0, clutch_ab = c(0,0), viviparous = 0,
    photostart = 1, photofinish = 4, E_init = ((debpars[16]/24)*debpars[8]/debpars[14])/(debpars[13]/24), E_H_init = E_H_init,  
    v_init = v_init, stage = stage, mh = 1,  minclutch = 0, clutchsize = 4, aestivate = 1, depress = 0.1,
    wetlandDepths = wetlandDepths, wetlandTemps = wetlandTemps, startday = 4.5*30, frogbreed = 4)
  
  metout<-as.data.frame(nicheout$metout)
  shadmet<-as.data.frame(nicheout$shadmet)
  soil<-as.data.frame(nicheout$soil)
  shadsoil<-as.data.frame(nicheout$shadsoil)
  rainfall<-as.data.frame(nicheout$RAINFALL)
  grassgrowths<-as.data.frame(nicheout$grassgrowths)
  grasstsdms<-as.data.frame(nicheout$grasstsdms)
  environ<-as.data.frame(nicheout$environ)
  enbal<-as.data.frame(nicheout$enbal)
  masbal<-as.data.frame(nicheout$masbal)
  debout<-as.data.frame(nicheout$debout)
  
  yearout<-as.data.frame(nicheout$yearout)
  if(nyears>1){
    yearsout<-as.data.frame(nicheout$yearsout)
  }else{
    yearsout<-t(as.data.frame(nicheout$yearsout))
  }
  
  wetlandTemps<-as.data.frame(environ$WATERTEMP)
  wetlandDepths<-as.data.frame(environ$CONDEP)
  
  rainfall<-as.data.frame(cbind(dates2,rainfall))
  colnames(rainfall)<-c("dates","rainfall")
  
  debout<-cbind(dates,debout)
  environ<-cbind(dates,environ)
  masbal<-cbind(dates,masbal)
  enbal<-cbind(dates,enbal)
  soil<-cbind(dates,soil)
  metout<-cbind(dates,metout)
  shadsoil<-cbind(dates,shadsoil)
  shadmet<-cbind(dates,shadmet)
  
  # plot results
  filename=paste('growth_',IDs[turtle],'.pdf',sep="")
  pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
  par(mfrow = c(2,1))
  par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
  par(mar = c(3,4,1,1) + 0.1) # margin spacing stuff 
  # plot predicted growth against observed for a given turtle
  with(debout, {plot(WETMASS~dates,type = "l",xlab = "year",ylab = "wet mass (g)",col='blue',ylim=c(0,500))})
  with(environ, points(CONDEP/10~dates,type='l'))
  data<-subset(ENBR,ID==IDs[turtle]) # 144,341,380,387,196,4,256
  with(data,points(Mass~Date,col='red', cex=0.75, pch=4))
  title(main=paste("turtle #",IDs[turtle],sep=""))
  
  # plot predicted length
  with(debout, {plot(SVL~dates,type = "l",xlab = "year",ylab = "carapace length (mm)",col='blue',ylim=c(0,140))})
  points(environ$TC~dates,type='l',col='light blue')
  abline(27,0,lty=2,col='red')
  with(environ, points(CONDEP/10~dates,type='l'))
  with(data,points(Length~Date,col='red',cex=0.75, pch=4))
  dev.off()
}
