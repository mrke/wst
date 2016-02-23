#WST model with new NicheMapR package

library(NicheMapR)
source("../micro_australia/get.soil.R")
source("micro_aust2.R")
longlat <- c(116.032883,-31.7567)
loc<-longlat
ystart <- 1971# start year
yfinish <- 2014# end year
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model

DEP <- c(0.,2.,  5.,  10,  15,  20.,  30.,  60.,  90.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature

soilpro<-matrix(data = 0, nrow = 6, ncol = 5)
colnames(soilpro)<-c('i','blkdens','clay','silt','sand')
soilpro[,1]<-seq(1,6)
soilpro[,2]<-1.4
soilpro[,3]<-90
soilpro[,4]<-0.0
soilpro[,5]<-10
soilpro<-as.data.frame(soilpro)
soil.hydro<-get.soil(SLGA = 0, soilpro = soilpro)
PE<-soil.hydro$PE
BB<-soil.hydro$BB
BD<-soil.hydro$BD
KS<-soil.hydro$KS
BulkDensity <- BD[seq(1,19,2)]*1000#rep(1360,10) # soil bulk density (kg/m3)

# run for wetland conditions
rainmult<-4.5

micro_wetland<-micro_aust2(loc = longlat, ystart = ystart, yfinish = yfinish, PE = PE, BB = BB, BD = 
    BD, KS = KS, BulkDensity = BulkDensity, minshade = 75, maxshade = 90, Usrhyt = 0.02, DEP = DEP, REFL = 0.2,
  rainmult = rainmult, runmoist = 1, maxpool = 500, cap = 0, LAI = 0, soildata = 0,
  evenrain = 1)
save(micro_wetland,file = 'micro_wetland.Rda') # for use if no access to AWAP server
#load('micro_wetland.Rda')

# run for turtle environment
rainmult<-1
soiltype=4
PE[1:19]<-CampNormTbl9_1[soiltype,4] #air entry potential J/kg 
KS[1:19]<-CampNormTbl9_1[soiltype,6] #saturated conductivity, kg s/m3
BB[1:19]<-CampNormTbl9_1[soiltype,5] #soil 'b' parameter
PE[1:9]<-CampNormTbl9_1[3,4] #air entry potential J/kg 
KS[1:9]<-CampNormTbl9_1[3,6] #saturated conductivity, kg s/m3
BB[1:9]<-CampNormTbl9_1[3,5] #soil 'b' parameter
PE[10:13]<-CampNormTbl9_1[4,4] #air entry potential J/kg 
KS[10:13]<-CampNormTbl9_1[4,6] #saturated conductivity, kg s/m3
BB[10:13]<-CampNormTbl9_1[4,5] #soil 'b' parameter    
micro<-micro_aust2(loc = longlat, ystart = ystart, yfinish = yfinish, PE = PE, BB = BB, BD = 
    BD, KS = KS, BulkDensity = BulkDensity, minshade = 0, maxshade = 90, Usrhyt = 0.02, DEP = DEP, REFL = 0.2, 
  rainmult = rainmult, runmoist = 1, maxpool = 500, soiltype = 4, cap = 0, LAI = 0, evenrain = 1, ERR = 2, soildata = 0)
write.csv(as.data.frame(micro$soil),'soil2.csv')
save(micro,file = 'micro.Rda') # for use if no access to AWAP server
#load('micro.Rda')




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

metout<-as.data.frame(cbind(dates,as.data.frame(micro_wetland$metout))) # above ground microclimatic conditions, min shade
shadmet<-as.data.frame(cbind(dates,as.data.frame(micro_wetland$shadmet))) # above ground microclimatic conditions, max shade
soil<-as.data.frame(cbind(dates,as.data.frame(micro_wetland$soil))) # soil temperatures, minimum shade
shadsoil<-as.data.frame(cbind(dates,as.data.frame(micro_wetland$shadsoil))) # soil temperatures, maximum shade
soilmoist<-as.data.frame(cbind(dates,as.data.frame(micro_wetland$soilmoist))) # soil water content, minimum shade
shadmoist<-as.data.frame(cbind(dates,as.data.frame(micro_wetland$shadmoist))) # soil water content, maximum shade
humid<-as.data.frame(cbind(dates,as.data.frame(micro_wetland$humid))) # soil humidity, minimum shade
shadhumid<-as.data.frame(cbind(dates,as.data.frame(micro_wetland$shadhumid))) # soil humidity, maximum shade
soilpot<-as.data.frame(cbind(dates,as.data.frame(micro_wetland$soilpot))) # soil water potential, minimum shade
shadpot<-as.data.frame(cbind(dates,as.data.frame(micro_wetland$shadpot))) # soil water potential, maximum shade
rainfall<-as.data.frame(cbind(dates2,as.data.frame(micro_wetland$RAINFALL)))



winter<-c("05","06","07","08","09","10")
summer<-c("01","02","03","04","11","12")

# wet period
TC<-subset(soil,  format(soil$dates, "%m") %in% winter)
TC<-soil$D0cm[metout$POOLDEP!=0]
#TC<-TC$D5cm
# reference temp and 5 parameters for the Arrhenius response curve

debpars=as.data.frame(read.csv('DEB model/DEB_pars_Pseudemydura_umbrina.csv',header=FALSE))$V1
T_REF<-debpars[1]-273 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-debpars[2]
TAL<-debpars[5]
TAH<-debpars[6]
TL<-debpars[3]
TH<-debpars[4]

temps<-seq(0,50,.25) # dummy temps for demo plot of response curve
plot(as.numeric(exp(TA*(1/(273+T_REF)-1/(273+temps)))/(1+exp(TAL*(1/(273+temps)-1/TL))+exp(TAH*(1/TH-1/(273+temps)))))~temps,type='l',ylab='correction factor',xlab='body temperature deg C',main='5 parameter Arrhenius thermal response curve')

TempCorr<-as.numeric(exp(TA*(1/(273+T_REF)-1/(273+TC)))/(1+exp(TAL*(1/(273+TC)-1/TL))+exp(TAH*(1/TH-1/(273+TC))))) # convert Tb each hour to temperature correction factor
TempCorr_mean<-mean(TempCorr) # get mean temperature correction factor
TempCorr_mean # report value to console
getTb<-function(Tb){ # function finding the difference between a temperature correction factor for a specified Tb compared to the mean calculated one (aim to make this zero)
  x<-exp(TA*(1/(273+T_REF)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))-TempCorr_mean
}
CTE<-uniroot(f=getTb,c(TL-273,TH-273),check.conv=TRUE)$root # search for a Tb (CTE) that gives the same temperature correction factor as the mean of the simulated temperature corrections
mean(TC) # report mean Tb to screen
CTE # report constant temperature equivalent to screen

# aestivation period
TC<-subset(metout,  format(metout$dates, "%m") %in% summer)
TC<-metout$TAREF[metout$POOLDEP==0]
#TC<-TC$TAREF

# reference temp and 5 parameters for the Arrhenius response curve

T_REF<-debpars[1]-273 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-debpars[2]
TAL<-debpars[5]
TAH<-debpars[6]
TL<-debpars[3]
TH<-debpars[4]

temps<-seq(0,50,.25) # dummy temps for demo plot of response curve
plot(as.numeric(exp(TA*(1/(273+T_REF)-1/(273+temps)))/(1+exp(TAL*(1/(273+temps)-1/TL))+exp(TAH*(1/TH-1/(273+temps)))))~temps,type='l',ylab='correction factor',xlab='body temperature deg C',main='5 parameter Arrhenius thermal response curve')

TempCorr<-as.numeric(exp(TA*(1/(273+T_REF)-1/(273+TC)))/(1+exp(TAL*(1/(273+TC)-1/TL))+exp(TAH*(1/TH-1/(273+TC))))) # convert Tb each hour to temperature correction factor
TempCorr_mean<-mean(TempCorr) # get mean temperature correction factor
TempCorr_mean # report value to console
getTb<-function(Tb){ # function finding the difference between a temperature correction factor for a specified Tb compared to the mean calculated one (aim to make this zero)
  x<-exp(TA*(1/(273+T_REF)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))-TempCorr_mean
}
CTE<-uniroot(f=getTb,c(TL-273,TH-273),check.conv=TRUE)$root # search for a Tb (CTE) that gives the same temperature correction factor as the mean of the simulated temperature corrections
mean(TC) # report mean Tb to screen
CTE # report constant temperature equivalent to screen


i<-1990
startdate<-paste('01/06/',i,sep='')
enddate<-paste('15/11/',i,sep='')
dstart<-as.POSIXct(as.Date(startdate, "%d/%m/%Y"))-3600*11
dfinish<-as.POSIXct(as.Date(enddate, "%d/%m/%Y"))-3600*10
plotsoilmoist<-subset(soilmoist,  soilmoist$dates > dstart & soilmoist$dates < dfinish )
plothumid<-subset(humid,  humid$dates > dstart & humid$dates < dfinish )
plotsoilpot<-subset(soilpot,  soilpot$dates > dstart & soilpot$dates < dfinish )
plotsoil<-subset(soil,  soil$dates > dstart & soil$dates < dfinish )
plotmetout<-subset(metout,  metout$dates > dstart & metout$dates < dfinish )

plot(plotmetout$dates, plotmetout[,11],type='l',col = "black",lty=1,ylab='pond depth (mm)',xlab='date',ylim=c(0,500),main=i)

points(obs.dep$Date,obs.dep$Level,type='p',col='red')
points(obs.dep$Date,obs.dep$Level,type='l',col='red')
if(i>=2008){
  plot(plotsoil$dates, plotsoil[,6],type='l',col = "black",lty=1,ylim = c(0,80),main=i,ylab='temperature (C)',xlab='date')
  #points(obs.temp$date,obs.temp$WaterTemp,type='p',col='red')
  points(obs.temp$date,obs.temp$WaterTemp,type='l',col=addTrans("red",70))
}




debpars=as.data.frame(read.csv('DEB model/DEB_pars_Pseudemydura_umbrina.csv',header=FALSE))$V1

nicheout=ectotherm(ABSMAX = 0.866, ABSMIN = 0.866, VTMAX = 39, VTMIN = 26, write_input = 0, EMISAN = 1, gutfill = 75,
  TBASK = TBASK, TEMERGE = 8.5, ctmax = 39, ctmin = 3.5, TPREF = 33.5,  peyes = 0.03, ptcond = 0.1, skinwet = 0.2,
  shdburrow = 1, minwater = 15, maxshades = micro$MAXSHADES, mindepth = 3, raindrink = raindrink, DEB = 1, z=debpars[8], del_M=debpars[9], F_m = 13290,  
  kap_X=debpars[11],   v=debpars[13]/24, kap=debpars[14], p_M=debpars[16]/24, 
  E_G=debpars[19],   kap_R=debpars[15], k_J=debpars[18]/24, E_Hb=debpars[20],
  E_Hp=debpars[21], h_a=debpars[22]*10^-1/(24^2),   s_G=debpars[23],   E_0=debpars[24],  mu_E = mu_E,
  T_REF = debpars[1]-273, TA = debpars[2], TAL = debpars[5], TAH = debpars[6], TL = debpars[3], TH = debpars[4], 
  E_sm=186.03*6, K = 500, X = 111.7, plantsim = c(4, 4, 14, -200, -1500, 82), clutch_ab = c(0.085,0.7), viviparous = 1,
  photostart = 4, E_init = ((debpars[16]/24)*debpars[8]/debpars[14])/(debpars[13]/24), E_H_init = debpars[20]+5,  
  v_init = debpars[25]^3, stage = 1, mh = 1,  minclutch = 0, clutchsize = 2., startday = startday)
