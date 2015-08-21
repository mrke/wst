
# R Implementation of an integration of the microclimate model of Warren Porter's Niche Mapper system 
# Michael Kearney November 2013

# This version uses the Australia Water Availability Project (AWAP) daily 5km climate
# layers for Australia for air temperature, relative humidity, rainfall and cloud cover
# and uses monthly soil moisture estimates (splined) and the Australia Soils database to
# obtain soil properties, including their change through time due to soil moisture.
# Humidity is only from 1971 onward. Cloud cover is only from 1990 onward (and is based
# on daily solar layers relative to clear sky calculations from NicheMapR).
# It also uses a global monthly soil moisture estimate from NOAA CPC Soil Moisture http://140.172.38.100/psd/thredds/catalog/Datasets/cpcsoil/catalog.html
# Aerosol attenuation can also be computed based on the Global Aerosol Data Set (GADS)
# Koepke, P., M. Hess, I. Schult, and E. P. Shettle. 1997. Global Aerosol Data Set. Max-Planck-Institut for Meteorologie, Hamburg
# by choosing the option 'rungads<-1' 

# required R packages
# raster
# sp
# ncdf
# XML
# dismo
# chron
# rgdal
# zoo
# RODBC

spatial<-"c:/Australian Environment/" # place where climate input files are kept

############## location and climatic data  ###################################
sitemethod <- 0 # 0=specified single site long/lat, 1=place name search using geodis (needs internet)
longlat <- c(116.032883,-31.7567)#c(136.9166667,-30.48333333)#c(139.152, -34.111)#c(139.152, -34.111)#c(139.35, -33.93)<- Kerr and Bull 2004 site c(sitestodo[k,1],sitestodo[k,2])#c(138.3960,-23.7915)#c(149.04945,-35.24894)#c(x[k,1],x[k,2]) # type a long/lat here in decimal degrees
loc <- "Ellenbrook Nature Reserve, Western Australia" # type in a location here, used if option 1 is chosen above
timezone<-0 # if timezone=1 (needs internet), uses GNtimezone function in package geonames to correct to local time zone (excluding daylight saving correction)
rungads<-1 # use the Global Aerosol Database?
dailywind<-1 # use daily windspeed database?
terrain<-0 # include terrain (slope, aspect, horizon angles) (1) or not (0)?
soildata<-0 # include soil data for Australia (1) or not (0)?
snowmodel<-0 # run snow version? (slower!)
ystart <- 1911# start year for weather generator calibration dataset or AWAP database
yfinish <- 2014# end year for weather generator calibration dataset
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model, only for AWAP data (!!max 10 years!!)

# 
# if(sitemethod==1){
# library(dismo)
# longlat <- geocode(loc)[1, 3:4] # assumes first geocode match is correct
# }
# prevdir<-getwd()
# setwd('x:')
# cmd<-paste("R --no-save --args ",longlat[1]," ",longlat[2]," < extract.R",sep='')
# system(cmd)
# soilpro<-read.csv('data.csv')
# FC<-(7.561+1.176*soilpro$clay-0.009843*soilpro$clay^2+0.2132*soilpro$silt)/100
# PWP<-(-1.304+1.117*soilpro$clay-0.009309*soilpro$clay^2)/100
# setwd(prevdir)
# write.table(cbind(1,longlat[1],longlat[2],soilpro), file = "soilprops.txt", sep = ",", col.names = F, qmethod = "double", append = T)
# #    
# # pre-extracted
DEP <- c(0.,2.,  5.,  10,  15,  20.,  30.,  60.,  90.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature


soilpro<-read.csv("c:/git/Tiliqua_rugosa/microclimate/soilprops.txt",header=FALSE)
colnames(soilpro)<-c('i','site','long','lat','desc','blkdens','clay','silt','sand')
soilpro<-subset(soilpro,site==1)
soilpro<-soilpro[,5:9]
soilpro[,2]<-1.4
soilpro[,3]<-90
soilpro[,4]<-0.0
soilpro[,5]<-10
#    
soil_depths<-c(2.5,7.5,22.5,45,80,150)
plot(soilpro$clay~soil_depths,ylim=c(0,100),col='red',type='l')
points(soilpro$sand~soil_depths,ylim=c(0,100),col='orange',type='l')
points(soilpro$silt~soil_depths,ylim=c(0,100),col='grey',type='l')
title(main=loc)
legend("topleft", inset=.05,
       legend=round(soilpro[1,3:5],1),bty="n", 
       horiz=TRUE, bg=NULL, cex=0.8)

DEP2<-rep(0,18)
j<-1
for(i in 1:length(DEP2)){
  if(i%%2==0){
    DEP2[i]<-DEP2[i-1]+(DEP[j]-DEP2[i-1])/2
  }else{
    DEP2[i]<-DEP[j]
    j<-j+1
  }
}
DEP2<-as.data.frame(floor(DEP2))
colnames(DEP2)<-"DEPTH"
 
par(mfrow=c(2,2))


CampNormTbl9_1<-read.csv('../micro_australia/CampNormTbl9_1.csv')
Richter<-read.csv('../micro_australia/Richter_Table1_SI.csv')
dclay<-0.001 #mm
dsilt<-0.026 #mm
dsand<-1.05 #mm
a<-(soilpro$clay/100)*log(dclay) + (soilpro$sand/100)*log(dsand) + (soilpro$silt/100)*log(dsilt)
b.1<-(((soilpro$clay/100)*log(dclay)^2+(soilpro$sand/100)*log(dsand)^2+(soilpro$silt/100)*log(dsilt)^2)-a^2)^(1/2)
dg<-exp(a)
sigma_g<-exp(b.1)
PES<-(0.5*dg^(-1/2))*-1
b<--2*PES+0.2*sigma_g
PE<-PES*(soilpro$blkdens/1.3)^(0.67*b)
KS<-0.004*(1.3/soilpro$blkdens)^(1.3*b)*exp(-6.9*soilpro$clay/100-3.7*soilpro$silt/100)
BD<-soilpro$blkdens
   

plot(KS~soil_depths,xlim=c(-1,200),ylim=c(0.000017,0.0058))
KS_spline <-spline(soil_depths,KS,n=201,xmin=0,xmax=200,method='natural')
points(KS_spline$y,col='red',type='l')
KS_spline<-as.data.frame(cbind(KS_spline$x,KS_spline$y))
colnames(KS_spline)<-c('DEPTH','VALUE')
KS<-merge(DEP2,KS_spline)
KS<-c(KS[1,2],KS[,2])
KS[KS<0.000017]<-0.000017
   
plot(PE~soil_depths,xlim=c(-1,200),ylim=c(-15,0))
PE_spline <-spline(soil_depths,PE,n=201,xmin=0,xmax=200,method='natural')
points(PE_spline$y,col='red',type='l')
PE_spline<-as.data.frame(cbind(PE_spline$x,PE_spline$y))
colnames(PE_spline)<-c('DEPTH','VALUE')
PE<-merge(DEP2,PE_spline)
PE<-c(-1*PE[1,2],-1*PE[,2])
   
plot(b~soil_depths,xlim=c(-1,200),ylim=c(2,24))
b_spline <-spline(soil_depths,b,n=201,xmin=0,xmax=200,method='natural')
points(b_spline$y,col='red',type='l')
b_spline<-as.data.frame(cbind(b_spline$x,b_spline$y))
colnames(b_spline)<-c('DEPTH','VALUE')
b<-merge(DEP2,b_spline)
BB<-c(b[1,2],b[,2])
   
plot(BD~soil_depths,xlim=c(-1,200),ylim=c(1,1.6))
BD_spline <-spline(soil_depths,BD,n=201,xmin=0,xmax=200,method='natural')
points(BD_spline$y,col='red',type='l')
BD_spline<-as.data.frame(cbind(BD_spline$x,BD_spline$y))
colnames(BD_spline)<-c('DEPTH','VALUE')
BD<-merge(DEP2,BD_spline)
BD<-c(BD[1,2],BD[,2])

par(mfrow=c(1,1))

############# microclimate model parameters ################################
EC <- 0.0167238 # Eccenricity of the earth's orbit (current value 0.0167238, ranges between 0.0034 to 0.058)
RUF <- 0.004 # Roughness height (m), , e.g. sand is 0.05, grass may be 2.0, current allowed range: 0.001 (snow) - 2.0 cm.
# Next for parameters are segmented velocity profiles due to bushes, rocks etc. on the surface, IF NO EXPERIMENTAL WIND PROFILE DATA SET ALL THESE TO ZERO!
Z01 <- 0. # Top (1st) segment roughness height(m)
Z02 <- 0. # 2nd segment roughness height(m)
ZH1 <- 0. # Top of (1st) segment, height above surface(m)
ZH2 <- 0. # 2nd segment, height above surface(m)
SLE <- 0.96 # Substrate longwave IR emissivity (decimal %), typically close to 1
ERR <- 2.0 # Integrator error for soil temperature calculations
Thcond <- 2.5 # soil minerals thermal conductivity (W/mC)
Density <- 2560. # soil minerals density (kg/m3)
SpecHeat <- 870. # soil minerals specific heat (J/kg-K)
BulkDensity <- 1400 # soil bulk density (kg/m3)
cap<-0 # organic cap present on soil surface? (cap has lower conductivity - 0.2 W/mC - and higher specific heat 1920 J/kg-K)
SatWater <- 0.26 # volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
Clay <- 20 # clay content for matric potential calculations (%)
SoilMoist <- 0 # fractional soil moisture (decimal %)
rainmult<-4.3 # rain multiplier for surface soil moisture (use to induce runoff), proportion
runmoist<-1 # run soil moisture model (0=no, 1=yes)?
SoilMoist_Init<-rep(0.3,10) # initial soil water content, m3/m3
evenrain<-1 # spread daily rainfall evenly across 24hrs (1) or one event at midnight (2)
maxpool<-500 # max depth for water pooling on the surface, mm (to account for runoff)
soiltype<-4
#for(soiltype in 1:11){
CampNormTbl9_1<-read.csv('../micro_australia/CampNormTbl9_1.csv')
fieldcap<-CampNormTbl9_1[soiltype,7] # field capacity, mm
wilting<-CampNormTbl9_1[soiltype,8]  # use value from digital atlas of Australian soils # wilting point, mm
# PE<-rep(CampNormTbl9_1[soiltype,4],19) #air entry potential J/kg 
# KS<-rep(CampNormTbl9_1[soiltype,6],19) #saturated conductivity, kg s/m3
# BB<-rep(CampNormTbl9_1[soiltype,5],19) #soil 'b' parameterPE<-rep(CampNormTbl9_1[soiltype,4],19) #air entry potential J/kg 
# PE[1:2]<-CampNormTbl9_1[7,4] #air entry potential J/kg 
# KS[1:2]<-CampNormTbl9_1[7,6] #saturated conductivity, kg s/m3
# BB[1:2]<-CampNormTbl9_1[7,5] #soil 'b' parameter
# PE[10:19]<-CampNormTbl9_1[soiltype,4] #air entry potential J/kg 
# KS[10:19]<-CampNormTbl9_1[soiltype,6] #saturated conductivity, kg s/m3
# BB[10:19]<-CampNormTbl9_1[soiltype,5] #soil 'b' parameter
BD<-rep(1.4,19)# Mg/m3, soil bulk density for soil moisture calcs
L<-c(0,0,8.18990859,7.991299442,7.796891252,7.420411664,7.059944542,6.385001059,5.768074989,4.816673431,4.0121088,1.833554792,0.946862989,0.635260544,0.804575,0.43525621,0.366052856,0,0)*10000
LAI<-0.0 # leaf area index, used to partition traspiration/evaporation from PET
REFL<-0.2 # soil reflectance (decimal %) - average for all Nick's Ellenbrook nests
slope<-0. # slope (degrees, range 0-90)
aspect<-180. # aspect (degrees, 0 = North, range 0-360)
hori<-rep(0,24) # enter the horizon angles (degrees) so that they go from 0 degrees azimuth (north) clockwise in 15 degree intervals
PCTWET<-0. # percentage of surface area acting as a free water surface (%)
CMH2O <- 1. # precipitable cm H2O in air column, 0.1 = VERY DRY; 1.0 = MOIST AIR CONDITIONS; 2.0 = HUMID, TROPICAL CONDITIONS (note this is for the whole atmospheric profile, not just near the ground)  
TIMAXS <- c(1.0, 1.0, 0.0, 0.0)   # Time of Maximums for Air Wind RelHum Cloud (h), air & Wind max's relative to solar noon, humidity and cloud cover max's relative to sunrise        													
TIMINS <- c(0.0, 0.0, 1.0, 1.0)   # Time of Minimums for Air Wind RelHum Cloud (h), air & Wind min's relative to sunrise, humidity and cloud cover min's relative to solar noon
minshade<-0. # minimum available shade (%)
maxshade<-90. # maximum available shade (%)
runshade<-1. # run the model twice, once for each shade level (1) or just for the first shade level (0)?
manualshade<-1 # if using soildata, which includes shade, this will override the data from the database and force max shade to be the number specified above
Usrhyt <- 2# local height (cm) at which air temperature, relative humidity and wind speed calculatinos will be made 
rainwet<-1.5 # mm rain that causes soil to become 90% wet
snowtemp<-1.5 # temperature at which precipitation falls as snow (used for snow model)
snowdens<-0.4 # snow density (mg/m3)
snowmelt<-1. # proportion of calculated snowmelt that doesn't refreeze
undercatch<-1. # undercatch multipier for converting rainfall to snow
rainmelt<-0.016 # paramter in equation that melts snow with rainfall as a function of air temp
write_input<-0 # write csv files of final input to working directory? 1=yes, 0=no.
warm<-0 # uniform warming of air temperature input to simulate climate change
loop<-0 # if doing multiple years, this shifts the starting year by the integer value


microdir<-'microclimate/Ellenbrook/'

for(i in 1:2){ # run model twice, once to get wetland conditions, second time without soil moisture model to get general microclimate conditions
  if(i==2){
    rainmult<-1
     PE[1:19]<-CampNormTbl9_1[soiltype,4] #air entry potential J/kg 
 KS[1:19]<-CampNormTbl9_1[soiltype,6] #saturated conductivity, kg s/m3
 BB[1:19]<-CampNormTbl9_1[soiltype,5] #soil 'b' parameter
    PE[1:9]<-CampNormTbl9_1[3,4] #air entry potential J/kg 
KS[1:9]<-CampNormTbl9_1[3,6] #saturated conductivity, kg s/m3
BB[1:9]<-CampNormTbl9_1[3,5] #soil 'b' parameter
PE[10:13]<-CampNormTbl9_1[4,4] #air entry potential J/kg 
KS[10:13]<-CampNormTbl9_1[4,6] #saturated conductivity, kg s/m3
BB[10:13]<-CampNormTbl9_1[4,5] #soil 'b' parameter

  }
  
  # run the model
  maindir<-getwd()
  setwd('/git/micro_australia/')
  niche<-list(L=L,LAI=LAI,SoilMoist_Init=SoilMoist_Init,evenrain=evenrain,runmoist=runmoist,maxpool=maxpool,PE=PE,KS=KS,BB=BB,BD=BD,loop=loop,warm=warm,rainwet=rainwet,manualshade=manualshade,dailywind=dailywind,terrain=terrain,soildata=soildata,loc=loc,ystart=ystart,yfinish=yfinish,nyears=nyears,RUF=RUF,SLE=SLE,ERR=ERR,DEP=DEP,Thcond=Thcond,Density=Density,SpecHeat=SpecHeat,BulkDensity=BulkDensity,Clay=Clay,SatWater=SatWater,SoilMoist=SoilMoist,CMH2O=CMH2O,TIMAXS=TIMAXS,TIMINS=TIMINS,minshade=minshade,maxshade=maxshade,Usrhyt=Usrhyt,REFL=REFL,slope=slope,aspect=aspect,hori=hori,rungads=rungads,cap=cap,write_input=write_input,spatial=spatial,snowmodel=snowmodel,snowtemp=snowtemp,snowdens=snowdens,snowmelt=snowmelt,undercatch=undercatch,rainmelt=rainmelt,rainmult=rainmult,runshade=runshade)
  source('NicheMapR_Setup_micro.R')
  nicheout<-NicheMapR(niche)
  setwd(maindir)
  
  if(i==1){
    dim<-nicheout$dim
    metout<-as.data.frame(nicheout$metout[1:(dim*24),]) # above ground microclimatic conditions, min shade
    soil<-as.data.frame(nicheout$soil[1:(dim*24),]) # soil temperatures, minimum shade
    write.csv(soil[,6],paste(microdir,'wetlandTemps',ystart,'_',yfinish,'.csv',sep=""))
    write.csv(metout[,10],paste(microdir,'wetlandDepths',ystart,'_',yfinish,'.csv',sep=""))
    
  }else{
    # get output
    dim<-nicheout$dim
    metout<-as.data.frame(nicheout$metout[1:(dim*24),]) # above ground microclimatic conditions, min shade
    shadmet<-as.data.frame(nicheout$shadmet[1:(dim*24),]) # above ground microclimatic conditions, max shade
    soil<-as.data.frame(nicheout$soil[1:(dim*24),]) # soil temperatures, minimum shade
    shadsoil<-as.data.frame(nicheout$shadsoil[1:(dim*24),]) # soil temperatures, maximum shade
    soilmoist<-as.data.frame(nicheout$soilmoist[1:(dim*24),]) # soil water content, minimum shade
    shadmoist<-as.data.frame(nicheout$shadmoist[1:(dim*24),]) # soil water content, maximum shade
    humid<-as.data.frame(nicheout$humid[1:(dim*24),]) # soil humidity, minimum shade
    shadhumid<-as.data.frame(nicheout$shadhumid[1:(dim*24),]) # soil humidity, maximum shade
    soilpot<-as.data.frame(nicheout$soilpot[1:(dim*24),]) # soil water potential, minimum shade
    shadpot<-as.data.frame(nicheout$shadpot[1:(dim*24),]) # soil water potential, maximum shade
    rainfall<-as.data.frame(nicheout$RAINFALL)
    MAXSHADES<-as.data.frame(nicheout$MAXSHADES)
    elev<-as.numeric(nicheout$ALTT)
    REFL<-as.numeric(nicheout$REFL)
    longlat<-as.matrix(nicheout$longlat)
    ectoin<-rbind(elev,REFL,longlat,0,0,1990,1990+nyears-1)
    
    
    write.csv(metout,paste(microdir,'metout',ystart,'_',yfinish,'.csv',sep=""))
    write.csv(soil,paste(microdir,'soil',ystart,'_',yfinish,'.csv',sep=""))
    write.csv(soilpot,paste(microdir,'soilpot',ystart,'_',yfinish,'.csv',sep=""))
    write.csv(humid,paste(microdir,'humid',ystart,'_',yfinish,'.csv',sep=""))
    write.csv(soilmoist,paste(microdir,'soilmoist',ystart,'_',yfinish,'.csv',sep=""))
    if(runshade==0){
      write.csv(metout,paste(microdir,'shadmet',ystart,'_',yfinish,'.csv',sep=""))
      write.csv(soil,paste(microdir,'shadsoil',ystart,'_',yfinish,'.csv',sep=""))
      write.csv(humid,paste(microdir,'shadhumid',ystart,'_',yfinish,'.csv',sep=""))
      write.csv(soilpot,paste(microdir,'shadpot',ystart,'_',yfinish,'.csv',sep=""))
      write.csv(soilmoist,paste(microdir,'shadmoist',ystart,'_',yfinish,'.csv',sep=""))
    }else{
      write.csv(shadmet,paste(microdir,'shadmet',ystart,'_',yfinish,'.csv',sep=""))
      write.csv(shadsoil,paste(microdir,'shadsoil',ystart,'_',yfinish,'.csv',sep=""))
      write.csv(shadhumid,paste(microdir,'shadhumid',ystart,'_',yfinish,'.csv',sep=""))
      write.csv(shadpot,paste(microdir,'shadpot',ystart,'_',yfinish,'.csv',sep=""))
      write.csv(shadmoist,paste(microdir,'shadmoist',ystart,'_',yfinish,'.csv',sep=""))
    }
    write.csv(rainfall,paste(microdir,'rainfall',ystart,'_',yfinish,'.csv',sep=""))
    write.csv(ectoin,paste(microdir,'ectoin',ystart,'_',yfinish,'.csv',sep=""))
    write.csv(DEP,paste(microdir,'DEP',ystart,'_',yfinish,'.csv',sep=""))
    write.csv(MAXSHADES,paste(microdir,'MAXSHADES',ystart,'_',yfinish,'.csv',sep=""))
  }
}



winter<-c("05","06","07","08","09","10")
summer<-c("01","02","03","04","11","12")

# wet period
TC<-subset(soil,  format(soil$dates, "%m") %in% winter)
TC<-TC$D5cm
# reference temp and 5 parameters for the Arrhenius response curve

debpars<-as.data.frame(read.csv('DEB model/Zero_variate/DEB_pars_Pseudemydura_umbrina.csv',header=FALSE))$V1
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
TC<-TC$TAREF

# reference temp and 5 parameters for the Arrhenius response curve

debpars<-as.data.frame(read.csv('DEB model/Zero_variate/DEB_pars_Pseudemydura_umbrina.csv',header=FALSE))$V1
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


  i<-2009
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




