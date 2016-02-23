
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
longlat <- c(115.185222,-34.297333)#c(116.032883,-31.7567)#c(136.9166667,-30.48333333)#c(139.152, -34.111)#c(139.152, -34.111)#c(139.35, -33.93)<- Kerr and Bull 2004 site c(sitestodo[k,1],sitestodo[k,2])#c(138.3960,-23.7915)#c(149.04945,-35.24894)#c(x[k,1],x[k,2]) # type a long/lat here in decimal degrees
loc <- "Ellenbrook Nature Reserve, Western Australia" # type in a location here, used if option 1 is chosen above
timezone<-0 # if timezone=1 (needs internet), uses GNtimezone function in package geonames to correct to local time zone (excluding daylight saving correction)
rungads<-1 # use the Global Aerosol Database?
dailywind<-0 # use daily windspeed database?
terrain<-0 # include terrain (slope, aspect, horizon angles) (1) or not (0)?
soildata<-1 # include soil data for Australia (1) or not (0)?
snowmodel<-0 # run snow version? (slower!)
ystart <- 2015# start year for weather generator calibration dataset or AWAP database
yfinish <- 2015# end year for weather generator calibration dataset
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model, only for AWAP data (!!max 10 years!!)

# 
# if(sitemethod==1){
# library(dismo)
# longlat <- geocode(loc)[1, 3:4] # assumes first geocode match is correct
# }
prevdir<-getwd()
setwd('x:')
cmd<-paste("R --no-save --args ",longlat[1]," ",longlat[2]," < extract.R",sep='')
system(cmd)
soilpro<-read.csv('data.csv')
FC<-(7.561+1.176*soilpro$clay-0.009843*soilpro$clay^2+0.2132*soilpro$silt)/100
PWP<-(-1.304+1.117*soilpro$clay-0.009309*soilpro$clay^2)/100
setwd(prevdir)
# write.table(cbind(1,longlat[1],longlat[2],soilpro), file = "soilprops.txt", sep = ",", col.names = F, qmethod = "double", append = T)
# #    
# # pre-extracted
DEP <- c(0.,1.5, 2.5, 5., 8.25,  15,  20.,    60.,  90.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature


# soilpro<-read.csv("c:/git/Tiliqua_rugosa/microclimate/soilprops.txt",header=FALSE)
# colnames(soilpro)<-c('i','site','long','lat','desc','blkdens','clay','silt','sand')
# soilpro<-subset(soilpro,site==1)
# soilpro<-soilpro[,5:9]
# soilpro[,2]<-1.4
# soilpro[,3]<-90
# soilpro[,4]<-0.0
# soilpro[,5]<-10
# #    
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
Thcond <- rep(2.5,10) # soil minerals thermal conductivity (W/mC)
Density <- rep(2560.,10) # soil minerals density (kg/m3)
SpecHeat <- rep(870.,10) # soil minerals specific heat (J/kg-K)
BulkDensity <- rep(BD[1]*1000,10)
cap<-1 # organic cap present on soil surface? (cap has lower conductivity - 0.2 W/mC - and higher specific heat 1920 J/kg-K)
SatWater <- rep(0.26,10) # volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
Clay <- rep(22,10) # clay content for matric potential calculations (%)
SoilMoist <- 0 # fractional soil moisture (decimal %)
rainmult<-1 # rain multiplier for surface soil moisture (use to induce runoff), proportion
runmoist<-1 # run soil moisture model (0=no, 1=yes)?
SoilMoist_Init<-rep(0.3,10) # initial soil water content, m3/m3
evenrain<-1 # spread daily rainfall evenly across 24hrs (1) or one event at midnight (2)
maxpool<-500 # max depth for water pooling on the surface, mm (to account for runoff)
soiltype<-4
#for(soiltype in 1:11){
CampNormTbl9_1<-read.csv('../micro_australia/CampNormTbl9_1.csv')
fieldcap<-CampNormTbl9_1[soiltype,7] # field capacity, mm
wilting<-CampNormTbl9_1[soiltype,8]  # use value from digital atlas of Australian soils # wilting point, mm
PE[1:9]<-CampNormTbl9_1[3,4] #air entry potential J/kg 
KS[1:9]<-CampNormTbl9_1[3,6] #saturated conductivity, kg s/m3
BB[1:9]<-CampNormTbl9_1[3,5] #soil 'b' parameter
PE[10:13]<-CampNormTbl9_1[4,4] #air entry potential J/kg 
KS[10:13]<-CampNormTbl9_1[4,6] #saturated conductivity, kg s/m3
BB[10:13]<-CampNormTbl9_1[4,5] #soil 'b' parameter
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
densfun<-c(0.001369,0.1095) # slope and intercept of linear model of snow density as a function of day of year - if it is c(0,0) then fixed density used
snowmelt<-1. # proportion of calculated snowmelt that doesn't refreeze
undercatch<-1. # undercatch multipier for converting rainfall to snow
rainmelt<-0.016 # paramter in equation that melts snow with rainfall as a function of air temp
write_input<-0 # write csv files of final input to working directory? 1=yes, 0=no.
warm<-0 # uniform warming of air temperature input to simulate climate change
loop<-0 # if doing multiple years, this shifts the starting year by the integer value


# run the model
maindir<-getwd()
setwd('/git/micro_australia/')
niche<-list(densfun=densfun,L=L,LAI=LAI,SoilMoist_Init=SoilMoist_Init,evenrain=evenrain,runmoist=runmoist,maxpool=maxpool,PE=PE,KS=KS,BB=BB,BD=BD,loop=loop,warm=warm,rainwet=rainwet,manualshade=manualshade,dailywind=dailywind,terrain=terrain,soildata=soildata,loc=loc,ystart=ystart,yfinish=yfinish,nyears=nyears,RUF=RUF,SLE=SLE,ERR=ERR,DEP=DEP,Thcond=Thcond,Density=Density,SpecHeat=SpecHeat,BulkDensity=BulkDensity,Clay=Clay,SatWater=SatWater,SoilMoist=SoilMoist,CMH2O=CMH2O,TIMAXS=TIMAXS,TIMINS=TIMINS,minshade=minshade,maxshade=maxshade,Usrhyt=Usrhyt,REFL=REFL,slope=slope,aspect=aspect,hori=hori,rungads=rungads,cap=cap,write_input=write_input,spatial=spatial,snowmodel=snowmodel,snowtemp=snowtemp,snowdens=snowdens,snowmelt=snowmelt,undercatch=undercatch,rainmelt=rainmelt,rainmult=rainmult,runshade=runshade)
source('NicheMapR_Setup_micro.R')
nicheout<-NicheMapR(densfun=densfun,L=L,LAI=LAI,SoilMoist_Init=SoilMoist_Init,evenrain=evenrain,runmoist=runmoist,maxpool=maxpool,PE=PE,KS=KS,BB=BB,BD=BD,loop=loop,warm=warm,rainwet=rainwet,manualshade=manualshade,dailywind=dailywind,terrain=terrain,soildata=soildata,loc=loc,ystart=ystart,yfinish=yfinish,nyears=nyears,RUF=RUF,SLE=SLE,ERR=ERR,DEP=DEP,Thcond=Thcond,Density=Density,SpecHeat=SpecHeat,BulkDensity=BulkDensity,Clay=Clay,SatWater=SatWater,SoilMoist=SoilMoist,CMH2O=CMH2O,TIMAXS=TIMAXS,TIMINS=TIMINS,minshade=minshade,maxshade=maxshade,Usrhyt=Usrhyt,REFL=REFL,slope=slope,aspect=aspect,hori=hori,rungads=rungads,cap=cap,write_input=write_input,spatial=spatial,snowmodel=snowmodel,snowtemp=snowtemp,snowdens=snowdens,snowmelt=snowmelt,undercatch=undercatch,rainmelt=rainmelt,rainmult=rainmult,runshade=runshade)
setwd(maindir)

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

ectoin<-rbind(elev,REFL,longlat,1990,1990+nyears-1)
tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates<-seq(ISOdate(2015,1,1,tz=tzone)-3600*12, ISOdate(2015,11,17,tz=tzone)-3600*13, by="hours") 
dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
dates<-unique(dates)
dates2<-seq(ISOdate(2015,1,1,tz=tzone)-3600*12, ISOdate(2015,11,17,tz=tzone)-3600*13, by="days") 
dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years

metout<-cbind(dates,metout)
shadmet<-cbind(dates,shadmet)
soil<-cbind(dates,soil)
shadsoil<-cbind(dates,shadsoil)
soilmoist<-cbind(dates,soilmoist)
shadmoist<-cbind(dates,shadmoist)
humid<-cbind(dates,humid)
shadhumid<-cbind(dates,shadhumid)
soilpot<-cbind(dates,soilpot)
shadpot<-cbind(dates,shadpot)
wetlandTemps<-cbind(dates,wetlandTemps)
wetlandDepths_all<-cbind(dates,wetlandDepths_all)
rainfall2_all<-as.data.frame(cbind(dates2,rainfall_all))
colnames(rainfall2_all)<-c('dates','rainfall')

write.csv(soil,'augusta_soil.csv')
write.csv(shadsoil,'augusta_shadsoil.csv')

devour <- function(soiltemps, sexfun=c(1,2,3,4,5), p1, k1, s1, b1, b2, b3, b4, b5, tsp1, tsp2, devrate=c('hourly','daily'), DEP=c(0.,1.5,5.,10.,15.,20.,30.,50.,100.,200.), write_devwin=F, write_sexrat=F) {
  
  ####Return error if any parameter values are not specified, but no checks for correct values 
  if (p1=='NULL') {
    stop('Cannot devour without p')
    }
  if (k1=='NULL') {
    stop('Cannot devour without k')
  }
  if (s1=='NULL') {
    stop('Cannot devour without s')
  }
  if (b1=='NULL') {
    stop('Cannot devour without b1')
  }
  if (b2=='NULL') {
    stop('Cannot devour without b2')
  }
  if (b3=='NULL') {
    stop('Cannot devour without b3')
  }
  if (b4=='NULL') {
    stop('Cannot devour without b4')
  }
  if (b5=='NULL') {
    stop('Cannot devour without b5')
  }
  if (tsp1=='NULL') {
    stop('Cannot devour without tsp1')
  }
  if (tsp2=='NULL') {
    stop('Cannot devour without tsp2')
  }
  
  #######Definitions of calculated constants
  c1 <- 1/(1+(0.28*b4)+(0.72*log(1+b4)))	#value for Stephens Is. = 0.2450346601
  c2 <- 1+b4/(1+(1.5*b4)+(0.39*b4^2)) 	#value for Stephens Is. = 1.24958403
  
  soil.data <- soiltemps
  
  #######Define function used to calculate hatchling sex ratios#########
  
  sexrat <- function(soil.data) {
    
    SIMS <- length(soil.data)
    findev <- 0 #findev is total development calculated to occur during TSP
    
    for(i in 1:SIMS) {
      
      u <- ((soil.data[i]-b3)/(b3-b2))-c1
      v <- (u+exp(b4*u))/c2
      
      if (devrate== 'hourly') {
          hrat <- (b1*10^(-1*(v^2)*(1-b5+b5*v^2)))/24  #calculate hourly dev rate
          } else {
            if (devrate== 'daily') {
            hrat <- (b1*10^(-1*(v^2)*(1-b5+b5*v^2)))  #calculate daily dev rate
            } else {
              stop('Invalid devrate specified')
              }
            }
          
      if(i == 1) {
        findev[i] <- 0 + hrat
        } else {
          findev[i] <- findev[i-1] + hrat #calculate development as cumsum of dev rate
          }		
        }
    
    dev.50 <- findev[SIMS]/2 #calculate half-way point of development during TSP 
    devtemp <- soil.data[findev >= dev.50] #truncate temp values if development < half-way point
    cte <- devtemp[1] #cte corresponds to first temp at which development >= half-way point
    
    if (sexfun==1) {
        sr <- 1/(1 + (exp((1/s1)*(log(p1+k1)-log(cte+k1))))) ##JS changed arguments and missing bracket
        } else {
          if (sexfun==2) {
            sr <- 1/(1 + exp((1/s1)*(exp(p1*k1)-exp(cte*k1)))) 
            } else {
              if (sexfun==3) {
                sr <- exp(log(0.5)*((cte/p1)^(-1/s1))) 
                } else {
                  if (sexfun==4) {
                    sr <- 1-exp(log(0.5)*((cte/p1)^(1/s1)))
                    } else {  
                      if (sexfun==5) {
                        sr <- (1+(2^exp(k1)-1)*exp((1/s1)*(p1-cte)))^(-1/exp(k1))
                        #sr <- (1+(2^exp(3.6)-1)*exp((1/(0.0096))*(22.03-cte)))^(-1/exp(3.6))
                      } else {
                        stop('Invalid sexfun specified')
                      }  
                    } 
                  }
                }
              }
    combi <- data.frame(cte,sr)
  } ###end sexrat
  
  #Identify number of sites in input dataframe
  latlon <- 0
  lendf <- dim(soiltemps)[1]
  
  for( i in 1:lendf) {
    latlon[i] <- paste(soiltemps$Latitude[i], soiltemps$Longitude[i], sep="") #combine lat, lon columns	
    }
  
  enumsites <- nlevels(as.factor(latlon))		#count number of individual sites using latlon as unique identifier	
  soiltemps <- data.frame(soiltemps, latlon)	#attach latlon column to new df
  
  #Create column of sitenumbers
  siteID <- 0
  K <- 1
  
  for( i in 1:enumsites) {
    soil.t <- soiltemps[soiltemps$latlon == levels(as.factor(latlon))[i],] #create soiltemps.df where latlon column is changed to corresponding sitenumber
    lensoil.t <- dim(soil.t)[1]
      
      for(j in 1:lensoil.t) {
        siteID[K] <- i
        K <- K + 1
        }	
      }
  
  soiltemps <- data.frame(soiltemps, siteID)
  soiltemps2<-soiltemps
  soiltemps2$TJUL<-NULL
  
  #For each site calculate cumulative development at each soil node
  
  for( i in 1:enumsites) {
    
    site.data <- soiltemps[soiltemps$siteID == i,]#specify that the data is only the data for the i'th site, then loop through sites
    
    site.data2 <- soiltemps2[soiltemps2$siteID == i,]
    lenrat <- dim(site.data2)[1]
    
    devrat <- array(0, dim=c(lenrat,10)) #create development rate array
    
    soil.t <- data.frame(cbind(site.data2$D0cm, site.data2$D2.5cm, site.data2$D5cm, site.data2$D7.5cm, site.data2$D10cm, site.data2$D12.5cm , site.data2$D15cm, site.data2$D17.5cm, site.data2$D20cm, site.data2$D22.5cm)) #creation of site-specific soil temp dataframe
    
    for(j in 1:10) {
      
      T <- 1
      tdev <- 0	#initialize total development
      
      while(tdev <= 150) {	#calculate development to predicted hatching with lots of room for error
        
        u <- ((soil.t[T,j]-b3)/(b3-b2))-c1
        v <- (u+exp(b4*u))/c2
        
        if (devrate== 'hourly') {
          hrat <- (b1*10^(-1*(v^2)*(1-b5+b5*v^2)))/24  #calculate hourly dev rate
        } else {
          if (devrate=='daily') {
            hrat <- (b1*10^(-1*(v^2)*(1-b5+b5*v^2)))  #calculate daily dev rate
          } else {
            stop('Invalid devrate specified')
          }
        }
        
        if (T == 1) {
            devrat[T,j] <- 0 + hrat
              } else {
                devrat[T,j] <- devrat[T-1,j] + hrat 
              }
        
        if (T ==lenrat) {
            tdev <- 151	#exit loop if dataframe ends prior to 150% development
              } else {
                tdev <- devrat[T,j]	
              }
            T <- T+1
          }
    } 
   

    
    #Extract all column values for which development is within the TSP
    #Sort rows ascending by temperature 
    
    soil.node1 <- data.frame(site.data[devrat[,1] <= tsp2 & devrat[,1] >= tsp1,])
    soil.node1 <- soil.node1[order(soil.node1$D0cm, soil.node1$JULDAY, soil.node1$TIME),]
    
    soil.node2 <- data.frame(site.data[devrat[,2] <= tsp2 & devrat[,2] >= tsp1,])
    soil.node2 <- soil.node2[order(soil.node2$D2.5cm, soil.node2$JULDAY, soil.node2$TIME),]
    
    soil.node3 <- data.frame(site.data[devrat[,3] <= tsp2 & devrat[,3] >= tsp1,])
    soil.node3 <- soil.node3[order(soil.node3$D5cm, soil.node3$JULDAY, soil.node3$TIME),]
    
    soil.node4 <- data.frame(site.data[devrat[,4] <= tsp2 & devrat[,4] >= tsp1,])
    soil.node4 <- soil.node4[order(soil.node4$D7.5cm, soil.node4$JULDAY, soil.node4$TIME),]
    
    soil.node5 <- data.frame(site.data[devrat[,5] <= tsp2 & devrat[,5] >= tsp1,])
    soil.node5 <- soil.node5[order(soil.node5$D10cm, soil.node5$JULDAY, soil.node5$TIME),]
    
    soil.node6 <- data.frame(site.data[devrat[,6] <= tsp2 & devrat[,6] >= tsp1,])
    soil.node6 <- soil.node6[order(soil.node6$D12.5cm, soil.node6$JULDAY, soil.node6$TIME),]
    
    soil.node7 <- data.frame(site.data[devrat[,7] <= tsp2 & devrat[,7] >= tsp1,])
    soil.node7 <- soil.node7[order(soil.node7$D15cm, soil.node7$JULDAY, soil.node7$TIME),]
    
    soil.node8 <- data.frame(site.data[devrat[,8] <= tsp2 & devrat[,8] >= tsp1,])
    soil.node8 <- soil.node8[order(soil.node8$D17.5cm, soil.node8$JULDAY, soil.node8$TIME),]
    
    soil.node9 <- data.frame(site.data[devrat[,9] <= tsp2 & devrat[,9] >= tsp1,])
    soil.node9 <- soil.node9[order(soil.node9$D20cm, soil.node9$JULDAY, soil.node9$TIME),]
    
    soil.node10 <- data.frame(site.data[devrat[,10] <= tsp2 & devrat[,10] >= tsp1,])
    soil.node10 <- soil.node10[order(soil.node10$D22.5cm, soil.node10$JULDAY, soil.node10$TIME),]
    
    hatch <- array(0, dim=c(10))	#return first date on which development >= 100
    devfin <- array(0, dim=c(10))	#final predicted total development
    
    for(s in 1:10) {
      
      if (max(devrat[,s]) >= 100) {
        #hatch[s] <- site.data$JULDAY[devrat[,s] >= 100][1] # If hatching predicted, return first JULDAY where development >=100.
        hatch[s] <- site.data$TJUL[devrat[,s] >= 100][1] # If hatching predicted, return first JULDAY where development >=100.
        devfin[s] <- max(devrat[,s]) #return max development
          } else {
            hatch[s] <- NA  #if development doesn't reach 100%, return 'NA'
            devfin[s] <- max(devrat[,s])
            }
        }
    
    
  #####testing 35 degree mortality
    
  nodetemps <- data.frame(site.data$D0cm, site.data$D2.5cm, site.data$D5cm, site.data$D7.5cm, site.data$D10cm, site.data$D12.5cm, site.data$D15cm, site.data$D17.5cm, site.data$D20cm, site.data$D22.5cm)
  
  #nodedim<-dim(nodetemps)
  
  #nodemort <- array(0,dim= (nodedim))
  nodemort <- array(0,dim= c(205*24,10))
  
  
  for(n in 1:(205*24)) {
      
     if (site.data$D0cm[n]>=34) {nodemort[n,1]<-as.numeric(1)}
  }
 
  for(n in 1:(205*24)) {
    
    if (site.data$D2.5cm[n]>=34) {nodemort[n,2]<-as.numeric(1)}
  }
  
  for(n in 1:(205*24)) {
    
    if (site.data$D5cm[n]>=34) {nodemort[n,3]<-as.numeric(1)}
  }
  
  for(n in 1:(205*24)) {
    
    if (site.data$D7.5cm[n]>=34) {nodemort[n,4]<-as.numeric(1)}
  }
 
 for(n in 1:(205*24)) {
   
   if (site.data$D10cm[n]>=34) {nodemort[n,5]<-as.numeric(1)}
 }
 
 for(n in 1:(205*24)) {
   
   if (site.data$D12.5cm[n]>=34) {nodemort[n,6]<-as.numeric(1)}
 }
 
 for(n in 1:(205*24)) {
   
   if (site.data$D15cm[n]>=34) {nodemort[n,7]<-as.numeric(1)}
 }
 
 for(n in 1:(205*24)) {
   
   if (site.data$D17.5cm[n]>=34) {nodemort[n,8]<-as.numeric(1)}
 }
 
 for(n in 1:(205*24)) {
   
   if (site.data$D20cm[n]>=34) {nodemort[n,9]<-as.numeric(1)}
 }
 
 for(n in 1:(205*24)) {
   
   if (site.data$D22.5cm[n]>=34) {nodemort[n,10]<-as.numeric(1)}
 }
  
  mort <- array(0, dim=c(10))
  
  
  for (w in 1:10) {
    if #(sum(nodemort[,w])>=((length(nodemort[,1]))/66)){
    (sum(nodemort[,w])>=(205*24*0.05)){
      mort[w]<- paste ('yes')
    }else{
      mort[w]<- paste('no')
    }
  }
  
 mort<-data.frame(mort)
    
    
    ###Restart development fun using sorted temps
    ###Check: Is development predicted to reach at least upper limit of TSP? 
    ###If yes, run 'sexrat' to calculate cte and sr; if no, return 'NA.'
    
    if (max(devrat[,1]) >= tsp2) { 
      sr1 <- sexrat(soil.node1$D0cm)$sr
      cte1 <- sexrat(soil.node1$D0cm)$cte
      soil.node1 <- data.frame(soil.node1, "0cm", "T")
      } else {
        sr1 <- NA
        cte1 <- NA
        soil.node1 <- data.frame(site.data[1,], "0cm", "F")	
        }
    
    if (max(devrat[,2]) >= tsp2) {
      sr2 <- sexrat(soil.node2$D2.5cm)$sr
      cte2 <- sexrat(soil.node2$D2.5cm)$cte
      soil.node2 <- data.frame(soil.node2, "2.5cm", "T")
        } else {
          sr2 <- NA
          cte2 <- NA
          soil.node2 <- data.frame(site.data[1,], "2.5cm", "F")	
          }
    
    if (max(devrat[,3]) >= tsp2) {
      
      sr3 <- sexrat(soil.node3$D5cm)$sr
      cte3 <- sexrat(soil.node3$D5cm)$cte
      soil.node3 <- data.frame(soil.node3, "5cm", "T")
        } else {
          sr3 <- NA
          cte3 <- NA
          soil.node3 <- data.frame(site.data[1,], "5cm", "F")	
          }
    
    if (max(devrat[,4]) >= tsp2) {
      sr4 <- sexrat(soil.node4$D7.5cm)$sr
      cte4 <- sexrat(soil.node4$D7.5cm)$cte
      soil.node4 <- data.frame(soil.node4, "7.5cm", "T")
      } else {
        sr4 <- NA
        cte4 <- NA
        soil.node4 <- data.frame(site.data[1,], "7.5cm", "F")	
        }
    
    if (max(devrat[,5]) >= tsp2) {
      sr5 <- sexrat(soil.node5$D10cm)$sr
      cte5 <- sexrat(soil.node5$D10cm)$cte
      soil.node5 <- data.frame(soil.node5, "10cm", "T")
      } else {
        sr5 <- NA
        cte5 <- NA
        soil.node5 <- data.frame(site.data[1,], "10cm", "F")	
        }
    
    if (max(devrat[,6]) >= tsp2) {
      sr6 <- sexrat(soil.node6$D12.5cm)$sr
      cte6 <- sexrat(soil.node6$D12.5cm)$cte
      soil.node6 <- data.frame(soil.node6, "12.5cm", "T")
      } else {
        sr6 <- NA
        cte6 <- NA
        soil.node6 <- data.frame(site.data[1,], "12.5cm", "F")	
        }
    
    if (max(devrat[,7]) >= tsp2) {
      sr7 <- sexrat(soil.node7$D15cm)$sr
      cte7 <- sexrat(soil.node7$D15cm)$cte
      soil.node7 <- data.frame(soil.node7, "15cm", "T")
      } else {
        sr7 <- NA
        cte7 <- NA
        soil.node7 <- data.frame(site.data[1,], "15cm", "F")	
        }
    
    if (max(devrat[,8]) >= tsp2) {
      sr8 <- sexrat(soil.node8$D17.5cm)$sr
      cte8 <- sexrat(soil.node8$D17.5cm)$cte
      soil.node8 <- data.frame(soil.node8, "17.5cm", "T")
      } else {
        sr8 <- NA
        cte8 <- NA
        soil.node8 <- data.frame(site.data[1,], "17.5cm", "F")
        }
    
    if (max(devrat[,9]) >= tsp2) {
      sr9 <- sexrat(soil.node9$D20cm)$sr
      cte9 <- sexrat(soil.node9$D20cm)$cte
      soil.node9 <- data.frame(soil.node9, "20cm", "T")
      } else {
        sr9 <- NA
        cte9 <- NA
        soil.node9 <- data.frame(site.data[1,], "20cm", "F")	
        }
    
    if (max(devrat[,10]) >= tsp2) {
      sr10 <- sexrat(soil.node10$D22.5cm)$sr
      cte10 <- sexrat(soil.node10$D22.5cm)$cte
      soil.node10 <- data.frame(soil.node10, "22.5cm", "T")
      } else {
        sr10 <- NA
        cte10 <- NA
        soil.node10 <- data.frame(site.data[1,], "22.5cm", "F")	
        }
    
  thermstart1<- min(soil.node1$TJUL)
  thermstart2<- min(soil.node2$TJUL)
  thermstart3<- min(soil.node3$TJUL)
  thermstart4<- min(soil.node4$TJUL)
  thermstart5<- min(soil.node5$TJUL)
  thermstart6<- min(soil.node6$TJUL)
  thermstart7<- min(soil.node7$TJUL)
  thermstart8<- min(soil.node8$TJUL)
  thermstart9<- min(soil.node9$TJUL)
  thermstart10<- min(soil.node10$TJUL)
  
  thermend1<-max(soil.node1$TJUL)
  thermend2<-max(soil.node2$TJUL)
  thermend3<-max(soil.node3$TJUL)
  thermend4<-max(soil.node4$TJUL)
  thermend5<-max(soil.node5$TJUL)
  thermend6<-max(soil.node6$TJUL)
  thermend7<-max(soil.node7$TJUL)
  thermend8<-max(soil.node8$TJUL)
  thermend9<-max(soil.node9$TJUL)
  thermend10<-max(soil.node10$TJUL)
    
    #Devwin writes day, time, depth and temp information during TSP to 'devwin.csv' in current working directory
    
    options(warn=-1)
    
    ###INSERT creation of devwin dataframe
    
    if (write_devwin=='TRUE') {
    
      if(i==1) {
        Colname <- c("lat", "lon", "JULDAY", "TIME", "D0cm", "D2.5cm","D5cm", "D7.5cm", "D10cm", "D12.5cm", "D15cm", "D17.5cm", "D20cm", "D22.5cm", "latlon", "siteID", "dep_win", "valid_devwin")
        write.csv(file="devwin.csv", soil.node1, append=T, row.names=F, col.names= Colname)
          } else {
          write.csv(file="devwin.csv", soil.node1, append=T, col.names=F, row.names=F)
          }
    write.csv(file="devwin.csv", soil.node2, append=T, col.names=F, row.names=F)
    write.csv(file="devwin.csv", soil.node3, append=T, col.names=F, row.names=F)
    write.csv(file="devwin.csv", soil.node4, append=T, col.names=F, row.names=F)
    write.csv(file="devwin.csv", soil.node5, append=T, col.names=F, row.names=F)
    write.csv(file="devwin.csv", soil.node6, append=T, col.names=F, row.names=F)
    write.csv(file="devwin.csv", soil.node7, append=T, col.names=F, row.names=F)
    write.csv(file="devwin.csv", soil.node8, append=T, col.names=F, row.names=F)
    write.csv(file="devwin.csv", soil.node9, append=T, col.names=F, row.names=F)
    write.csv(file="devwin.csv", soil.node10, append=T, col.names=F, row.names=F)
    }
    
    if (DEP != 'NULL') {
        deps <- DEP
        } else {
          deps <- c('depth1','depth2','depth3','depth4','depth5','depth6','depth7','depth8','depth9','depth10')  
        }
    
    lonsite <- site.data[1,1]
    lonsites <- rep(lonsite,10)
    latsite <- site.data[1,2]
    latsites <- rep(latsite,10)
    
    srs <- c(sr1,sr2,sr3,sr4,sr5,sr6,sr7,sr8,sr9,sr10)
    ctes <- c(cte1,cte2,cte3,cte4,cte5,cte6,cte7,cte8,cte9,cte10)
  thermstarts<- c(thermstart1,thermstart2,thermstart3,thermstart4,thermstart5,thermstart6,thermstart7,thermstart8,thermstart9,thermstart10)
  thermends<- c(thermend1,thermend2,thermend3,thermend4,thermend5,thermend6,thermend7,thermend8,thermend9,thermend10)
  
    outdat <- data.frame(lonsites, latsites, deps, srs, ctes, hatch, devfin, thermstarts, thermends,mort)
    
    if (i == 1) {
      sexrat_out <- outdat
      } else {
        sexrat_out <- rbind(sexrat_out,outdat)
      }
 

    
    #Sexrat writes siteID, cte, sex ratios, soil depths, and hatch dates for all sites to 'sexrat.csv' in current working directory
    
    if (write_sexrat == 'TRUE') {
    
      if(i == 1) {
        COLnam <- c("lat", "lon", "depth", "sr", "cte", "hatchdat", "devfin", "thermstarts", "thermends", "mort")
        write.csv(file="sexrat.csv", sexrat_out, append=T, sep=",", row.names=F, col.names=COLnam) #writes file header ## JS changed outdat to sexrat_out
          } else {
            write.csv(file="sexrat.csv", sexrat_out, append=T, col.names=F, row.names=F, sep=",") #writes row data		## JS changed outdat to sexrat_out
            }	
      }
    }
  
  
  
  
  options(warn=1)
  return(sexrat_out)
}  #####end devour


#load('soil.rda')
#longlat <- c(116.032883,-31.7567)#c(136.9166667,-30.48333333)#c(139.152, -34.111)#c(139.152, -34.111)#c(139.35, -33.93)<- Kerr and Bull 2004 site c(sitestodo[k,1],sitestodo[k,2])#c(138.3960,-23.7915)#c(149.04945,-35.24894)#c(x[k,1],x[k,2]) # type a long/lat here in decimal degrees
soiltemps_all <- cbind(longlat[1],longlat[2],soil,paste(longlat[1],longlat[2], sep=","),1,0)
colnames(soiltemps_all) <-  c('Longitude','Latitude','JULDAY','TIME','D0cm','D2.5cm','D5cm','D7.5cm','D10cm','D12.5cm','D15cm','D17.5cm','D20cm','D22.5cm','latlon,','siteID','TJUL')
years<-rep(seq(1,20),24*365)
years<-years[order(years)]
soiltemps_all$year<-years
for(i in 1:19){
  soiltemps<-subset(soiltemps_all,(year==i & JULDAY >= 296) | (year==i+1 & JULDAY < 296))[,1:17]
  
  TJUL<-rep(seq(1,365),24)
  TJUL<-TJUL[order(TJUL)]
  soiltemps$TJUL<-TJUL
  
  #source('devour_JS_131115.R')
  output <- devour(soiltemps,  sexfun=1, p1=29.4, k1=0.1, s1=-0.02, b1=1.31075, b2=15.13903, b3=29.19753, b4=6, b5=0.4, tsp1=0, tsp2=100, devrate='hourly', DEP=c(0.,1.5,5.,10.,15.,20.,30.,50.,100.,200.), write_devwin=F, write_sexrat=F)
  ##TSP set to 0-100% development so that CTE output is CTE for whole development period
  output$deps<-DEP
  
  start <- rep((soiltemps$TJUL [1]), each=10)
  
  dayone<-paste(1989+i,"-10-22",sep="") ##note: needs to be one day prior to when oviposition starts as "as.Date" package uses 0 as day one whereas julian days have 1 as day one
  
  start_inc <-as.Date(start, origin=dayone)
  
  
  end <-as.Date(output$hatch, origin=dayone)
  
  inc_time <- output$hatch - start
  
  startTSP<-as.Date(output$thermstarts, origin=dayone)
  
  endTSP<-as.Date(output$thermends, origin=dayone)
  
  finaloutput<- data.frame(output$lonsites, output$latsites, output$deps, output$srs, output$ctes, start_inc, end, inc_time, startTSP, endTSP,output$mort)
  
  names<-c("lonsites","latsites","deps","srs","ctes","start inc","end inc","inc time","start TSP","end TSP", "mort")
  if(i==1){
    write.table(file="Test DEVOUR output_mortality5.csv", finaloutput, col.names=F, row.names=F, sep=",",append=TRUE)
  }else{
    write.table(file="Test DEVOUR output_mortality5.csv", finaloutput, col.names=F, row.names=F, sep=",",append=TRUE)
  } 
}

