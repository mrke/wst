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
sitemethod <- 1 # 0=specified single site long/lat, 1=place name search using geodis (needs internet)
longlat<-c(115.866852,-31.910205) # type a long/lat here in decimal degrees
loc <- "Welshpool, Perth, Western Australia" # type in a location here, used if option 1 is chosen above
timezone<-0 # if timezone=1 (needs internet), uses GNtimezone function in package geonames to correct to local time zone (excluding daylight saving correction)
rungads<-0 # use the Global Aerosol Database?
dailywind<-0 # use daily windspeed database?
terrain<-0 # include terrain (slope, aspect, horizon angles) (1) or not (0)?
soildata<-1 # include soil data for Australia (1) or not (0)?
snowmodel<-0 # run snow version? (slower!)
ystart <- 2008# start year for weather generator calibration dataset or AWAP database
yfinish <- 2013# end year for weather generator calibration dataset
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model, only for AWAP data (!!max 10 years!!)

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
DEP <- c(0., 1.5,  5.,  10.,  15.,  20.,  30.,  50.,  100.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature
Thcond <- 2.5 # soil minerals thermal conductivity (W/mC)
Density <- 2560. # soil minerals density (kg/m3)
SpecHeat <- 870. # soil minerals specific heat (J/kg-K)
BulkDensity <- 1300 # soil bulk density (kg/m3)
cap<-1 # organic cap present on soil surface? (cap has lower conductivity - 0.2 W/mC - and higher specific heat 1920 J/kg-K)
SatWater <- 0.26 # volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
Clay <- 20 # clay content for matric potential calculations (%)
SoilMoist <- 0 # fractional soil moisture (decimal %)
REFL<-0.18 # soil reflectance (decimal %)
slope<-0. # slope (degrees, range 0-90)
aspect<-180. # aspect (degrees, 0 = North, range 0-360)
hori<-rep(40,24) # enter the horizon angles (degrees) so that they go from 0 degrees azimuth (north) clockwise in 15 degree intervals
PCTWET<-0. # percentage of surface area acting as a free water surface (%)
CMH2O <- 1. # precipitable cm H2O in air column, 0.1 = VERY DRY; 1.0 = MOIST AIR CONDITIONS; 2.0 = HUMID, TROPICAL CONDITIONS (note this is for the whole atmospheric profile, not just near the ground)  
TIMAXS <- c(1.0, 1.0, 0.0, 0.0)   # Time of Maximums for Air Wind RelHum Cloud (h), air & Wind max's relative to solar noon, humidity and cloud cover max's relative to sunrise        													
TIMINS <- c(0.0, 0.0, 1.0, 1.0)   # Time of Minimums for Air Wind RelHum Cloud (h), air & Wind min's relative to sunrise, humidity and cloud cover min's relative to solar noon
minshade<-0. # minimum available shade (%)
maxshade<-90. # maximum available shade (%)
manualshade<-1 # if using soildata, which includes shade, this will override the data from the database and force max shade to be the number specified above
Usrhyt <- 100# local height (cm) at which air temperature, relative humidity and wind speed calculatinos will be made 
rainwet<-1.5 # mm rain that causes soil to become 90% wet
snowtemp<-1.5 # temperature at which precipitation falls as snow (used for snow model)
snowdens<-0.4 # snow density (mg/m3)
snowmelt<-1. # proportion of calculated snowmelt that doesn't refreeze
undercatch<-1. # undercatch multipier for converting rainfall to snow
rainmelt<-0.016 # paramter in equation that melts snow with rainfall as a function of air temp
write_input<-0 # write csv files of final input to working directory? 1=yes, 0=no.
warm<-0 # uniform warming of air temperature input to simulate climate change
loop<-0 # if doing multiple years, this shifts the starting year by the integer value

# run the model
niche<-list(loop=loop,warm=warm,rainwet=rainwet,manualshade=manualshade,dailywind=dailywind,terrain=terrain,soildata=soildata,loc=loc,ystart=ystart,yfinish=yfinish,nyears=nyears,RUF=RUF,SLE=SLE,ERR=ERR,DEP=DEP,Thcond=Thcond,Density=Density,SpecHeat=SpecHeat,BulkDensity=BulkDensity,Clay=Clay,SatWater=SatWater,SoilMoist=SoilMoist,CMH2O=CMH2O,TIMAXS=TIMAXS,TIMINS=TIMINS,minshade=minshade,maxshade=maxshade,Usrhyt=Usrhyt,REFL=REFL,slope=slope,aspect=aspect,hori=hori,rungads=rungads,cap=cap,write_input=write_input,spatial=spatial,snowmodel=snowmodel,snowtemp=snowtemp,snowdens=snowdens,snowmelt=snowmelt,undercatch=undercatch,rainmelt=rainmelt)
source('NicheMapR_Setup_micro_zoo.R')
nicheout<-NicheMapR(niche)

# get output
metout<-as.data.frame(nicheout$metout[1:(365*24*nyears),]) # above ground microclimatic conditions, min shade
plot(metout$TAREF,type='l')

write.csv(metout,'metout_zoo.csv')

########################################################################


metout<-read.csv('metout_ZOO.csv')

T_REF<-20 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-17885
TAL<-50000.
TAH<-48000.
TL<-280.
TH<-303.5

T_REF<-20 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-6000
TAL<-5e+04.
TAH<-9e+04.
TL<-273
TH<-273+38


TC<-as.data.frame(exp(TA*(1/(273+T_REF)-1/(273+metout$TAREF)))/(1+exp(TAL*(1/(273+metout$TAREF)-1/TL))+exp(TAH*(1/TH-1/(273+metout$TAREF)))))

tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates<-as.data.frame(seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")) 

dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
TC<-as.data.frame(cbind(dates,TC))
colnames(TC)<-c('dates','TC')
dailyTC<-aggregate(TC$TC,by=list(substr(TC$dates,1,10)),mean)
plot(dailyTC$x)
adultsTC<-dailyTC[1:(365*3),]
hatchTC<-dailyTC[(365*3+1):(365*6),]
dailyTC_adult<-aggregate(adultsTC$x,by=list(substr(adultsTC$Group.1,6,10)),mean)
dailyTC_hatch<-aggregate(hatchTC$x,by=list(substr(hatchTC$Group.1,6,10)),mean)

# start in March, day 110

dailyTC_adult_final<-rbind(dailyTC_adult[110:365,],dailyTC_adult[1:109,])
dailyTC_hatch_final<-rbind(dailyTC_hatch[110:365,],dailyTC_hatch[1:109,])

write.csv(dailyTC_adult_final[,2],'adultTC.csv')
write.csv(dailyTC_hatch_final[,2],'hatchTC.csv')

mean(dailyTC_adult_final[,2])
mean(dailyTC_hatch_final[,2])


Tb<-20 # put your guess in here and then run the next line to see what TC that implies
exp(TA*(1/(273+T_REF)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))

