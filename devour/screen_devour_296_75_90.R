basedir<-getwd()
workdir<-"/vlsci/VR0212/shared/NicheMapR_Working/projects/wst/"

args <- (commandArgs(TRUE))
simnum<-as.numeric(args[1])
bioregion<-as.numeric(args[2])
scenario<-args[3]
#scenario<-""

barcoo<-paste('/scratch/VR0212/bio',bioregion,'/',sep="")
load(paste(barcoo,'longlat.bin',sep=''))
longlat <- data[simnum,1:2]

numsites<-ceiling(nrow(data)/2/1000)
jstart<-numsites*(simnum-1)+1
jfinish<-numsites*(simnum-1)+numsites

if(jstart<=nrow(data)){
  
  if(jfinish>nrow(data)){
    jfinish<-nrow(data)
  }
  
  
  for(jobnum in jstart:jfinish){
    
    longlat <- data[jobnum,1:2]

#longlat<-c(139.3109, -33.888)
#longlats<-read.csv("/vlsci/VR0212/shared/NicheMapR_Working/bioregion_points_final.csv")
#longlats<-subset(longlats,RASTERVALU!=0)
#lng<-which.min(abs(longlats[,1]-longlat[1]))
#coarselong<-longlats[lng,1]
#longlats<-subset(longlats,longlats$long==longlats[lng,1])
#lt<-which.min(abs(longlats[,2]-longlat[2]))
#coarselat<-longlats[lt,2]
#bioregion<-longlats[lt,3]
#barcoo<-paste('/scratch/VR0212/bio',bioregion,'/',sep="")
#load(paste(barcoo,'longlat.bin',sep=''))
#jobnum<-as.numeric(rownames(subset(data,data$V2==coarselong & data$V3==coarselat)))
#quadrangle<-jobnum

######################### model modes ###########################################################
mac<-0 # choose mac (1) or pc (0) 
writecsv<-0 # make Fortran code write output as csv files
write_input<-0 # write csv files of final input to working directory? 1=yes, 0=no.
runshade<-1 # run the model twice, once for each shade level (1) or just for the first shade level (0)?
runmoist<-1 # run soil moisture model (0=no, 1=yes)?
snowmodel<-0 # run the snow model (0=no, 1=yes)? - note that this runs slower
basic<-0 # for use with a simplified demo script 
shore<-0 # include tide effects (if 0, an empty matrix of tide effects is created)
rungads<-1 # use the Global Aerosol Database?
#########################################################################################################

############## location and climatic data  ###################################
spatial<-"c:/Australian Environment/" # place where climate input files are kept
sitemethod <- 0 # 0=specified single site long/lat, 1=place name search using geodis (needs internet)
#longlat<-c(139.3109, -33.888) # type a long/lat here in decimal degrees
loc <- "Manjimup, Australia" # type in a location here, used if option 1 is chosen above
timezone<-0 # if timezone=1 (needs internet), uses GNtimezone function in package geonames to correct to local time zone (excluding daylight saving correction)
dailywind<-1 # use daily windspeed database?
terrain<-0 # include terrain (slope, aspect, horizon angles) (1) or not (0)?
soildata<-1 # include soil data for Australia (1) or not (0)?
ystart<-2000
yfinish<-2009
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model, only for AWAP data (!!max 10 years!!)

DEP <- c(0., 1.,  3, 5.,  10,  15,  30.,  60.,  100.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature
DEP <- c(0., 2.5, 5, 8.25,  15,  20.,  30.,  60.,  90.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature


#longlats<-read.csv("/vlsci/VR0212/shared/NicheMapR_Working/bioregion_points_final.csv")
#longlats<-subset(longlats,RASTERVALU!=0)
#lng<-which.min(abs(longlats[,1]-longlat[1]))
#coarselong<-longlats[lng,1]
#longlats<-subset(longlats,longlats$long==longlats[lng,1])
#lt<-which.min(abs(longlats[,2]-longlat[2]))
#coarselat<-longlats[lt,2]
#bioregion<-longlats[lt,3]
#barcoo<-paste('/scratch/VR0212/bio',bioregion,'/',sep="")
#load(paste(barcoo,'longlat.bin',sep=''))
#jobnum<-as.numeric(rownames(subset(data,data$V2==coarselong & data$V3==coarselat)))
quadrangle<-jobnum


############# microclimate model parameters ################################
EC <- 0.0167238 # Eccenricity of the earth's orbit (current value 0.0167238, ranges between 0.0034 to 0.058)
RUF <- 0.004 # Roughness height (m), , e.g. sand is 0.05, grass may be 2.0, current allowed range: 0.001 (snow) - 2.0 cm.
# Next for parameters are segmented velocity profiles due to bushes, rocks etc. on the surface, IF NO EXPERIMENTAL WIND PROFILE DATA SET ALL THESE TO ZERO!
Z01 <- 0. # Top (1st) segment roughness height(m)
Z02 <- 0. # 2nd segment roughness height(m)
ZH1 <- 0. # Top of (1st) segment, height above surface(m)
ZH2 <- 0. # 2nd segment, height above surface(m)
SLE <- 0.96 # Substrate longwave IR emissivity (decimal %), typically close to 1
ERR <- 2 # Integrator error for soil temperature calculations

#if(sitemethod==1){
#longlat <- geocode(loc)[1, 3:4] # assumes first geocode match is correct
#}

#longlat<-c(139.3109, -33.888)
prevdir<-getwd()
setwd('/hsm/VR0212/shared/CSIRO Soil and Landscape Grid')
x<-cbind(longlat[1],longlat[2])
library('raster')
library('rgdal')
files<-c('density/BDW_000_005_EV_N_P_AU_NAT_C_20140801.tif','density/BDW_005_015_EV_N_P_AU_NAT_C_20140801.tif','density/BDW_015_030_EV_N_P_AU_NAT_C_20140801.tif','density/BDW_030_060_EV_N_P_AU_NAT_C_20140801.tif','density/BDW_060_100_EV_N_P_AU_NAT_C_20140801.tif','density/BDW_100_200_EV_N_P_AU_NAT_C_20140801.tif')
bdw<-rep(NA,6)
for(ii in 1:6){
a<-raster(files[ii])
bdw[ii]<-extract(a,x)
rm(a)
gc()
}
files<-c('clay/CLY_000_005_EV_N_P_AU_NAT_C_20140801.tif','clay/CLY_005_015_EV_N_P_AU_NAT_C_20140801.tif','clay/CLY_015_030_EV_N_P_AU_NAT_C_20140801.tif','clay/CLY_030_060_EV_N_P_AU_NAT_C_20140801.tif','clay/CLY_060_100_EV_N_P_AU_NAT_C_20140801.tif','clay/CLY_100_200_EV_N_P_AU_NAT_C_20140801.tif')
cly<-rep(NA,6)
for(ii in 1:6){
a<-raster(files[ii])
cly[ii]<-extract(a,x)
rm(a)
gc()
}
files<-c('silt/SLT_000_005_EV_N_P_AU_NAT_C_20140801.tif','silt/SLT_005_015_EV_N_P_AU_NAT_C_20140801.tif','silt/SLT_015_030_EV_N_P_AU_NAT_C_20140801.tif','silt/SLT_030_060_EV_N_P_AU_NAT_C_20140801.tif','silt/SLT_060_100_EV_N_P_AU_NAT_C_20140801.tif','silt/SLT_100_200_EV_N_P_AU_NAT_C_20140801.tif')
slt<-rep(NA,6)
for(ii in 1:6){
a<-raster(files[ii])
slt[ii]<-extract(a,x)
rm(a)
gc()
}
files<-c('sand/SND_000_005_EV_N_P_AU_NAT_C_20140801.tif','sand/SND_005_015_EV_N_P_AU_NAT_C_20140801.tif','sand/SND_015_030_EV_N_P_AU_NAT_C_20140801.tif','sand/SND_030_060_EV_N_P_AU_NAT_C_20140801.tif','sand/SND_060_100_EV_N_P_AU_NAT_C_20140801.tif','sand/SND_100_200_EV_N_P_AU_NAT_C_20140801.tif')
snd<-rep(NA,6)
for(ii in 1:6){
a<-raster(files[ii])
snd[ii]<-extract(a,x)
rm(a)
gc()
}
setwd(prevdir)
soilpro<-as.data.frame(cbind(bdw,cly,slt,snd))
colnames(soilpro)<-c('blkdens','clay','silt','sand')
# pre-extracted
# soilpro<-read.csv(paste(workdir,"/soilprops.txt",sep=""),header=FALSE)
# colnames(soilpro)<-c('i','site','long','lat','desc','blkdens','clay','silt','sand')
# soilpro<-subset(soilpro,site==1)
# soilpro<-soilpro[,5:9]
   
soil_depths<-c(2.5,7.5,22.5,45,80,150)
#plot(soilpro$clay~soil_depths,ylim=c(0,100),col='red',type='l')
#points(soilpro$sand~soil_depths,ylim=c(0,100),col='orange',type='l')
#points(soilpro$silt~soil_depths,ylim=c(0,100),col='grey',type='l')
#title(main=loc)
#legend("topleft", inset=.05,
#       legend=round(soilpro[1,3:5],1),bty="n", 
#       horiz=TRUE, bg=NULL, cex=0.8)

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
 
#par(mfrow=c(2,2))
#par(mar = c(2,2,1,2) + 0.1) # margin spacing stuff 
#par(mar = c(5,5,5,5) + 0.1) # margin spacing stuff 

CampNormTbl9_1<-read.csv('/hsm/VR0212/shared/NicheMapR_Working/CampNormTbl9_1.csv')
Richter<-read.csv('/hsm/VR0212/shared/NicheMapR_Working/Richter_Table1_SI.csv')
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
   

#plot(KS~soil_depths,xlim=c(-1,200),ylim=c(0.000017,0.0058))
KS_spline <-spline(soil_depths,KS,n=201,xmin=0,xmax=200,method='natural')
#points(KS_spline$y,col='red',type='l')
KS_spline<-as.data.frame(cbind(KS_spline$x,KS_spline$y))
colnames(KS_spline)<-c('DEPTH','VALUE')
KS<-merge(DEP2,KS_spline)
KS<-c(KS[1,2],KS[,2])
KS[KS<0.000017]<-0.000017
   
#plot(PE~soil_depths,xlim=c(-1,200),ylim=c(-15,0))
PE_spline <-spline(soil_depths,PE,n=201,xmin=0,xmax=200,method='natural')
#points(PE_spline$y,col='red',type='l')
PE_spline<-as.data.frame(cbind(PE_spline$x,PE_spline$y))
colnames(PE_spline)<-c('DEPTH','VALUE')
PE<-merge(DEP2,PE_spline)
PE<-c(-1*PE[1,2],-1*PE[,2])
   
#plot(b~soil_depths,xlim=c(-1,200),ylim=c(2,24))
b_spline <-spline(soil_depths,b,n=201,xmin=0,xmax=200,method='natural')
#points(b_spline$y,col='red',type='l')
b_spline<-as.data.frame(cbind(b_spline$x,b_spline$y))
colnames(b_spline)<-c('DEPTH','VALUE')
b<-merge(DEP2,b_spline)
BB<-c(b[1,2],b[,2])
   
#plot(BD~soil_depths,xlim=c(-1,200),ylim=c(1,1.6))
BD_spline <-spline(soil_depths,BD,n=201,xmin=0,xmax=200,method='natural')
#points(BD_spline$y,col='red',type='l')
BD_spline<-as.data.frame(cbind(BD_spline$x,BD_spline$y))
colnames(BD_spline)<-c('DEPTH','VALUE')
BD<-merge(DEP2,BD_spline)
BD<-c(BD[1,2],BD[,2])
  
Thcond <- rep(2.5,10) # soil minerals thermal conductivity (W/mC)
Density <- rep(2560.,10) # soil minerals density (kg/m3)
SpecHeat <- 870. # soil minerals specific heat (J/kg-K)
BulkDensity <- rep(1300,10) # soil bulk density (kg/m3)
cap<-1 # organic cap present on soil surface? (cap has lower conductivity - 0.2 W/mC - and higher specific heat 1920 J/kg-K)
SatWater <- rep(0.26,10) # volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
Clay <- rep(20,10) # clay content for matric potential calculations (%)
SoilMoist <- 0 # fractional soil moisture (decimal %)
rainmult<-1 # rain multiplier for surface soil moisture (use to induce runoff), proportion
runmoist<-1 # run soil moisture model (0=no, 1=yes)?
SoilMoist_Init<-c(0.1,0.12,0.15,0.3,0.4,0.4,0.4,0.4,0.4,0.4) # initial soil water content, m3/m3
evenrain<-0 # spread daily rainfall evenly across 24hrs (1) or one event at midnight (0)
maxpool<-10000#6 # max depth for water pooling on the surface, mm (to account for runoff)
L<-c(0,0,rep(4,9),1.8,0.95,0.85,0.8,0.4,0.366,0,0)*10000
L<-c(0,0,8.18990859,7.991299442,7.796891252,7.420411664,7.059944542,6.385001059,5.768074989,4.816673431,4.0121088,1.833554792,0.946862989,0.635260544,0.804575,0.43525621,0.366052856,0,0)*10000
LAI<-0.1 # leaf area index, used to partition traspiration/evaporation from PET
soiltype<-10
CampNormTbl9_1<-read.csv('CampNormTbl9_1.csv')
fieldcap<-CampNormTbl9_1[soiltype,7] # field capacity, mm
wilting<-CampNormTbl9_1[soiltype,8]  # use value from digital atlas of Australian soils # wilting point, mm
PE<-rep(CampNormTbl9_1[soiltype,4],19) #air entry potential J/kg 
KS<-rep(CampNormTbl9_1[soiltype,6],19) #saturated conductivity, kg s/m3
BB<-rep(CampNormTbl9_1[soiltype,5],19) #soil 'b' parameterPE<-rep(CampNormTbl9_1[soiltype,4],19) #air entry potential J/kg 
PE[1:9]<-CampNormTbl9_1[3,4] #air entry potential J/kg 
KS[1:9]<-CampNormTbl9_1[3,6] #saturated conductivity, kg s/m3
BB[1:9]<-CampNormTbl9_1[3,5] #soil 'b' parameter
PE[10:19]<-CampNormTbl9_1[4,4] #air entry potential J/kg 
KS[10:19]<-CampNormTbl9_1[4,6] #saturated conductivity, kg s/m3
BB[10:19]<-CampNormTbl9_1[4,5] #soil 'b' parameter
REFL<-0.2 # soil reflectance (decimal %)
slope<-0. # slope (degrees, range 0-90)
aspect<-180. # aspect (degrees, 0 = North, range 0-360)
hori<-rep(0,24) # enter the horizon angles (degrees) so that they go from 0 degrees azimuth (north) clockwise in 15 degree intervals
PCTWET<-0 # percentage of surface area acting as a free water surface (%)
CMH2O <- 1. # precipitable cm H2O in air column, 0.1 = VERY DRY; 1.0 = MOIST AIR CONDITIONS; 2.0 = HUMID, TROPICAL CONDITIONS (note this is for the whole atmospheric profile, not just near the ground)  
TIMAXS <- c(1.0, 1.0, 0.0, 0.0)   # Time of Maximums for Air Wind RelHum Cloud (h), air & Wind max's relative to solar noon, humidity and cloud cover max's relative to sunrise          												
TIMINS <- c(0.0, 0.0, 1.0, 1.0)   # Time of Minimums for Air Wind RelHum Cloud (h), air & Wind min's relative to sunrise, humidity and cloud cover min's relative to solar noon
minshade<-75. # minimum available shade (%)
maxshade<-90. # maximum available shade (%)
runshade<-1. # run the model twice, once for each shade level (1) or just for the first shade level (0)?
manualshade<-1 # if using soildata, which includes shade, this will override the data from the database and force max shade to be the number specified above
Usrhyt <- 3# local height (cm) at which air temperature, relative humidity and wind speed calculatinos will be made 
rainwet<-1.5 # mm rain that causes soil to become 90% wet
snowtemp<-1.5 # temperature at which precipitation falls as snow (used for snow model)
snowdens<-0.325 # snow density (mg/m3)
densfun<-c(0.001369,0.1095) # slope and intercept of linear model of snow density as a function of day of year - if it is c(0,0) then fixed density used
snowmelt<-0.9 # proportion of calculated snowmelt that doesn't refreeze
undercatch<-1.0 # undercatch multipier for converting rainfall to snow
rainmelt<-0.0125#85 # paramter in equation that melts snow with rainfall as a function of air temp, start with 0.0125
warm<-0 # uniform warming of air temperature input to simulate climate change
loop<-0 # if doing multiple years, this shifts the starting year by the integer value

# run the model
maindir<-getwd()
setwd("/vlsci/VR0212/shared/NicheMapR_Working/")
niche<-list(scenario=scenario,densfun=densfun,writecsv=writecsv,barcoo=barcoo,quadrangle=quadrangle,L=L,LAI=LAI,SoilMoist_Init=SoilMoist_Init,evenrain=evenrain,runmoist=runmoist,maxpool=maxpool,PE=PE,KS=KS,BB=BB,BD=BD,loop=loop,warm=warm,rainwet=rainwet,manualshade=manualshade,dailywind=dailywind,terrain=terrain,soildata=soildata,loc=loc,ystart=ystart,yfinish=yfinish,nyears=nyears,RUF=RUF,SLE=SLE,ERR=ERR,DEP=DEP,Thcond=Thcond,Density=Density,SpecHeat=SpecHeat,BulkDensity=BulkDensity,Clay=Clay,SatWater=SatWater,SoilMoist=SoilMoist,CMH2O=CMH2O,TIMAXS=TIMAXS,TIMINS=TIMINS,minshade=minshade,maxshade=maxshade,Usrhyt=Usrhyt,REFL=REFL,slope=slope,aspect=aspect,hori=hori,rungads=rungads,cap=cap,write_input=write_input,spatial=spatial,snowmodel=snowmodel,snowtemp=snowtemp,snowdens=snowdens,snowmelt=snowmelt,undercatch=undercatch,rainmelt=rainmelt,rainmult=rainmult,runshade=runshade)
source('NicheMapR_Setup_microsnow.R')
nicheout<-NicheMapR(niche)
setwd(maindir)
# get output
metout1<-nicheout$metout # above ground microclimatic conditions, min shade
shadmet1<-nicheout$shadmet # above ground microclimatic conditions, max shade
soil1<-nicheout$soil # soil temperatures, minimum shade
shadsoil1<-nicheout$shadsoil # soil temperatures, maximum shade
soilmoist1<-nicheout$soilmoist # soil water content, minimum shade
shadmoist1<-nicheout$shadmoist # soil water content, maximum shade
humid1<-nicheout$humid # soil humidity, minimum shade
shadhumid1<-nicheout$shadhumid # soil humidity, maximum shade
soilpot1<-nicheout$soilpot # soil water potential, minimum shade
shadpot1<-nicheout$shadpot # soil water potential, maximum shade
RAINFALL1<-nicheout$RAINFALL
MAXSHADES1<-nicheout$MAXSHADES
elev1<-as.numeric(nicheout$ALTT)
REFL1<-as.numeric(nicheout$REFL)
longlat1<-as.matrix(nicheout$longlat)
fieldcap1<-as.numeric(nicheout$fieldcap)
wilting1<-0.11 # soil moisture at node 3 that means no food available
longlat1<-nicheout$longlat

source("devour_JS_131115.R")

soiltemps_all <- cbind(longlat[1],longlat[2],as.data.frame(soil1),paste(longlat[1],longlat[2], sep=","),1,0)
colnames(soiltemps_all) <-  c('Longitude','Latitude','JULDAY','TIME','D0cm','D2.5cm','D5cm','D7.5cm','D10cm','D12.5cm','D15cm','D17.5cm','D20cm','D22.5cm','latlon,','siteID','TJUL')
years<-rep(seq(1,nyears),24*365)
years<-years[order(years)]
soiltemps_all$year<-years
for(i in 1:(nyears-1)){
  soiltemps<-subset(soiltemps_all,(year==i & JULDAY >= 296) | (year==i+1 & JULDAY < 296))[,1:17]
  
  TJUL<-rep(seq(1,365),24)
  TJUL<-TJUL[order(TJUL)]
  soiltemps$TJUL<-TJUL
  
  #source('devour_JS_131115.R')
  output <- devour(soiltemps,  sexfun=1, p1=29.4, k1=0.1, s1=-0.02, b1=1.31075, b2=15.13903, b3=29.19753, b4=6, b5=0.4, tsp1=0, tsp2=100, devrate='hourly', DEP=c(0.,1.5,5.,10.,15.,20.,30.,50.,100.,200.), write_devwin=F, write_sexrat=F)
  ##TSP set to 0-100% development so that CTE output is CTE for whole development period
  output$deps<-DEP
  
  start <- rep((soiltemps$TJUL [1]), each=10)
  
  dayone<-paste((ystart-1)+i,"-10-22",sep="") ##note: needs to be one day prior to when oviposition starts as "as.Date" package uses 0 as day one whereas julian days have 1 as day one
  
  start_inc <-as.Date(start, origin=dayone)
  
  
  end <-as.Date(output$hatch, origin=dayone)
  
  inc_time <- output$hatch - start
  
  startTSP<-as.Date(output$thermstarts, origin=dayone)
  
  endTSP<-as.Date(output$thermends, origin=dayone)
  
  finaloutput<- data.frame(output$lonsites, output$latsites, output$deps, output$srs, output$ctes, start_inc, end, inc_time, startTSP, endTSP,output$mort)
  
  names<-c("lonsites","latsites","deps","srs","ctes","start inc","end inc","inc time","start TSP","end TSP", "mort")
  #if(i==1){
  #  write.table(file="Test DEVOUR output_mortality5.csv", finaloutput, col.names=T, row.names=F, sep=",",append=TRUE)
  #}else{
    write.table(file=paste("devour_bioreg_",bioregion,"_ovi_",296,"_shade_20_year_",ystart+i,"_scen_",scenario,".csv",sep=""), finaloutput, col.names=F, row.names=F, sep=",",append=TRUE)
  #} 
}


soiltemps_all <- cbind(longlat[1],longlat[2],shadsoil1,paste(longlat[1],longlat[2], sep=","),1,0)
colnames(soiltemps_all) <-  c('Longitude','Latitude','JULDAY','TIME','D0cm','D2.5cm','D5cm','D7.5cm','D10cm','D12.5cm','D15cm','D17.5cm','D20cm','D22.5cm','latlon,','siteID','TJUL')
years<-rep(seq(1,nyears),24*365)
years<-years[order(years)]
soiltemps_all$year<-years
for(i in 1:(nyears-1)){
  soiltemps<-subset(soiltemps_all,(year==i & JULDAY >= 296) | (year==i+1 & JULDAY < 296))[,1:17]
  
  TJUL<-rep(seq(1,365),24)
  TJUL<-TJUL[order(TJUL)]
  soiltemps$TJUL<-TJUL
  
  #source('devour_JS_131115.R')
  output <- devour(soiltemps,  sexfun=1, p1=29.4, k1=0.1, s1=-0.02, b1=1.31075, b2=15.13903, b3=29.19753, b4=6, b5=0.4, tsp1=0, tsp2=100, devrate='hourly', DEP=c(0.,1.5,5.,10.,15.,20.,30.,50.,100.,200.), write_devwin=F, write_sexrat=F)
  ##TSP set to 0-100% development so that CTE output is CTE for whole development period
  output$deps<-DEP
  
  start <- rep((soiltemps$TJUL [1]), each=10)
  
  dayone<-paste((ystart-1)+i,"-10-22",sep="") ##note: needs to be one day prior to when oviposition starts as "as.Date" package uses 0 as day one whereas julian days have 1 as day one
  
  start_inc <-as.Date(start, origin=dayone)
  
  
  end <-as.Date(output$hatch, origin=dayone)
  
  inc_time <- output$hatch - start
  
  startTSP<-as.Date(output$thermstarts, origin=dayone)
  
  endTSP<-as.Date(output$thermends, origin=dayone)
  
  finaloutput<- data.frame(output$lonsites, output$latsites, output$deps, output$srs, output$ctes, start_inc, end, inc_time, startTSP, endTSP,output$mort)
  
  names<-c("lonsites","latsites","deps","srs","ctes","start inc","end inc","inc time","start TSP","end TSP", "mort")
  #if(i==1){
  #  write.table(file="Test DEVOUR output_mortality5.csv", finaloutput, col.names=T, row.names=F, sep=",",append=TRUE)
  #}else{
    write.table(file=paste("devour_bioreg_",bioregion,"_ovi_",296,"_shade_50_year_",ystart+i,"_scen_",scenario,".csv",sep=""), finaloutput, col.names=F, row.names=F, sep=",",append=TRUE)
  #} 
}

#write.table(yearoutput_loop, file = paste("yearoutput_loop_1b.csv",sep=""), sep = ",", col.names = F, qmethod = "double", append = T)
#write.table(yearsoutput_loop, file = paste("yearsoutput_loop_1b.csv",sep=""), sep = ",", col.names = F, qmethod = "double",append = T)

#} # end loop through pctwets
} # end loop through sites
}
