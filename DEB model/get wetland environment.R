library(NicheMapR)
load('micro_wetland.Rda')
load('micro.Rda')

longlat <- c(116.032883,-31.7567)
loc<-longlat

  ystart <- 1971# start year
  yfinish <- 2014# end year
  nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model
  # append dates
  tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
  dates<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
  dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
  dates2<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days") 
  dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years

  metout<-micro_wetland$metout
  soil<-micro_wetland$soil
  wetlandDepths<-metout[,10] # pool depth
  wetlandTemps<-soil[,4] # make water temp soil surface temp (under water)

  debpars=as.data.frame(read.csv('DEB model/DEB_pars_Pseudemydura_umbrina.csv',header=FALSE))$V1
    minshade=100
    v_init = debpars[25]^3
    stage = 1
    E_H_init = debpars[20]+5

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
  
  wetland=environ[,c(2,3,5,16,17)]
  wetland$CONDEP[wetland$CONDEP<10]<-0
  wetland$CONDEP[wetland$CONDEP>=10]<-1
T_REF<-debpars[1]-273 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-debpars[2]
TAL<-debpars[5]
TAH<-debpars[6]
TL<-debpars[3]
TH<-debpars[4]  

# temperature correction factor using basking behaviour (simulated body temp)
wetland$TC<-as.numeric(exp(TA*(1/(273+T_REF)-1/(273+wetland$TC)))/(1+exp(TAL*(1/(273+wetland$TC)-1/TL))+exp(TAH*(1/TH-1/(273+wetland$TC))))) # convert Tb each hour to temperature correction factor  
Tcorrect=aggregate(wetland$TC,by=list(wetland$CONDEP),FUN=mean)
# temperature correction factor using water temperature only
wetland$WATERTEMP<-as.numeric(exp(TA*(1/(273+T_REF)-1/(273+wetland$WATERTEMP)))/(1+exp(TAL*(1/(273+wetland$WATERTEMP)-1/TL))+exp(TAH*(1/TH-1/(273+wetland$WATERTEMP))))) # convert Tb each hour to temperature correction factor  
Tcorrect2=aggregate(wetland$WATERTEMP,by=list(wetland$CONDEP),FUN=mean)

wettimes=aggregate(wetland$CONDEP,by=list(wetland$DAY),FUN=max)
hydroperiods=aggregate(wetland$CONDEP,by=list(wetland$YEAR),FUN=sum)
hydroperiods$x=hydroperiods$x/24/30.5
mean(hydroperiods$x)

write.csv(wettimes,'DEB model/wettimes.csv')
