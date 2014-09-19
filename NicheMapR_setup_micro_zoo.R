NicheMapR <- function(niche) {
  unlist(niche)
  
  errors<-0
  
  # error trapping - originally inside the Fortran code, but now checking before executing Fortran
  if(sitemethod%in%c(0,1)==FALSE){
    cat("ERROR: the variable 'sitemethod' be either 0 or 1.
        Please correct.", '\n')
    errors<-1
  }
  if(longlat[1]>180 | longlat[2] > 90){
    cat("ERROR: Latitude or longitude (longlat) is out of bounds.
        Please enter a correct value.", '\n')
    errors<-1
  }
  if(timezone%in%c(0,1)==FALSE){
    cat("ERROR: the variable 'timezone' be either 0 or 1.
        Please correct.", '\n')
    errors<-1
  }
  if(rungads%in%c(0,1)==FALSE){
    cat("ERROR: the variable 'rungads' be either 0 or 1.
        Please correct.", '\n')
    errors<-1
  }
  if(EC<0.0034 | EC > 0.058){
    cat("ERROR: the eccentricity variable (EC) is out of bounds.
        Please enter a correct value (0.0034 - 0.058).", '\n')
    errors<-1
  }
  if(RUF<0.0001){
    cat("ERROR: The roughness height (RUF) is too small ( < 0.0001).
        Please enter a larger value.", '\n')
    errors<-1
  }
  if(RUF>2){
    cat("ERROR: The roughness height (RUF) is too large ( > 2).
        Please enter a smaller value.", '\n')
    errors<-1
  }
  if(DEP[1]!=0){
    cat("ERROR: First soil node (DEP[1]) must = 0 cm.
        Please correct", '\n')
    errors<-1
  }
  if(length(DEP)!=10){
    cat("ERROR: You must enter 10 different soil depths.", '\n')
    errors<-1
  }
  for(i in 1:9){
    if(DEP[i+1]<=DEP[i]){
      cat("ERROR: Soil depth (DEP array) is not in ascending size", '\n')
      errors<-1
    }
  }
  if(DEP[10]>500){
    cat("ERROR: Deepest soil depth (DEP array) is too large (<=500 cm)", '\n')
    errors<-1
  }  
  if(Thcond<0){
    cat("ERROR: Thermal variable conductivity (THCOND) is negative.
        Please input a positive value.", '\n')
    errors<-1
  }
  if(Density<0){
    cat("ERROR: Density variable (Density) is negative.
        Please input a positive value.", '\n')
    errors<-1
  }
  if(SpecHeat<0){
    cat("ERROR: Specific heat variable (SpecHeat) is negative.
        Please input a positive value.", '\n')
    errors<-1
  }
  if(BulkDensity<0){
    cat("ERROR: Bulk density value (BulkDensity) is negative.
        Please input a positive value.", '\n')
    errors<-1
  }
  if(Clay<0){
    cat("ERROR: Clay density value (Clay) is negative.
        Please input a positive value.", '\n')
    errors<-1
  }
  if(SoilMoist<0 | SoilMoist >1){
    cat("ERROR: Soil moisture value (SoilMoist) is out of bounds.
        Please input a value between 0 and 1.", '\n')
    errors<-1
  }
  if(REFL<0 | REFL>1){
    cat("ERROR: Soil reflectivity value (REFL) is out of bounds.
        Please input a value between 0 and 1.", '\n')
    errors<-1
  }
  if(slope<0 | slope>90){
    cat("ERROR: Slope value (slope) is out of bounds.
        Please input a value between 0 and 90.", '\n')
    errors<-1
  }
  if(aspect<0 | aspect>365){
    cat("ERROR: Aspect value (aspect) is out of bounds.
        Please input a value between 0 and 365.", '\n')
    errors<-1
  }
  if(max(hori)>90 | min(hori)<0){
    cat("ERROR: At least one of your horizon angles (hori) is out of bounds.
        Please input a value between 0 and 90", '\n')
    errors<-1
  }
  if(length(hori)!=24){
    cat("ERROR: You must enter 24 horizon angle values.", '\n')
    errors<-1
  }
  if(SLE<0.5 | SLE > 1){
    cat("ERROR: Emissivity (SLE) is out of bounds.
        Please enter a correct value (0.05 - 1.00).", '\n')
    errors<-1
  }
  if(ERR<0){
    cat("ERROR: Error bound (ERR) is too small.
        Please enter a correct value (> 0.00).", '\n')
    errors<-1
  }
  if(Usrhyt<RUF){
    cat("ERROR: Reference height (Usrhyt) smaller than roughness height (RUF).
        Please use a larger height above the surface.", '\n')
    errors<-1
  }
  if(Usrhyt<0.5 | Usrhyt>120){
    cat("ERROR: Reference height (Usrhyt) is out of bounds.
        Please enter a correct value (0.05 - 120).", '\n')
    errors<-1
  }
  if(CMH2O<0.5 | CMH2O>120){
    cat("ERROR: Preciptable water in air column (CMH2O) is out of bounds.
        Please enter a correct value (0.1 - 2).", '\n')
    errors<-1
  }
  if(max(TIMAXS)>24 | min(TIMAXS)<0){
    cat("ERROR: At least one of your times of weather maxima (TIMAXS) is out of bounds.
        Please input a value between 0 and 24", '\n')
    errors<-1
  }
  if(max(TIMINS)>24 | min(TIMINS)<0){
    cat("ERROR: At least one of your times of weather minima (TIMINS) is out of bounds.
        Please input a value between 0 and 24", '\n')
    errors<-1
  }
  if(minshade>maxshade | minshade==maxshade){
    cat("ERROR: Your value for minimum shade (minshade) is greater than or equal to the maximum shade (maxshade).
        Please correct this.", '\n')
    errors<-1
  }  
  if(minshade>100 | minshade<0){
    cat("ERROR: Your value for minimum shade (minshade) is out of bounds.
        Please input a value between 0 and 100.", '\n')
    errors<-1
  }    
  if(maxshade>100 | maxshade<0){
    cat("ERROR: Your value for maximum shade (maxshade) is out of bounds.
        Please input a value between 0 and 100.", '\n')
    errors<-1
  }   
  if(write_input%in%c(0,1)==FALSE){
    cat("ERROR: the variable 'write_input' be either 0 or 1.
        Please correct.", '\n')
    errors<-1
  }
  # end error trapping
  if(errors==0){ # continue
    
    ################## loading packages ###################################
    if(require("zoo")){
      print("zoo is loaded correctly")
    } else {
      print("trying to install zoo")
      install.packages("zoo")
      if(require(zoo)){
        print("zoo installed and loaded")
      } else {
        stop("could not install zoo")
      }
    }
    if(require("ncdf")){
      print("ncdf is loaded correctly")
    } else {
      print("trying to install ncdf")
      install.packages("ncdf")
      if(require(ncdf)){
        print("ncdf installed and loaded")
      } else {
        stop("could not install ncdf")
      }
    }
    
    if(require("XML")){
      print("XML is loaded correctly")
    } else {
      print("trying to install XML")
      install.packages("XML")
      if(require(XML)){
        print("XML installed and loaded")
      } else {
        stop("could not install XML")
      }
    }
    
    if(require("dismo")){
      print("dismo is loaded correctly")
    } else {
      print("trying to install dismo")
      install.packages("dismo")
      if(require(dismo)){
        print("dismo installed and loaded")
      } else {
        stop("could not install dismo")
      }
    }
    
    if(require("chron")){
      print("chron is loaded correctly")
    } else {
      print("trying to install chron")
      install.packages("chron")
      if(require(chron)){
        print("chron installed and loaded")
      } else {
        stop("could not install chron")
      }
    }
    
    if(require("rgdal")){
      print("rgdal is loaded correctly")
    } else {
      print("trying to install rgdal")
      install.packages("rgdal")
      if(require(rgdal)){
        print("rgdal installed and loaded")
      } else {
        stop("could not install rgdal")
      }
    }
    
    if(require("RODBC")){
      print("RODBC is loaded correctly")
    } else {
      print("trying to install RODBC")
      install.packages("RODBC")
      if(require(RODBC)){
        print("RODBC installed and loaded")
      } else {
        stop("could not install RODBC")
      }
    }
    ################## end loading packages ###################################
    
    timeinterval<-365 # number of time intervals to generate predictions for over a year (must be 12 <= x <=365)
    juldays12<-c(15.,46.,74.,105.,135.,166.,196.,227.,258.,288.,319.,349.)
    juldaysn<-juldays12
    if(nyears>1){ # create sequence of days for splining across multiple years
      for(i in 1:(nyears-1)){
        juldaysn<-c(juldaysn,(juldays12+365*i))
      }
    }
    daystart<-1
    dates<-Sys.time()-60*60*24
    curyear<-as.numeric(format(dates,"%Y"))
    REFL<-rep(REFL,timeinterval*nyears) # soil reflectances
    SoilMoist<-rep(SoilMoist,timeinterval*nyears)
    Density<-Density/1000 # density of minerals - convert to Mg/m3
    BulkDensity<-BulkDensity/1000 # density of minerals - convert to Mg/m3
    if(soildata==0){
      soilprop<-cbind(0,0)
      maxshades <- rep(maxshade,365*nyears)
      minshades <- rep(minshade,365*nyears)
    }
    pctwet_mult<-0#0.01 # factor by which uppper soil wetness is multiplied to get surface %wet for evaporative cooling
    
    adiab_cor<-1 # correct for lapse rate
    microdaily<-1 # run microclimate model where one iteration of each day occurs and last day gives initial conditions for present day
    if(sitemethod==1){
      longlat <- geocode(loc)[1, 3:4] # assumes first geocode match is correct
    }
    x <- rbind(longlat) # get long/lat in a form usable by the geocode and extract function
    
    # now extract terrain and soil data from grids
    f1 <- paste(spatial,"ausclim_rowids.nc",sep="");
    f2 <- paste(spatial,"ausdem_full.nc",sep="");
    f3 <- paste(spatial,"agg_9secdem.nc",sep="");
    f4 <- paste(spatial,"Aust9secDEM.tif",sep="");
    if(soildata==1){
      cat("extracting soil data", '\n')  
      static_soil<-paste(spatial,"static_soil.nc",sep="")
      emissivities<-paste(spatial,"aus_emissivities.nc",sep="")
      # read data in from netcdf file
      static_soil_data<-brick(static_soil) 
      static_soil_vars <- extract(static_soil_data,x)
      labels<-c('albedo','FAPAR1','FAPAR2','FAPAR3','FAPAR4','FAPAR5','FAPAR6','FAPAR7','FAPAR8','FAPAR9','FAPAR10','FAPAR11','FAPAR12','volwater_Upper','volwater_lower','thick_upper','thick_lower','code')
      colnames(static_soil_vars)<-labels  
      emissivities_data<-brick(emissivities) 
      SLES2 <- extract(emissivities_data,x)
      
      # read in other soil related files for working out lumped soil type and properties
      # such as clay % for getting water potential
      filename<-paste(spatial,"ppfInterpAll.txt",sep="")
      ppf<-as.data.frame(read.table(file = filename, sep = ",", header=TRUE))
      filename<-paste(spatial,"Lumped soil types.txt",sep="")
      lumped.soil<-as.data.frame(read.table(file = filename, sep = ","))
      filename<-paste(spatial,"SoilTypeLUT_725_AWAP.csv",sep="")
      soiltype<-as.data.frame(read.table(file = filename, sep = ","))
      
      soilcode<-subset(soiltype, soiltype[1]==static_soil_vars[18])
      lumped<-subset(lumped.soil, V4==as.character(soilcode[1,2]))
      soiltype<-lumped[1,6]
      soilprop<-subset(ppf, ppf==soilcode[1,2]) 
    }else{
      SLES2 <- rep(SLE,timeinterval*nyears)
      if(manualshade==0){
        cat("extracting shade data", '\n')  
        static_soil<-paste(spatial,"static_soil.nc",sep="")
        emissivities<-paste(spatial,"aus_emissivities.nc",sep="")
        # read data in from netcdf file
        static_soil_data<-brick(static_soil) 
        static_soil_vars <- extract(static_soil_data,x)
        labels<-c('albedo','FAPAR1','FAPAR2','FAPAR3','FAPAR4','FAPAR5','FAPAR6','FAPAR7','FAPAR8','FAPAR9','FAPAR10','FAPAR11','FAPAR12','volwater_Upper','volwater_lower','thick_upper','thick_lower','code')
        colnames(static_soil_vars)<-labels 
      }
    }
    if(terrain==1){
      cat("extracting terrain data")
      for(i in 1:24){
        horifile<-paste(spatial,'horizon',i,'.tif',sep="")
        horiz<-raster(horifile)
        if(i==1){
          horizons_data<-horiz
        }else{
          horizons_data<-stack(horizons_data,raster(horifile))
        }
      }
      HORIZONS <- t(extract(horizons_data,x))
      elevslpasp<-stack(paste(spatial,'elev.tif',sep=""),paste(spatial,'slope.tif',sep=""),paste(spatial,'aspect.tif',sep=""))
      ELEVSLPASP <- extract(elevslpasp,x)
      ELEVSLPASP<-as.matrix((ifelse(is.na(ELEVSLPASP),0,ELEVSLPASP)))
      ALTITUDES <- ELEVSLPASP[,1]
      SLOPES <- ELEVSLPASP[,2]
      AZMUTHS <- ELEVSLPASP[,3]
      # the horizons have been arranged so that they go from 0 degrees azimuth (north) clockwise - r.horizon starts
      # in the east and goes counter clockwise!
      HORIZONS <- (ifelse(is.na(HORIZONS),0,HORIZONS))/10 # get rid of na and get back to floating point
      HORIZONS <- data.frame(HORIZONS)
      VIEWF_all <- 1-rowSums(sin(t(HORIZONS)*pi/180))/length(t(HORIZONS)) # convert horizon angles to radians and calc view factor(s)
      r1 <- raster(f1)
      r2 <- raster(f2)
      r3 <- raster(f3)
      dbrow <- extract(r1, x)
      AUSDEM <- extract(r2, x)
      AGG <- extract(r3, x)
    }else{
      r1 <- raster(f1)
      r2 <- raster(f2)
      r3 <- raster(f3)
      r4 <- raster(f4)
      dbrow <- extract(r1, x)
      AUSDEM <- extract(r2, x)
      AGG <- extract(r3, x)
      ALTITUDES <- extract(r4, x)
      #ALTITUDES <- AUSDEM
      #cat("using 0.05 res DEM!")
      HORIZONS <- hori
      HORIZONS <- data.frame(HORIZONS)
      VIEWF_all <- rep(1,length(x[,1]))
      SLOPES<-rep(slope,length(x[,1]))
      AZMUTHS<-rep(aspect,length(x[,1]))
    } 
    hori<-HORIZONS
    row.names(hori)<-NULL
    hori<-as.numeric(as.matrix(hori))
    
    if(soildata==1){
      VIEWF<-VIEWF_all
      SLES<-SLES2
    }else{
      VIEWF<-VIEWF_all
    }
    
    # setting up for temperature correction using lapse rate given difference between 9sec DEM value and 0.05 deg value
    if(AUSDEM==-9999 | is.na(AUSDEM)=='TRUE'){
      delta_elev = AGG - ALTITUDES
    }else{
      delta_elev = AUSDEM - ALTITUDES
    }
    adiab_corr = delta_elev * 0.0058 # Adiabatic temperature correction for elevation (C), mean for Australian Alps
    adiab_corr_max = delta_elev * 0.0077 # Adiabatic temperature correction for elevation (C), mean for Australian Alps
    adiab_corr_min = delta_elev * 0.0039 # Adiabatic temperature correction for elevation (C), mean for Australian Alps
    
    # connect to server
    channel2 <- odbcConnect("ausclim_predecol",uid = "student", pwd = "student")
    channel <- odbcConnect("AWAPDaily",uid = "student", pwd = "student")
    
    
    # preliminary test for incomplete year, if simulation includes the present year  
    yearlist<-seq(ystart,(ystart+(nyears-1)),1)
    for(j in 1:nyears){ # start loop through years
      yeartodo<-yearlist[j]
      lat1<-x[2]-0.024
      lat2<-x[2]+0.025
      lon1<-x[1]-0.024
      lon2<-x[1]+0.025 
      query<-paste("SELECT a.latitude, a.longitude, b.*
                   FROM [AWAPDaily].[dbo].[latlon] as a
                   , [AWAPDaily].[dbo].[",yeartodo,"] as b
                   where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") 
                   order by b.day",sep="")
      output<- sqlQuery(channel,query)
      output$sol<-as.numeric(as.character(output$sol))
      #if(yeartodo==2012){
      #  output<-output[-170,]
      #}
      if(nrow(output)>365){
        # fix leap years   
        #output<-output[1:365,]
        output<-output[-60,]
      }
      if(j==1){
        results<-output
      }else{
        results<-rbind(results,output)
      } 
    }
    nyears2<-nrow(results)/365
    ndays<-nrow(results)
    juldaysn2<-juldaysn[juldaysn<=ndays]
    juldaysn2<-juldaysn[1:round(nyears2*12)]
    julnum<-ndays
    juldays<-seq(daystart,365,1)
    julday <- subset(juldays, juldays!=0)
    julday<-rep(julday,nyears)
    ida<-ndays
    idayst <- 1 # start month
    # end preliminary test for incomplete year, if simulation includes the present year 
    
    if((soildata==1 & nrow(soilprop)>0)|soildata==0){
      
      if(soildata==1){
        # get static soil data into arrays
        REFL <- static_soil_vars[,1]  # albedo/reflectances
        maxshades <- static_soil_vars[,2:13] # assuming FAPAR represents shade
        upperetasat <- static_soil_vars[,14]
        loweretasat <- static_soil_vars[,15]
        upperdep <- static_soil_vars[,16] # thickness of A horizon
        lowerdep <- static_soil_vars[,17] # thickness of B horizon
        
        #### start query of soil moisture ###
        channel <- odbcConnect("AWAPSoil",uid = "student", pwd = "student")  
        lat1<-x[2]-0.025
        lat2<-x[2]+0.0249
        lon1<-x[1]-0.025
        lon2<-x[1]+0.0249
        
        yearlist<-seq(ystart,(ystart+(nyears-1)),1)
        for(j in 1:nyears){ # start loop through years
          yeartodo<-yearlist[j]
          # syntax for query
          query<-paste("SELECT a.latitude, a.longitude, b.*
                       FROM [AWAPSoil].[dbo].[latlon] as a
                       , [AWAPSoil].[dbo].[",yeartodo,"] as b
                       where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") 
                       order by b.month",sep="")
          # exectue query
          if(j==1){
            output<- sqlQuery(channel,query)
            output<-cbind(output$latitude,output$longitude,output$id,output$month,output$WRel1,output$WRel2)
            year<-as.data.frame(rep(yearlist[j],nrow(output)))
            colnames(year)<-'rep(yearlist[j], nrow(output1))'
          }else{
            output1<-sqlQuery(channel,query)
            output1<-cbind(output1$latitude,output1$longitude,output1$id,output1$month,output1$WRel1,output1$WRel2)
            output<-rbind(output,output1)
            year<-rbind(year,as.data.frame(rep(yearlist[j],nrow(output1))))
          }
        } # end loop through years
        
        output<-cbind(year,output)
        dates<-paste("15/",output[,5],"/",output[,1],sep="")
        dates<-strptime(dates, "%d/%m/%Y") # convert to date format
        dates<-format(dates, "%d/%m/%Y")
        output<-cbind(as.Date(dates, "%d/%m/%Y"),output)
        colnames(output)<-c("date","year","latitude","longitude","id","month","WRel1","WRel2")
        
        uppermoist <- output$WRel1 
        lowermoist <- output$WRel2
        # add an extra value for soil moisture if doing current year, which will be incomplete
        if(yfinish==curyear){
          uppermoist<-c(uppermoist,uppermoist[length(uppermoist)])
          lowermoist<-c(lowermoist,lowermoist[length(lowermoist)])
          # do it again for now - Nov 2013, Oct not in yet!
          uppermoist<-c(uppermoist,uppermoist[length(uppermoist)])
          lowermoist<-c(lowermoist,lowermoist[length(lowermoist)])
        }
        moistupper<-uppermoist
        moistlower<-lowermoist
        
        #### end query of soil moisture ###
        
        shademax<-maxshades
        
      }else{
        if(manualshade==0){
          maxshades <- static_soil_vars[,2:13] # assuming FAPAR represents shade
        }
        shademax<-maxshades
      }
      
      if(is.na(dbrow)!=TRUE & is.na(ALTITUDES)!=TRUE){ 
        
        if(rungads==1){
          ####### get solar attenuation due to aerosols with program GADS #####################
          lat5s<-seq(-90,90,5) #lat range for GADS
          lon5s<-seq(-180,175,5) #long range for GADS
          lat5<-lat5s[which.min(abs(lat5s-x[2]))]
          lon5<-lon5s[which.min(abs(lon5s-x[1]))]
          relhum<-1.
          season<-0.
          gadin<-list(lat5=lat5,lon5=lon5,relhum=relhum,season=season)
          source('/git/micro_australia/gads/gads.R')
          gadout<-gads(gadin)
          optdep.summer<-as.data.frame(gadout$optdep)
          season<-1.
          gadin<-list(lat5=lat5,lon5=lon5,relhum=relhum,season=season)
          gadout<-gads(gadin)
          optdep.winter<-as.data.frame(gadout$optdep)
          optdep<-cbind(optdep.winter[,1],rowMeans(cbind(optdep.summer[,2],optdep.winter[,2])))
          optdep<-as.data.frame(optdep)
          colnames(optdep)<-c("LAMBDA","OPTDEPTH")
          a<-lm(OPTDEPTH~poly(LAMBDA, 6, raw=TRUE),data=optdep)
          LAMBDA<-c(290,295,300,305,310,315,320,330,340,350,360,370,380,390,400,420,440,460,480,500,520,540,560,580,600,620,640,660,680,700,720,740,760,780,800,820,840,860,880,900,920,940,960,980,1000,1020,1080,1100,1120,1140,1160,1180,1200,1220,1240,1260,1280,1300,1320,1380,1400,1420,1440,1460,1480,1500,1540,1580,1600,1620,1640,1660,1700,1720,1780,1800,1860,1900,1950,2000,2020,2050,2100,2120,2150,2200,2260,2300,2320,2350,2380,2400,2420,2450,2490,2500,2600,2700,2800,2900,3000,3100,3200,3300,3400,3500,3600,3700,3800,3900,4000)
          TAI<-predict(a,data.frame(LAMBDA))
          setwd('..') #getting back to working directory
          setwd('..')
          ################ end GADS ################################################## 
        }else{ # use the original profile from Elterman, L. 1970. Vertical-attenuation model with eight surface meteorological ranges 2 to 13 kilometers. U. S. Airforce Cambridge Research Laboratory, Bedford, Mass.
          TAI<-c(0.0670358341290886,0.0662612704779235,0.065497075238002,0.0647431301168489,0.0639993178022531,0.0632655219571553,0.0625416272145492,0.0611230843885423,0.0597427855962549,0.0583998423063099,0.0570933810229656,0.0558225431259535,0.0545864847111214,0.0533843764318805,0.0522154033414562,0.0499736739981675,0.047855059159556,0.0458535417401334,0.0439633201842001,0.0421788036108921,0.0404946070106968,0.0389055464934382,0.0374066345877315,0.0359930755919066,0.0346602609764008,0.0334037648376212,0.0322193394032758,0.0311029105891739,0.0300505736074963,0.0290585886265337,0.0281233764818952,0.0272415144391857,0.0264097320081524,0.0256249068083005,0.0248840604859789,0.0241843546829336,0.0235230870563317,0.0228976873502544,0.0223057135186581,0.0217448478998064,0.0212128934421699,0.0207077699817964,0.0202275105711489,0.0197702578594144,0.0193342605242809,0.0189178697551836,0.0177713140039894,0.0174187914242432,0.0170790495503944,0.0167509836728154,0.0164335684174899,0.0161258546410128,0.0158269663770596,0.0155360978343254,0.0152525104459325,0.0149755299703076,0.0147045436435285,0.0144389973831391,0.0141783930434343,0.0134220329447663,0.0131772403830191,0.0129356456025128,0.0126970313213065,0.0124612184223418,0.0122280636204822,0.01199745718102,0.0115436048739351,0.0110993711778668,0.0108808815754663,0.0106648652077878,0.0104513876347606,0.0102405315676965,0.00982708969547694,0.00962473896278535,0.00903679230300494,0.00884767454432418,0.0083031278398166,0.00796072474935954,0.00755817587626185,0.00718610751850881,0.00704629977586921,0.00684663903049612,0.00654155580333479,0.00642947339729728,0.00627223096874308,0.00603955966866779,0.00580920937536261,0.00568506186880564,0.00563167068287251,0.00556222005081865,0.00550522989971023,0.00547395763028062,0.0054478983436216,0.00541823364504573,0.00539532163908382,0.00539239864119488,0.00541690124712384,0.00551525885358836,0.00564825853509463,0.00577220185074264,0.00584222986640171,0.00581645238345584,0.00566088137411449,0.00535516862329704,0.00489914757707667,0.00432017939770409,0.0036813032251836,0.00309019064543606,0.00270890436501562,0.00276446109239711,0.00356019862584603)
        } #end check if running gads
        
        
        
        yearlist<-seq(ystart,(ystart+(nyears-1)),1)
        for(j in 1:nyears){ # start loop through years
          yeartodo<-yearlist[j]
          lat1<-x[2]-0.024
          lat2<-x[2]+0.025
          lon1<-x[1]-0.024
          lon2<-x[1]+0.025 
          query<-paste("SELECT a.latitude, a.longitude, b.*
                       FROM [AWAPDaily].[dbo].[latlon] as a
                       , [AWAPDaily].[dbo].[",yeartodo,"] as b
                       where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") 
                       order by b.day",sep="")
          output<- sqlQuery(channel,query)
          output$sol<-as.numeric(as.character(output$sol))
          
          if(nrow(output)>365){
            # fix leap years   
            output<-output[-60,]
          }
          if(j==1){
            results<-output
          }else{
            results<-rbind(results,output)
          } 
        } 
        if(dailywind==1){
          channel <- odbcConnect("dailywind",uid = "student", pwd = "student")
          for(j in 1:nyears){ # start loop through years
            yeartodo<-yearlist[j]
            lat1<-x[2]-0.024
            lat2<-x[2]+0.025
            lon1<-x[1]-0.024
            lon2<-x[1]+0.025 
            query<-paste("SELECT a.latitude, a.longitude, b.*
                         FROM [dailywind].[dbo].[latlon] as a
                         , [dailywind].[dbo].[",yeartodo,"] as b
                         where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") 
                         order by b.day",sep="")
            output<- sqlQuery(channel,query)
            
            if(nrow(output)>365){
              # fix leap years   
              output<-output[-60,]
            }
            if(j==1){
              dwind<-output
            }else{
              dwind<-rbind(dwind,output)
            } 
          } 
          dwind<-dwind$wind/15.875
        }
        
        if(adiab_cor==1){
          TMAXX<-as.matrix(results$tmax+adiab_corr_max)
          TMINN<-as.matrix(results$tmin+adiab_corr_min)
        }else{
          TMAXX<-as.matrix(results$tmax)
          TMINN<-as.matrix(results$tmin)
        }
        RAINFALL<-results$rr
        output_AWAPDaily<-results
        
        # cloud cover
        
        if(ystart>1989 & sum(results[,9],na.rm=TRUE)>0){ # solar radiation data available
          query<-paste("SELECT a.*
                       FROM [ausclim].[dbo].[clearskysol] as a
                       where (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") 
                       ",sep="")
          output_ausclim<- sqlQuery(channel,query)
          
          if(nrow(output_ausclim)==0){ #no satellite coverage, get data from ausclim
            clouds<-paste("select cloud1,cloud2,cloud3,cloud4,cloud5,cloud6,cloud7,cloud8,cloud9,cloud10,cloud11,cloud12 FROM cloudcover WHERE i = ",dbrow,sep="")
            #CCMAXX <- dbGetQuery(con,statement=clouds)*100
            if(vlsci==0){
              CCMAXX<- sqlQuery(channel2,clouds)*100
            }
            CCMINN <- CCMAXX
            CCMAXX1 <-spline(juldays12,CCMAXX,n=timeinterval,xmin=1,xmax=365,method="periodic")
            CCMAXX <- rep(CCMAXX1$y,nyears)
            CCMINN <- CCMAXX
          }else{
            weekly_sol<-cbind(1:52,t(output_ausclim[3:54]))
            daily_sol <-spline(seq(3,361,7),weekly_sol[,2],n=365,xmin=1,xmax=365,method="periodic")
            daily_sol<-as.numeric(daily_sol$y)
            daily_sol<-rep(daily_sol,nyears)
            if(is.na(output_AWAPDaily[1,9])==TRUE){
              output_AWAPDaily[1,9]=mean(output_AWAPDaily[,9],na.rm=TRUE)
            }
            if(is.na(output_AWAPDaily[7300,9])==TRUE){
              output_AWAPDaily[nrow(output_AWAPDaily),9]=mean(output_AWAPDaily[,9],na.rm=TRUE)
            }
            solar<-na.approx(output_AWAPDaily[,9])
            cloud<-(1-as.data.frame(solar)/as.data.frame(daily_sol))*100
            cloud[cloud<0]<-0
            cloud[cloud>100]<-100
            cloud<-as.matrix(cbind(output_AWAPDaily[,4],cloud))
            CCMAXX<-cloud[,2]
            CCMINN<-CCMAXX
          }
        }else{
          clouds<-paste("select cloud1,cloud2,cloud3,cloud4,cloud5,cloud6,cloud7,cloud8,cloud9,cloud10,cloud11,cloud12 FROM cloudcover WHERE i = ",dbrow,sep="")
          #CCMAXX <- dbGetQuery(con,statement=clouds)*100
          CCMAXX<- sqlQuery(channel2,clouds)*100
          CCMINN <- CCMAXX
          CCMAXX1 <-spline(juldays12,CCMAXX,n=timeinterval,xmin=1,xmax=365,method="periodic")
          CCMAXX <- rep(CCMAXX1$y,nyears)
          CCMINN <- CCMAXX
        }# end check for year 1990 or later
        if(ystart>1970){ #vapour pressure data available
          if(is.na(output_AWAPDaily[1,8])==TRUE){
            output_AWAPDaily[1,8]=mean(output_AWAPDaily[,8],na.rm=TRUE)
          }
          VAPRES<-na.approx(output_AWAPDaily[,8])
          VAPRES<-VAPRES*100 # convert from hectopascals to pascals
          TMAXK<-TMAXX+273.15
          loge<-TMAXK
          loge[loge>273.16]<- -7.90298*(373.16/TMAXK-1.)+5.02808*log10(373.16/TMAXK)-1.3816E-07*(10.^(11.344*(1.-TMAXK/373.16))-1.)+8.1328E-03*(10.^(-3.49149*(373.16/TMAXK-1.))-1.)+log10(1013.246)
          loge[loge<=273.16]<- -9.09718*(273.16/TMAXK-1.)-3.56654*log10(273.16/TMAXK)+.876793*(1.-TMAXK/273.16)+log10(6.1071)
          estar<-(10.^loge)*100. 
          RHMINN<-(VAPRES/estar)*100
          RHMINN[RHMINN>100]<-100
          RHMINN[RHMINN<0]<-0.01
          #RHMINN
          TMINK<-TMINN+273.15
          loge<-TMINK
          loge[loge>273.16]<- -7.90298*(373.16/TMINK-1.)+5.02808*log10(373.16/TMINK)-1.3816E-07*(10.^(11.344*(1.-TMINK/373.16))-1.)+8.1328E-03*(10.^(-3.49149*(373.16/TMINK-1.))-1.)+log10(1013.246)
          loge[loge<=273.16]<- -9.09718*(273.16/TMINK-1.)-3.56654*log10(273.16/TMINK)+.876793*(1.-TMINK/273.16)+log10(6.1071)
          estar<-(10.^loge)*100. 
          RHMAXX<-(VAPRES/estar)*100
          RHMAXX[RHMAXX>100]<-100
          RHMAXX[RHMAXX<0]<-0.01
        } #end check for year is 1971 or later
        
        # AUSCLIM query statements
        clouds<-paste("select cloud1,cloud2,cloud3,cloud4,cloud5,cloud6,cloud7,cloud8,cloud9,cloud10,cloud11,cloud12 FROM cloudcover WHERE i = ",dbrow,sep="")
        maxwinds<-paste("select maxwind1,maxwind2,maxwind3,maxwind4,maxwind5,maxwind6,maxwind7,maxwind8,maxwind9,maxwind10,maxwind11,maxwind12 FROM maxwind WHERE i = ",dbrow,sep="")
        minwinds<-paste("select minwind1,minwind2,minwind3,minwind4,minwind5,minwind6,minwind7,minwind8,minwind9,minwind10,minwind11,minwind12 FROM minwind WHERE i = ",dbrow,sep="")
        maxhumidities<-paste("select maxhum1,maxhum2,maxhum3,maxhum4,maxhum5,maxhum6,maxhum7,maxhum8,maxhum9,maxhum10,maxhum11,maxhum12 FROM maxhum WHERE i = ",dbrow,sep="")
        minhumidities<-paste("select minhum1,minhum2,minhum3,minhum4,minhum5,minhum6,minhum7,minhum8,minhum9,minhum10,minhum11,minhum12 FROM minhum WHERE i = ",dbrow,sep="")
        rainfall<-paste("select rainfall1,rainfall2,rainfall3,rainfall4,rainfall5,rainfall6,rainfall7,rainfall8,rainfall9,rainfall10,rainfall11,rainfall12 FROM rainfall WHERE i = ",dbrow,sep="")
        rainydays<-paste("select rainy1,rainy2,rainy3,rainy4,rainy5,rainy6,rainy7,rainy8,rainy9,rainy10,rainy11,rainy12 FROM rainydays WHERE i = ",dbrow,sep="")
        
        
        ALLMINTEMPS<-TMINN
        ALLMAXTEMPS<-TMAXX
        ALLTEMPS <- cbind(ALLMAXTEMPS,ALLMINTEMPS)
        WNMAXX <- sqlQuery(channel2,maxwinds)
        WNMINN <- sqlQuery(channel2,minwinds)  
        
        if(dailywind!=1 ){
          WNMAXX1 <-spline(juldays12,WNMAXX,n=timeinterval,xmin=1,xmax=365,method="periodic")
          WNMAXX<-rep(WNMAXX1$y,nyears) 
          WNMINN1 <-spline(juldays12,WNMINN,n=timeinterval,xmin=1,xmax=365,method="periodic")
          WNMINN<-rep(WNMINN1$y,nyears) 
        }
        
        if(soildata==1){
          uppermoist1<-spline(juldaysn2,moistupper,n=ndays,xmin=1,xmax=ndays,method="periodic")
          lowermoist1<-spline(juldaysn2,moistlower,n=ndays,xmin=1,xmax=ndays,method="periodic")
          uppermoists<-uppermoist1$y
          lowermoists<-lowermoist1$y
          SLES1<-spline(juldays12,SLES,n=timeinterval,xmin=1,xmax=365,method="periodic")
          SLES<-rep(SLES1$y,nyears)
          SLES<-SLES[1:ndays]
          maxshades1 <-spline(juldays12,shademax,n=timeinterval,xmin=1,xmax=365,method="periodic")
          MAXSHADES<-rep(maxshades1$y*100,nyears)
          MAXSHADES<-MAXSHADES[1:ndays]
          if(manualshade==1){
            maxshades <- rep(maxshade,365)
            maxshades <- rep(maxshades,nyears)
            MAXSHADES<-maxshades
            minshades <- rep(minshade,365)
            minshades <- rep(minshades,nyears)
            MINSHADES<-minshades
          }
        }else{
          if(manualshade==0){
            maxshades1 <-spline(juldays12,shademax,n=timeinterval,xmin=1,xmax=365,method="periodic")
            MAXSHADES<-rep(maxshades1$y*100,nyears)
            minshades <- rep(minshade,365)
            minshades <- rep(minshades,nyears)
            MINSHADES<-minshades
          }else{
            MAXSHADES<-maxshades
            MINSHADES<-minshades
          }
        }
        RAINYDAYS <- sqlQuery(channel2,rainydays)
        RAINYDAYS <- round(RAINYDAYS)
        
        
        
        REFLS <- (1:(timeinterval*nyears))*0+REFL
        if((soildata==1)&(length(RAINFALL)>0)){
          soilwet<-RAINFALL
          soilwet[soilwet<=rainwet] = 0 
          soilwet[soilwet>0] = 90
          #PCTWET <- uppermoists*pctwet_mult
          PCTWET <- uppermoists*soilprop$A_01bar*pctwet_mult*100
          PCTWET<-pmax(soilwet,PCTWET)
        }else{
          REFLS <- (1:(timeinterval*nyears))*0+REFL
          PCTWET <- (1:(timeinterval*nyears))*0+PCTWET
          soilwet<-RAINFALL
          soilwet[soilwet<=rainwet] = 0 
          soilwet[soilwet>0] = 90
          PCTWET<-pmax(soilwet,PCTWET)
        }
        
        
        
        
        
        if(soildata==1){
          # extra code for soil moisture start 
          Intrvls <-(1:julnum) # user-supplied last Julian day in each time interval sequence
          Numint <- julnum  # number of time intervals
          Numtyps <- 4
          depinterval<-findInterval(upperdep*100, DEP)
          deepnode1<-depinterval
          depinterval<-findInterval(lowerdep*100, DEP)
          deepnode2<-depinterval
          deepnode3<-10
          toprow<-rep(deepnode1,julnum)
          middlerow<-rep(deepnode2,julnum)
          bottomrow<-rep(deepnode3,julnum)
          Nodes <- matrix(data = 0, nrow = 10, ncol = 7300) # deepest nodes for each substrate type
          Nodes[1,1:julnum]<-3
          Nodes[2,1:julnum]<-toprow
          Nodes[3,1:julnum]<-middlerow
          Nodes[4,1:julnum]<-bottomrow
        }else{
          Intrvls<-rep(0,7300)  
          Intrvls[1] <- 1 # user-supplied last Julian day in each time interval sequence
          Numtyps <- 1 # number of substrate types
          Numint <- 1  # number of time intervals
          Nodes <- matrix(data = 0, nrow = 10, ncol = 7300) # deepest nodes for each substrate type
          Nodes[1,1] <- 10. # deepest nodes for each substrate type
        }
        
        if(timezone==1){
          if(!require(geonames)){
            stop('package "geonames" is required.')
          }
          ALREF<-(GNtimezone(longlat[2],longlat[1])[4])*-15
        }else{  
          ALREF <- abs(trunc(x[1]))
        }
        
        HEMIS <- ifelse(x[2]<0,2.,1.) 
        ALAT <- abs(trunc(x[2]))
        AMINUT <- (abs(x[2])-ALAT)*60
        ALONG <- abs(trunc(x[1]))
        ALMINT <- (abs(x[1])-ALONG)*60
        ALTT<-ALTITUDES
        SLOPE<-SLOPES
        AZMUTH<-AZMUTHS 
        
        avetemp<-(sum(TMAXX)+sum(TMINN))/(length(TMAXX)*2)
        soilinit<-rep(avetemp,length(DEP))
        tannul<-mean(unlist(ALLTEMPS))
        
        if(nyears==1){
          avetemp<-(sum(TMAXX)+sum(TMINN))/(length(TMAXX)*2)
          tannulrun<-rep(avetemp,365)
        }else{
          if(nrow(TMAXX)==1){
            avetemp<-colMeans(cbind(TMAXX, TMINN), na.rm=TRUE)
          }else{
            avetemp<-rowMeans(cbind(TMAXX, TMINN), na.rm=TRUE)
          }
          #library("TTR")
          #tannulrun<-SMA(avetemp,n=365)
          if(length(TMAXX)<365){
            tannulrun<-rep((sum(TMAXX)+sum(TMINN))/(length(TMAXX)*2),length(TMAXX))
          }else{
            tannulrun<-movingFun(avetemp,n=365,fun=mean,type='to')
            yearone<-rep((sum(TMAXX[1:365])+sum(TMINN[1:365]))/(365*2),365)
            tannulrun[1:365]<-yearone
            # SST
          }
          #            SST<-read.csv("C:/NicheMapR_Working/projects/SeaTurtles/SST.csv")
          #            SST<-subset(SST,year>=ystart & year<=yfinish)
          #            days<-seq(1,365*nyears)
          #            SST2 <-spline(SST$Day,SST$SST_D,n=timeinterval*nyears,xmin=1,xmax=365*nyears,method="periodic")
          #            #SST2 <-spline(SST$Day,SST$SST_G,n=timeinterval*nyears,xmin=1,xmax=365*nyears,method="periodic")
          #            SST2<-SST2$y
          #            window<-1
          #            SSTrun<-movingFun(SST2,n=window,fun=mean,type='to')
          #            yearone<-rep(sum(SST2[1:window]/window),window)
          #            SST2<-SSTrun
          #            SST2[1:window]<-yearone
          #tannulrun<-tannulrun+(SST2-tannulrun)
          #tannulrun<-rowMeans(cbind(tannulrun,SST2),na.rm=TRUE)
          #tannulrun<-SST2
        }
        
        
        # correct for fact that wind is measured at 10 m height
        # wind shear equation v / vo = (h / ho)^a
        #where
        #v = the velocity at height h (m/s)
        #vo = the velocity at height ho (m/s)
        #a = the wind shear exponent
        #Terrain   Wind Shear Exponent
        #- a -
        #  Open water   0.1
        #Smooth, level, grass-covered   0.15
        #Row crops 	0.2
        #Low bushes with a few trees 	0.2
        #Heavy trees 	0.25
        #Several buildings 	0.25
        #Hilly, mountainous terrain 	0.25
        if(dailywind!=1){
          WNMINN<-WNMINN*(1.2/10)^0.15*.1 # reduce min wind further because have only 9am/3pm values to get max/min
          WNMAXX<-WNMAXX*(1.2/10)^0.15
          WNMINN<-WNMINN#*3.25 # for snow
          WNMAXX<-WNMAXX#*3.25 # for snow
          cat('min wind * 0.1 ')
          #cat('max wind * 2.0 for snow ')
        }else{
          if(snowmodel==0){
            WNMAXX<-dwind*(1.2/2)^0.15
            WNMINN<-WNMAXX
            WNMAXX<-WNMAXX*2#*3.5#*5
            WNMINN<-WNMINN*0.5#1.5#*3.5#*2
            cat('min wind * 0.5')
            cat('max wind * 2')
          }else{
            WNMAXX<-dwind*(1.2/2)^0.15
            WNMINN<-WNMAXX
            WNMAXX<-WNMAXX*2*2.5#*3.5#*5
            WNMINN<-WNMINN*0.5*3#1.5#*3.5#*2
            cat('min wind * 0.5 * 1.5 for snow ')
            cat('max wind * 2 * 2.5 for snow')
          }
        }
        CCMINN<-CCMINN*0.5
        CCMAXX<-CCMAXX*2
        CCMINN[CCMINN>100]<-100
        CCMAXX[CCMAXX>100]<-100
        cat('min cloud * 0.5 ')
        cat('max cloud * 2')
        
        SNOW <- rep(0,timeinterval*nyears) # no snow simulated on surface
        
        # impose uniform warming
        TMAXX<-TMAXX+warm
        TMINN<-TMINN+warm
        
        if(soildata!=1){
          SLES<-matrix(nrow=7300,data=0) 
          SLES<-SLES+SLE
        }
        #quick fix to make it so that MINSHADES is at the user-specified value and MAXSHADES is from the FAPAR database 
        if(soildata==1 & manualshade==0){
          MINSHADES<-MAXSHADES
          MINSHADES[1:length(MINSHADES)]<-minshade
        }
        
        if(soildata==1){
          moists2<-matrix(nrow=10, ncol = ndays, data=0)
          soilwet[soilwet>0] = 1
          moists2[1,]<-uppermoists# soilwet #moists2[1,]*0+0#uppermoists
          moists2[2,]<-uppermoists
          moists2[3,]<-lowermoists
          moists2[4,]<-lowermoists
          moists<-moists2
        }else{
          
          moists2<-matrix(nrow=10, ncol = ndays, data=0)
          moists2[1,ndays]<-SoilMoist[1]
          moists<-moists2
          
        }
        
        soilprops<-matrix(data = 0, nrow = 10, ncol = 6)
        
        if(soildata==1){
          if(is.na(soilprop$BBDensity50)==FALSE){
            # soil properties (bulk density, saturated water content, proportion clay) for each level
            soilprops[1,1]<-soilprop$ABDensity50#*.65 # bulk density
            soilprops[2,1]<-soilprop$ABDensity50 
            soilprops[3,1]<-soilprop$BBDensity50#*.7#soilprop$BBDensity50
            soilprops[4,1]<-soilprop$BBDensity50#*.7#soilprop$BBDensity50
            soilprops[1,2]<-soilprop$A_01bar     # saturated water content
            soilprops[2,2]<-soilprop$A_01bar
            soilprops[3,2]<-soilprop$B_01bar
            soilprops[4,2]<-soilprop$B_01bar
            soilprops[1,3]<-soilprop$Aclay50     # percent clay
            soilprops[2,3]<-soilprop$Aclay50
            soilprops[3,3]<-soilprop$Bclay50
            soilprops[4,3]<-soilprop$Bclay50
          }else{
            # soil properties (bulk density, saturated water content, proportion clay) for each level
            soilprops[1,1]<-soilprop$ABDensity50#*.65 # bulk density
            soilprops[2,1]<-soilprop$ABDensity50 
            soilprops[3,1]<-soilprop$ABDensity50
            soilprops[4,1]<-soilprop$ABDensity50
            soilprops[1,2]<-soilprop$A_01bar     # saturated water content
            soilprops[2,2]<-soilprop$A_01bar
            soilprops[3,2]<-soilprop$A_01bar
            soilprops[4,2]<-soilprop$A_01bar
            soilprops[1,3]<-soilprop$Aclay50     # percent clay
            soilprops[2,3]<-soilprop$Aclay50
            soilprops[3,3]<-soilprop$Aclay50
            soilprops[4,3]<-soilprop$Aclay50
          }
          if(cap==1){
            soilprops[1,4]<-0.2#1.#Thcond#0.25 #0.07 #Thcond #0.07#Thcond #0.03 #2.5                # mineral thermal conductivity
          }else{
            soilprops[1,4]<-Thcond
          }
          soilprops[2,4]<-Thcond
          soilprops[3,4]<-Thcond
          soilprops[4,4]<-Thcond
          if(cap==1){
            soilprops[1,5]<-1920#SpecHeat#1920#1320           # mineral heat capacity
          }else{
            soilprops[1,5]<-SpecHeat 
          }
          soilprops[2,5]<-SpecHeat
          soilprops[3,5]<-SpecHeat
          soilprops[4,5]<-SpecHeat
          soilprops[1,6]<-Density#1.3 #2.65
          soilprops[2,6]<-Density
          soilprops[3,6]<-Density
          soilprops[4,6]<-Density
        }else{
          soilprops[1,1]<-BulkDensity 
          soilprops[1,2]<-SatWater    
          soilprops[1,3]<-Clay       
          soilprops[1,4]<-Thcond 
          soilprops[1,5]<-SpecHeat        
          soilprops[1,6]<-Density 
        }
        soilprops<-(ifelse(is.na(soilprops),0,soilprops))
        
        ############## manual soil properties ######################
        #                         Intrvls <-(1:julnum) # user-supplied last Julian day in each time interval sequence
        #                         Numint <- julnum  # number of time intervals
        #                         Numtyps <- 4
        #                  
        #                         Nodes <- matrix(data = 0, nrow = 10, ncol = 7300) # deepest nodes for each substrate type
        #                         Nodes[1,]<-Nodes[1,]*0+4
        #                         Nodes[2,]<-Nodes[2,]*0+5
        #                         Nodes[3,]<-Nodes[3,]*0+6
        #                         Nodes[4,]<-Nodes[4,]*0+9#[4,]*0+9
        #                 
        #                                 soilprops[1,1]<- 0.65#1.4# soil bulk density Mg/Mg
        #                                 soilprops[2,1]<-1.4
        #                                 soilprops[3,1]<-1.4
        #                                 soilprops[4,1]<-1.4
        #                                 soilprops[1,2]<- 0.4# saturated water content m3/m3
        #                                 soilprops[2,2]<-0.4
        #                                 soilprops[3,2]<-0.4
        #                                 soilprops[4,2]<-0.4
        #                                 soilprops[1,3]<-0 # percent clay
        #                                 soilprops[2,3]<-0
        #                                 soilprops[3,3]<-0
        #                                 soilprops[4,3]<-0
        #                                 soilprops[1,4]<- 0.2# mineral thermal conductivity (W/mC)
        #                                 soilprops[2,4]<-2.5
        #                                 soilprops[3,4]<-2.5
        #                                 soilprops[4,4]<-2.5
        #                                 soilprops[1,5]<- 1920#870# mineral head capacity (J/kgK)
        #                                 soilprops[2,5]<-870
        #                                 soilprops[3,5]<-870
        #                                 soilprops[4,5]<-870
        #                                 soilprops[1,6]<- 1.3#2.65# mineral density (Mg/Mg)
        #                                 soilprops[2,6]<- 2.65
        #                                 soilprops[3,6]<- 2.65
        #                                 soilprops[4,6]<-2.65
        #         
        #                 moists[1,]<-moists[1,]*0#0.2#+0
        #                 moists[2,]<-moists[2,]*0#*0.2#+0
        #                 moists[3,]<-moists[3,]*0#*0.2#+0
        #                 moists[4,]<-moists[4,]*0#*0.2#+0#*0.2#+0
        
        #PCTWET <- (1:(timeinterval*nyears))*0+30
        #hori<-rep(45,24)
        #RAINFALL[249]<-100
        
        #REFLS<-REFLS*0+0.2 # soil reflectances (decimal percent) # Cape Range
        #                 #REFLS<-REFLS*0+0.45 # soil reflectances (decimal percent) # Dirk Hartog
        #                 
        #                 #PCTWET<-PCTWET#*0+0.3 # percentage of unit surface area of ground that is wet (decimal percent)
        #                SLES<-SLES*0+0.96 # Substrate longwave IR emissivity (decimal %)
        #                 
        #                # DEP<-as.matrix(c(0., 2.5,  5.,  10., 15.,  30.,  40.,  50.,  60.,  250.)) # Soil nodes (cm)
        #                 
        #                 
        #                 #microinput[18]<-0 # slope (degrees)
        #                 #microinput[19]<-0 # aspect (degrees, 0 = North)
        #                 #microinput[22]<-1 # cm H2O in air column
        #                 #microinput[4] <- 1 # local height (cm) at which animal/container calculations will be made
        #                 #microinput[2]<-0.002 # Roughness height (m) (make sure it is smaller than user height)
        #                 
        #                 #WNMAXX<-WNMAXX*.45 # Change wind speed
        #                 #WNMINN<-WNMINN*.1 # Change wind speed
        #         ############################# end manual soil properties #######################
        ############## manual soil properties sea turtles ######################
        #         soilprops[1,1]<- 1.29# soil bulk density Mg/Mg
        #         soilprops[2,1]<-1.29
        #         soilprops[3,1]<-1.29
        #         soilprops[4,1]<-1.29
        #         soilprops[1,2]<- 0.4# saturated water content m3/m3
        #         soilprops[2,2]<-0.4
        #         soilprops[3,2]<-0.4
        #         soilprops[4,2]<-0.4
        #         soilprops[1,3]<-0 # percent clay
        #         soilprops[2,3]<-0
        #         soilprops[3,3]<-0
        #         soilprops[4,3]<-0
        #         soilprops[1,4]<- 8.8# mineral thermal conductivity (W/mC)
        #         soilprops[2,4]<-8.8
        #         soilprops[3,4]<-8.8
        #         soilprops[4,4]<-8.8
        #         soilprops[1,5]<- 800# mineral head capacity (J/kgK)
        #         soilprops[2,5]<-800
        #         soilprops[3,5]<-800
        #         soilprops[4,5]<-800
        #         soilprops[1,6]<- 2.66# mineral density (Mg/Mg)
        #         soilprops[2,6]<- 2.66
        #         soilprops[3,6]<- 2.66
        #         soilprops[4,6]<-2.66
        #         
        #         moists[1,]<-moists[1,]*0+.9
        #         moists[2,]<-moists[2,]*0+.9
        #         moists[3,]<-moists[3,]*0+.9
        #         moists[4,]<-moists[4,]*0+.9
        #         
        #         Nodes[1,]<-Nodes[1,]*0+2
        #         Nodes[2,]<-Nodes[2,]*0+4
        #         Nodes[3,]<-Nodes[3,]*0+6
        #         Nodes[4,]<-Nodes[4,]*0+9
        #         
        #         #REFLS<-REFLS*0+0.60 # soil reflectances (decimal percent) # Cape Range
        if(snowmodel==1){
          REFLS<-REFLS*0+0.30 # soil reflectances (decimal percent) # Dirk Hartog (actually - changed for snow, fix this)
        }
        #         #REFLS<-REFLS*0+0.62 # soil reflectances (decimal percent) # Gnaraloo
        #         
        #         PCTWET<-PCTWET#*0+0.3 # percentage of unit surface area of ground that is wet (decimal percent)
        #         SLES<-SLES#*0+ # Substrate longwave IR emissivity (decimal %)
        #         
        #        # DEP<-as.matrix(c(0., 2.5,  5.,  10., 15.,  30.,  40.,  50.,  60.,  250.)) # Soil nodes (cm)
        #         
        #         
        #         microinput[18]<-0 # slope (degrees)
        #         microinput[19]<-0 # aspect (degrees, 0 = North)
        #         microinput[22]<-1 # cm H2O in air column
        #         microinput[4] <- 1 # local height (cm) at which animal/container calculations will be made
        #         microinput[2]<-0.002 # Roughness height (m) (make sure it is smaller than user height)
        #         WNMAXX.orig<-WNMAXX
        #         WNMAXX<-WNMAXX.orig*1.5#*(1.8*1.3)#*1.8#*.45 # Change wind speed
        #         WNMINN<-WNMAXX.orig*0.5#*(.5*1.3)#*.45 # Change wind speed
        #         
        #         TMAXX<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/TMAXX_obs.csv", sep = ",",header=TRUE)
        #         names(TMAXX)<-NULL
        #         TMAXX<-as.matrix(TMAXX[-1])
        #         TMINN<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/TMINN_obs.csv", sep = ",",header=TRUE)
        #         names(TMINN)<-NULL
        #         TMINN<-as.matrix(TMINN[-1])
        #         RHMAXX<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/RHMAXX_obs.csv", sep = ",",header=TRUE)
        #         names(RHMAXX)<-NULL
        #         RHMAXX<-as.matrix(RHMAXX[-1])
        #         RHMINN<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/RHMINN_obs.csv", sep = ",",header=TRUE)
        #         names(RHMINN)<-NULL
        #         RHMINN<-as.matrix(RHMINN[-1])
        #         CCMAXX<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/CCMAXX_obs.csv", sep = ",",header=TRUE)
        #         names(CCMAXX)<-NULL
        #         CCMAXX<-as.matrix(CCMAXX[-1])
        #         CCMINN<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/CCMINN_obs.csv", sep = ",",header=TRUE)
        #         names(CCMINN)<-NULL
        #         CCMINN<-as.matrix(CCMINN[-1])
        #         WNMAXX<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/WNMAXX_obs.csv", sep = ",",header=TRUE)
        #         names(WNMAXX)<-NULL
        #         WNMAXX<-as.matrix(WNMAXX[-1])
        #         WNMINN<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/WNMINN_obs.csv", sep = ",",header=TRUE)
        #         names(WNMINN)<-NULL
        #         WNMINN<-as.matrix(WNMINN[-1])
        # #         
        #         WNMAXX.orig<-WNMAXX
        #         WNMINN.orig<-WNMINN
        #         WNMAXX<-WNMAXX.orig#*1.3#*.45 # Change wind speed
        #         WNMINN<-WNMINN.orig#*1.3#*.5#*.45 # Change wind speed
        ############################# end manual soil properties #######################
        
        if(loop>0){
          TMAXX<-c(TMAXX[((loop)*365+1):(nyears*365)],TMAXX[1:((loop)*365)])
          TMINN<-c(TMINN[((loop)*365+1):(nyears*365)],TMINN[1:((loop)*365)])
          RHMAXX<-c(RHMAXX[((loop)*365+1):(nyears*365)],RHMAXX[1:((loop)*365)])
          RHMINN<-c(RHMINN[((loop)*365+1):(nyears*365)],RHMINN[1:((loop)*365)])
          CCMAXX<-c(CCMAXX[((loop)*365+1):(nyears*365)],CCMAXX[1:((loop)*365)])
          CCMINN<-c(CCMINN[((loop)*365+1):(nyears*365)],CCMINN[1:((loop)*365)])
          WNMAXX<-c(WNMAXX[((loop)*365+1):(nyears*365)],WNMAXX[1:((loop)*365)])
          WNMINN<-c(WNMINN[((loop)*365+1):(nyears*365)],WNMINN[1:((loop)*365)])
          PCTWET<-c(PCTWET[((loop)*365+1):(nyears*365)],PCTWET[1:((loop)*365)])
          moists<-cbind(moists[,((loop)*365+1):(nyears*365)],moists[,1:((loop)*365)])
          RAINFALL<-c(RAINFALL[((loop)*365+1):(nyears*365)],RAINFALL[1:((loop)*365)])
          
        }
        
        TMAXX_adult<-na.approx(read.csv('pond and aestivation/Adults.csv')[,4])
        TMINN_adult<-na.approx(read.csv('pond and aestivation/Adults.csv')[,3])
        TMAXX_juv<-na.approx(read.csv('pond and aestivation/Hatchlings.csv')[,4])
        TMINN_juv<-na.approx(read.csv('pond and aestivation/Hatchlings.csv')[,3])
        TMAXX<-c(TMAXX_adult,TMAXX_juv)
        TMINN<-c(TMINN_adult,TMINN_juv)
        # microclimate input parameters listALTT,ALREF,ALMINT,ALONG,AMINUT,ALAT
        ALTT<-as.numeric(ALTT)
        ALREF<-as.numeric(ALREF)
        ALMINT<-as.numeric(ALMINT)
        ALONG<-as.numeric(ALONG)
        AMINUT<-as.numeric(AMINUT)
        ALAT<-as.numeric(ALAT)
        microinput<-c(julnum,RUF,ERR,Usrhyt,Numtyps,Numint,Z01,Z02,ZH1,ZH2,idayst,ida,HEMIS,ALAT,AMINUT,ALONG,ALMINT,ALREF,SLOPE,AZMUTH,ALTT,CMH2O,microdaily,tannul,EC,VIEWF,snowtemp,snowdens,snowmelt,undercatch)
        julday1=matrix(data = 0., nrow = 7300, ncol = 1)
        SLES1=matrix(data = 0., nrow = 7300, ncol = 1)
        Intrvls1=matrix(data = 0., nrow = 7300, ncol = 1)
        MAXSHADES1=matrix(data = 0., nrow = 7300, ncol = 1)
        MINSHADES1=matrix(data = 0., nrow = 7300, ncol = 1)
        TMAXX1=matrix(data = 0., nrow = 7300, ncol = 1)
        TMINN1=matrix(data = 0., nrow = 7300, ncol = 1)
        CCMAXX1=matrix(data = 0., nrow = 7300, ncol = 1)
        CCMINN1=matrix(data = 0., nrow = 7300, ncol = 1)
        RHMAXX1=matrix(data = 0., nrow = 7300, ncol = 1)
        RHMINN1=matrix(data = 0., nrow = 7300, ncol = 1)
        WNMAXX1=matrix(data = 0., nrow = 7300, ncol = 1)
        WNMINN1=matrix(data = 0., nrow = 7300, ncol = 1)
        SNOW1=matrix(data = 0., nrow = 7300, ncol = 1)
        REFLS1=matrix(data = 0., nrow = 7300, ncol = 1)
        PCTWET1=matrix(data = 0., nrow = 7300, ncol = 1)
        RAINFALL1=matrix(data = 0, nrow = 7300, ncol = 1)
        tannul1=matrix(data = 0, nrow = 7300, ncol = 1)
        moists1=matrix(data = 0., nrow = 10, ncol = 7300)
        julday1[1:julnum]<-julday
        SLES1[1:julnum]<-SLES
        Intrvls1[1:julnum]<-Intrvls
        MAXSHADES1[1:julnum]<-MAXSHADES
        MINSHADES1[1:julnum]<-MINSHADES
        TMAXX1[1:julnum]<-TMAXX
        TMINN1[1:julnum]<-TMINN
        CCMAXX1[1:julnum]<-CCMAXX
        CCMINN1[1:julnum]<-CCMINN
        RHMAXX1[1:julnum]<-RHMAXX
        RHMINN1[1:julnum]<-RHMINN
        WNMAXX1[1:julnum]<-WNMAXX
        WNMINN1[1:julnum]<-WNMINN
        SNOW1[1:julnum]<-SNOW
        REFLS1[1:julnum]<-REFLS
        PCTWET1[1:julnum]<-PCTWET
        RAINFALL1[1:julnum]<-RAINFALL
        tannul1[1:julnum]<-tannul
        moists1[1:10,1:julnum]<-moists
        # all microclimate data input list - all these variables are expected by the input argument of the fortran micro2014 subroutine
        micro<-list(microinput=microinput,julday=julday,SLES=SLES1,DEP=DEP,Intrvls=Intrvls1,Nodes=Nodes,MAXSHADES=MAXSHADES,MINSHADES=MINSHADES,TIMAXS=TIMAXS,TIMINS=TIMINS,TMAXX=TMAXX1,TMINN=TMINN1,RHMAXX=RHMAXX1,RHMINN=RHMINN1,CCMAXX=CCMAXX1,CCMINN=CCMINN1,WNMAXX=WNMAXX1,WNMINN=WNMINN1,SNOW=SNOW1,REFLS=REFLS1,PCTWET=PCTWET1,soilinit=soilinit,hori=hori,TAI=TAI,soilprops=soilprops,moists=moists1,RAINFALL=RAINFALL1,tannulrun=tannulrun)
        
        # write all input to csv files in their own folder
        if(write_input==1){
          write.table(as.matrix(microinput), file = "csv input/microinput.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(julday, file = "csv input/julday.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(SLES, file = "csv input/SLES.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(DEP, file = "csv input/DEP.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(Intrvls, file = "csv input/Intrvls.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(Nodes, file = "csv input/Nodes.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(MAXSHADES, file = "csv input/Maxshades.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(MINSHADES, file = "csv input/Minshades.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(TIMAXS, file = "csv input/TIMAXS.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(TIMINS, file = "csv input/TIMINS.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(TMAXX, file = "csv input/TMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(TMINN, file = "csv input/TMINN.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(RHMAXX, file = "csv input/RHMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(RHMINN, file = "csv input/RHMINN.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(CCMAXX, file = "csv input/CCMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(CCMINN, file = "csv input/CCMINN.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(WNMAXX, file = "csv input/WNMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(WNMINN, file = "csv input/WNMINN.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(SNOW, file = "csv input/SNOW.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(REFLS, file = "csv input/REFLS.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(PCTWET, file = "csv input/PCTWET.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(soilinit, file = "csv input/soilinit.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(hori, file = "csv input/hori.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(TAI, file = "csv input/TAI.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(soilprops, file="csv input/soilprop.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(moists,file="csv input/moists.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(RAINFALL,file="csv input/rain.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(tannulrun,file="csv input/tannulrun.csv", sep = ",", col.names = NA, qmethod = "double")
        }
        
        
        # Fortran wrapper for the microclimate model 
        #if(snowmodel==1){
        #  source('NicheMapperMicroYears_snow.r')
        #}else{
        source('/git/micro_australia/NicheMapR_micro.R')
        #}
        microut<-microclimate(micro)
        
        metout<-microut$metout # retrieve above ground microclimatic conditions, min shade
        shadmet<-microut$shadmet # retrieve above ground microclimatic conditions, max shade
        soil<-microut$soil # retrieve soil temperatures, minimum shade
        shadsoil<-microut$shadsoil # retrieve soil temperatures, maximum shade
        
        # metout/shadmet variables:
        # 1 JULDAY - day of year
        # 2 TIME - time of day (mins)
        # 3 TALOC - air temperature (deg C) at local height (specified by 'Usrhyt' variable)
        # 4 TAREF - air temperature (deg C) at reference height (1.2m)
        # 5 RHLOC - relative humidity (%) at local height (specified by 'Usrhyt' variable)
        # 6 RH  - relative humidity (%) at reference height (1.2m)
        # 7 VLOC - wind speed (m/s) at local height (specified by 'Usrhyt' variable)
        # 8 VREF - wind speed (m/s) at reference height (1.2m)
        # 9 ZEN - zenith angle of sun (degrees - 90 = below the horizon)
        # 10 SOLR - solar radiation (W/m2)
        # 11 TSKYC - sky radiant temperature (deg C)
        # 12 SNOWFALL - snow predicted to have fallen (mm)
        # 13 SNOWDEP - predicted snow depth (cm)
        
        # soil and shadsoil variables:
        # 1 JULDAY - day of year
        # 2 TIME - time of day (mins)
        # 3-12 D0cm ... - soil temperatures at each of the 10 specified depths
        
        
        return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,RAINFALL=RAINFALL,ALTT=ALTT,REFL=REFL[1],longlat=longlat,fieldcap=soilprop$A_01bar*100,wilting=soilprop$A_15bar*100,MAXSHADES=MAXSHADES,longlat=c(x[1],x[2])))
      } # end of check for na sites
} # end of check if soil data is being used but no soil data returned

  } # end error trapping
  } # end of NicheMapR_Setup_micro function    
