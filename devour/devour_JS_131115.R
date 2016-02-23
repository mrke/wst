
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
