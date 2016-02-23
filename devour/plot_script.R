setwd("c:/git/wst/devour/results")
files<-list.files()

names<-c("lonsites","latsites","deps","srs","ctes","start inc","end inc","inc time","start TSP","end TSP", "mort")

#scenarios<-c("","_ACCESS 1.3_2050","_ACCESS 1.3_2070","_HadGEM2-CC_2050","_HadGEM2-CC_2070")
scenarios<-c("","_ACCESS 1.3_2050","_ACCESS 1.3_2070")
shades<-c(20,50)
ovis<-c(296,327)

# year<-2009
# scenario<-""
# shade<-"20"
# ovi<-"327"

for(year in 2001:2009){
  
for(k in 1:length(scenarios)){
scenario<-scenarios[k]
for(l in 1:length(shades)){
shade<-shades[l]
for(m in 1:length(ovi)){
ovi<-ovis[m]

files.to.merge<-files[grep(pattern = paste("ovi_",ovi,"_shade_",shade,"_year_",year,"_scen_",scenario,".csv",sep=""),x = files)]

for(i in 1:length(files.to.merge)){
  data<-read.csv(files.to.merge[i],header = FALSE, stringsAsFactors = FALSE)
  colnames(data)<-names
  data_minshade<-data[1:(nrow(data)/2),]
  data_maxshade<-data[(nrow(data)/2+1):nrow(data),]
  if(i==1){
    alldata_maxshade<-data_maxshade
    alldata_minshade<-data_minshade
  }else{
    alldata_maxshade<-rbind(alldata_maxshade,data_maxshade)
    alldata_minshade<-rbind(alldata_minshade,data_minshade)
  }
}

if(shade==20){
  filemax<-paste("../merged_ovi_",ovi,"_shade_",75,"_year_",year,"_scen_",scenario,".csv",sep="")
}else{
  filemax<-paste("../merged_ovi_",ovi,"_shade_",90,"_year_",year,"_scen_",scenario,".csv",sep="")
}

filemin<-paste("../merged_ovi_",ovi,"_shade_",shade,"_year_",year,"_scen_",scenario,".csv",sep="")

write.csv(alldata_maxshade, filemax)
write.csv(alldata_minshade, filemin)


  par(mfrow=c(3,2))
    par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
    par(mar = c(3,3,1,1) + 0.1) # margin spacing stuff 
    
DEP <- c(0., 2.5, 5, 8.25,  15,  20.,  30.,  60.,  90.,  200.)
DEP <- c(0., 2.5, 5, 8.25,  15,  20.)

for(i in 1:length(DEP)){

  
  plotdata<-as.data.frame(subset(alldata_minshade,deps==DEP[i]))
  plotdata<-subset(plotdata, is.na(plotdata$`inc time`)==FALSE)
  max<-max(plotdata$`inc time`)
  min<-min(plotdata$`inc time`)
  
  lat1<-min(plotdata[,2])-.025 # min latitude
  lat2<-max(plotdata[,2])+.025 # max latitude
  lon1<-min(plotdata[,1])-.025 # min longitude
  lon2<-max(plotdata[,1])+.025 # max longitude
  quadwid<-(lon2-lon1)/.05
  quadlen<-(lat2-lat1)/.05
  gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)
  
  x<-cbind(plotdata$lonsites,plotdata$latsites) # list of co-ordinates
  
  grid <- rasterize(x, gridout,plotdata$`inc time`)
  grid <- projectRaster(grid, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  plot(grid, main=paste("Depth ",DEP[i]," cm",sep=""),zlim=c(min,max),xlim=c(lon1,lon2),ylim=c(lat1,lat2))
title("", cex = 1.5)
}
title(filemin, cex = 1.5, outer = TRUE)


for(i in 1:length(DEP)){

  
  plotdata<-as.data.frame(subset(alldata_maxshade,deps==DEP[i]))
  plotdata<-subset(plotdata, is.na(plotdata$`inc time`)==FALSE)
  max<-max(plotdata$`inc time`)
  min<-min(plotdata$`inc time`)
  
  lat1<-min(plotdata[,2])-.025 # min latitude
  lat2<-max(plotdata[,2])+.025 # max latitude
  lon1<-min(plotdata[,1])-.025 # min longitude
  lon2<-max(plotdata[,1])+.025 # max longitude
  quadwid<-(lon2-lon1)/.05
  quadlen<-(lat2-lat1)/.05
  gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)
  
  x<-cbind(plotdata$lonsites,plotdata$latsites) # list of co-ordinates
  
  grid <- rasterize(x, gridout,plotdata$`inc time`)
  grid <- projectRaster(grid, crs="+proj=longlat +datum=WGS84") # change projection here if needed
  plot(grid, main=paste("Depth ",DEP[i]," cm",sep=""),zlim=c(min,max),xlim=c(lon1,lon2),ylim=c(lat1,lat2))
title("", cex = 1.5)
}
title(filemax, cex = 1.5, outer = TRUE)

}}}}

