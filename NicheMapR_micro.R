microclimate <- function(micro) {
  # If the library hasn't been loaded yet, load it
  if (!is.loaded('micr2014')) {
    dyn.load('micr2014.dll')
  } 
  julnum <- as.double(micro$julnum) 
  a <- .Fortran("micr2014", 
as.double(micro$microinput), 
as.double(micro$julday), 
as.double(micro$SLES), 
as.double(micro$DEP), 
as.double(micro$Intrvls), 
as.double(micro$MAXSHADES), 
as.double(micro$MINSHADES), 
as.double(micro$Nodes), 
as.double(micro$TIMAXS), 
as.double(micro$TIMINS), 
as.double(micro$RHMAXX), 
as.double(micro$RHMINN), 
as.double(micro$CCMAXX), 
as.double(micro$CCMINN), 
as.double(micro$WNMAXX), 
as.double(micro$WNMINN), 
as.double(micro$TMAXX), 
as.double(micro$TMINN), 
as.double(micro$SNOW), 
as.double(micro$REFLS), 
as.double(micro$PCTWET), 
as.double(micro$soilinit),  
as.double(micro$hori),
as.double(micro$TAI),
as.double(micro$soilprop),
as.double(micro$moists),
as.double(micro$RAINFALL),
as.double(micro$tannulrun),
metout=matrix(data = 0., nrow = 24*7300, ncol = 18), 
soil=matrix(data = 0., nrow = 24*7300, ncol = 12), 
shadmet=matrix(data = 0., nrow = 24*7300, ncol = 18),
shadsoil=matrix(data = 0., nrow = 24*7300, ncol = 12))
dyn.unload("micr2014.dll") 

metout <- matrix(data = 0., nrow = 24*7300, ncol = 18)
shadmet <- matrix(data = 0., nrow = 24*7300, ncol = 18)
soil <- matrix(data = 0., nrow = 24*7300, ncol = 12)
shadsoil <- matrix(data = 0., nrow = 24*7300, ncol = 12)
storage.mode(metout)<-"double"
storage.mode(shadmet)<-"double"
storage.mode(soil)<-"double"
storage.mode(shadsoil)<-"double"
metout<-a$metout
shadmet<-a$shadmet
soil<-a$soil
shadsoil<-a$shadsoil
metout.names<-c("JULDAY","TIME","TALOC","TAREF","RHLOC","RH","VLOC","VREF","TS","T2","TDEEP","ZEN","SOLR","TSKYC","DEW","FROST","SNOWFALL","SNOWDEP")
colnames(metout)<-metout.names
colnames(shadmet)<-metout.names
soil.names<-c("JULDAY","TIME",paste("D",DEP,"cm", sep = ""))
colnames(soil)<-soil.names
colnames(shadsoil)<-soil.names

return (list(metout=metout, soil=soil, shadmet=shadmet, shadsoil=shadsoil))
}