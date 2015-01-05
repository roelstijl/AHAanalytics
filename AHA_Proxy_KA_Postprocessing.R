AHA_Proxy_KA_Postprocessing = function(PC_4) 
{
# This function will plot the data files used in project AHA onto google maps
# Input is a series of PC4 areas or a single PC4 area PC_4 = 6810:6823 for Arnhem
# Method include proxy 
# Load the required data files -------------------------------------
load(paste0(settings$Input_Datasets,"/23. Validatie_data/Validatie koppelingen.Rda"))
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_NOR.Rda"))
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))
load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_6_Spatial.Rda"))
load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_4_Spatial.Rda"))


# Add the PC4 to PC6 set
pc6@data$PC_4 = substr(pc6@data$POSTCODE,1,4)

# Coo_X van missing due to bug
colnames(assets$LSkabels)[c(3,4)]=c("Coo_X_van","Coo_Y_van")
colnames(assets$MSkabels)[c(3,4)]=c("Coo_X_van","Coo_Y_van")
assets$kabels = rbind(assets$LSkabels,assets$MSkabels,fill=TRUE)[,list(Brontabel,Status_ID,DateLength_ch,ID_NAN,Voltage,Length_ch,DateRemoved,DateAdded,Coo_X_van,Coo_Y_van,PC_6_van,Coo_X_naar,Coo_Y_naar,PC_6_naar,PC_4_van,PC_4_naar)]
setkey(assets$kabels,ID_NAN);   
assets$kabels = assets$kabels[DateAdded>"2014-01-04"]
assets$kabels = unique(assets$kabels)

assets$moffen = rbind(assets$LSmoffen,assets$MSmoffen,fill=TRUE)[,list(Brontabel,Status_ID,ID_NAN,Voltage,DateRemoved,DateAdded,Coo_X,Coo_Y,PC_4,PC_6)]
assets$moffen = assets$moffen[DateAdded>"2014-01-04"]

assets$all    = rbind(assets$moffen, assets$kabels,fill=TRUE)

storingen$all = rbind(storingen$LS,storingen$MS,fill=TRUE)[,list(Datum,Status,ID_KLAK_Melding,Getroffen_klanten_totaal,Duur_onderbreking,Datum_Eerste_Ontwerp,Klacht,Netcomponent,Mof,Veroorzaker,Tijdstip_definitief_einde,Coo_X,Coo_Y,PC_4,PC_6)]
storingen$all[,Coo_X:=as.numeric(gsub(",",".",Coo_X))];storingen$all[,Coo_Y:=as.numeric(gsub(",",".",Coo_Y))];
storingen$KLAKMelders[,Coo_X:=as.numeric(gsub(",",".",Coo_X))];storingen$KLAKMelders[,Coo_Y:=as.numeric(gsub(",",".",Coo_Y))];

# Create file with found assets  -------------------------------
ValidatieSet[,inNOR:=(ID_NAN %in% assets$all$ID_NAN)]
ValidatieSet[,inKLAK:=(ID_KLAK_Melding %in% storingen$all$ID_KLAK_Melding)]
ValidatieSet[,inKLAKMelders:=(ID_KLAK_Melding %in% storingen$KLAKMELDERS$ID_KLAK_Melding)]
ValidatieSet[,inAllSystems := inNOR&inKLAK&inKLAKMelders]

molten = melt(ValidatieSet, id.vars = c("Regio","Datum","ID_KLAK_Melding","ID_NAN"),measure.vars=c("inNOR","inKLAK","inKLAKMelders","inAllSystems"))

ggplot(molten,aes(variable,fill=paste(Regio,value))) + geom_bar(position="dodge",colour="black")
}

# Put it on a map -------------------------------------------------------
Createmaps = function(PC_4,assets,storingen,pc4,pc6,ID_KLAK_M,ID_NAN_M){
geosets = list()
PC_4_M=storingen$all[ID_KLAK_M == storingen$all$ID_KLAK_M,PC_4]
ID_KLAK_M= "3762757"
ID_NAN_M = "219DA2E3-B20C-4F88-B590-D5F0677056F5"

# Kabels
geosets$kabels=
  SpatialLinesDataFrame(
    SpatialLines(
      alply(
      assets$kabels[!is.na(Coo_X_van) & (PC_4_van== PC_4 |PC_4_naar== PC_4)],1,
      function(set) Lines(list(Line(t(rbind(set[,list(Coo_X_van,Coo_X_naar)],
      set[,list(Coo_Y_van,Coo_Y_naar)],use.names=FALSE)))),set$ID_NAN)),
    proj4string=CRS("+init=epsg:28992")), 
  assets$kabels[!is.na(Coo_X_van) & (PC_4_van== PC_4 |PC_4_naar== PC_4)],match.ID = FALSE)

# Moffen
geosets$moffen                 = (assets$moffen[!is.na(assets$moffen$Coo_X)&PC_4 == PC_4_M])
coordinates(geosets$moffen)    = geosets$moffen[,list(Coo_X,Coo_Y)]
proj4string(geosets$moffen)    = CRS("+init=epsg:28992")

# Storingen
geosets$storingen              = (storingen$all[!is.na(storingen$all$Coo_X)&PC_4 == PC_4_M])
coordinates(geosets$storingen) = geosets$storingen  [,list(Coo_X,Coo_Y)]
proj4string(geosets$storingen) = CRS("+init=epsg:28992")

# Melders
geosets$KLAKMelders              = (storingen$KLAKMelders[!is.na(storingen$KLAKMelders$Coo_X)& ID_KLAK_Melding ==ID_KLAK_M])
coordinates(geosets$KLAKMelders) = geosets$KLAKMelders  [,list(Coo_X,Coo_Y)]
proj4string(geosets$KLAKMelders) = CRS("+init=epsg:28992")

# Put it on the map
# map=plotGoogleMaps(geosets$kabels[geosets$kabels$ID_NAN!=ID_NAN_M,],legend=FALSE,layerName="Omringende Kabels",zcol="Status_ID",colPalette = heat.colors(2),strokeWeight=3,add=TRUE)
# map=plotGoogleMaps(geosets$moffen[geosets$moffen$ID_NAN!=ID_NAN_M,],legend=FALSE,layerName="Omrindngende Moffen",zcol="Status_ID",strokeColor = "Blue",add=TRUE,previousMap=map)
# map=plotGoogleMaps(geosets$storingen[geosets$storingen$ID_KLAK_Melding!=ID_KLAK_M,],legend=FALSE,layerName="Omringende Storingen",zcol="ID_KLAK_Melding",strokeColor = "Blue",add=TRUE,previousMap=map)
# 
# map=plotGoogleMaps(geosets$kabels[geosets$kabels$ID_NAN==ID_NAN_M,],legend=FALSE,layerName="Betrokken Kabel",zcol="Status_ID",strokeColor = "yellow",strokeWeight=10,previousMap=map,add=TRUE)
# map=plotGoogleMaps(geosets$moffen[geosets$moffen$ID_NAN==ID_NAN_M,],legend=FALSE,layerName="Betrokken Mof",zcol="Status_ID",strokeColor = "Blue",add=TRUE,previousMap=map)
# 
# map=plotGoogleMaps(geosets$storingen[geosets$storingen$ID_KLAK_Melding==ID_KLAK_M,],legend=FALSE,layerName="Kabels",zcol="ID_KLAK_Melding",strokeColor = "Blue",add=TRUE,previousMap=map)
# map=plotGoogleMaps(geosets$KLAKMelders,legend=FALSE,layerName="ID_KLAK_Melding",zcol="ID_KLAK_Melding",colPalette = "Blue",previousMap=map)

map=pGMwrapper(pc6[pc6$PC_4==PC_4_M,],"Postcode 6 gebieden","POSTCODE","Blue",3,FALSE)
map=pGMwrapper(geosets$storingen[geosets$storingen$ID_KLAK_Melding!=ID_KLAK_M,],"Omringende Storingen","ID_KLAK_Melding","Blue",3,FALSE,map)

map=pGMwrapper(geosets$storingen[geosets$storingen$ID_KLAK_Melding!=ID_KLAK_M,],"Omringende Storingen","ID_KLAK_Melding","Blue",3,FALSE,map)
map=pGMwrapper(geosets$moffen[geosets$moffen$ID_NAN!=ID_NAN_M,],"Omringende Moffen","Status_ID","Blue",3,FALSE,map)
map=pGMwrapper(geosets$kabels[geosets$kabels$ID_NAN!=ID_NAN_M,],"Omringende Kabels","Status_ID","Blue",3,FALSE,map)

map=pGMwrapper(geosets$kabels[geosets$kabels$ID_NAN==ID_NAN_M,],"Betrokken Kabels","Status_ID","Blue",3,FALSE,map)
map=pGMwrapper(geosets$moffen[geosets$moffen$ID_NAN==ID_NAN_M,],"Betrokken Moffen","Status_ID","Blue",3,FALSE,map)

map=pGMwrapper(geosets$storingen[geosets$storingen$ID_KLAK_Melding==ID_KLAK_M,],"Betrokken Storing","ID_KLAK_Melding","Blue",3,FALSE,map)
map=pGMwrapper(geosets$KLAKMelders[geosets$KLAKMelders$ID_KLAK_Melding==ID_KLAK_M,],"Betrokken Melders","ID_KLAK_Melding","Blue",3,TRUE,map,
               file=paste0(settings$Input_Datasets,"/geo.html"))

}

pGMwrapper = function (data,layername,group,colors,width,last=FALSE,previousmap=0,filename=""){
# Simple wrapper to make plotting more convenient
  options(warn=-1)
  txt1 = ifelse(!last,",add=TRUE","")
  txt2 = ifelse(class(previousmap)=="list",",previousMap=previousmap","")
  txt3 = ifelse(filename!="",",file = filename","")
  
  if(nrow(data)>0)
  {eval(parse(text=
               paste0("map=plotGoogleMaps(data,legend=FALSE,layerName=layername,zcol=group,colPalette = colors,strokeWeight=width",
               txt1,txt2,txt3
              ,")")))
  return(map)}
  else if(class(previousmap)=="list"){
    return(previousmap)
  }
  else{
    cat("Input is empty!\n")
  }
  options(warn=0)
}

# Redundant ----------------------------------
NORanalytics = function()
  
{
  load("C:/Datasets/AHAdata/2. Input Datasets/6. NOR/masterdataset_ELCVERBINDINGSDELEN.Rda")
  a = ValidatieSet$ID_NAN %in% masterdataset$ID_NAN
  barplot(table(a))  
  
  setkey(masterdataset,ID_NAN)
  setkey(ValidatieSet,ID_NAN)
  ValidatieSet=unique(ValidatieSet)
  masterdataset = unique(masterdataset)
  masterdataset[masterdataset$ID_NAN %in% ValidatieSet$ID_NAN]
  
  
  load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo.Rda"))
  
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_6_Spatial.Rda"))
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_4_Spatial.Rda"))
}

