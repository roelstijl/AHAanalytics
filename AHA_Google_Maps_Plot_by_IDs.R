AHA_Google_Maps_Plot_by_IDs = function(file,ID_KLAK, ID_Asset,Type_KLAK="LS",Type_Asset="moffen") 
{
# This function will plot the data files used in project AHA onto google maps
# Input is a series of PC4 areas or a single PC4 area PC_4 = 6810:6823 for Arnhem
# file is a file formatted for this function such as:
# file= "20141010_Moffen"
# Method include proxy 
  
  load(paste0(settings$Results,"/1. Proxy mijlpaal 1/", file,."R"))
# Loop z times, create plots from entries z (will generate 1 browser tab per run!) - errors are ignored.
  for (z in 1:10)
    {
  ID_Asset = gestoorde_moffen[z,7:11]
  ID_Asset = ID_Asset[!is.na(ID_Asset)]
  ID_KLAK  = gestoorde_moffen$ID_KLAK[z]
  
library("plotGoogleMaps")

# Load existing data into global environment
if (!exists("moffen")){
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data.Rda"),envir=globalenv())
}
map = list()


# Data prep kabels
if (Type_Asset =="kabels"){
kabels[kabels$DateRemoved=="","Change"] = "Added"
kabels[!kabels$DateRemoved=="","Change"] = "Removed"
kabels_geo = kabels[!is.na(kabels$Coo_X_van) & invwhich(match(ID_Asset,kabels$ID_Bron), length(kabels$Coo_X_van)),]
lst=list()
for(m in 1:length(kabels_geo[,1])) {
  a = as.matrix(kabels_geo[m,c("Coo_X_van","Coo_X_naar","Coo_Y_van","Coo_Y_naar")]);colnames(a)=NULL
  lst[[m]]=Polygons(list(Polygon(cbind (a[1,c(1,2,1)],a[1,c(3,4,3)]))),row.names(kabels_geo)[m])
}
kabels_geo_rem= SpatialPolygonsDataFrame(SpatialPolygons(lst[which(kabels_geo$Change=="Removed")],proj4string=CRS("+init=epsg:28992")),kabels_geo[kabels_geo$Change=="Removed",])
kabels_geo_add= SpatialPolygonsDataFrame(SpatialPolygons(lst[which(kabels_geo$Change=="Added")],proj4string=CRS("+init=epsg:28992")),kabels_geo[kabels_geo$Change=="Added",])
map[[length(map)+1]]=plotGoogleMaps(kabels_geo_rem,legend=FALSE,layerName="Kabels_Removed",zcol="DateAdded",strokeColor="#00FF99",add=TRUE)
map[[length(map)+1]]=plotGoogleMaps(kabels_geo_add,legend=FALSE,layerName="Kabels_Added",zcol="DateRemoved",strokeColor="#00CCFF",add=TRUE,previousMap=map[[length(map)]])
}

# Data prep moffen
try(
if (Type_Asset =="moffen"){
moffen[moffen$DateRemoved=="","Change"] = "Added"
moffen[!moffen$DateRemoved=="","Change"] = "Removed"
moffen$PC_4=substring(moffen$PC_XY,1,4)
moffen_geo = moffen[!is.na(moffen$Coo_X) &  invwhich(match(ID_Asset,moffen$ID_Bron), length(moffen$Coo_X)),]
coordinates(moffen_geo) = (moffen_geo[,c("Coo_X","Coo_Y")])
proj4string(moffen_geo) <- CRS("+init=epsg:28992")
if (length(map)<1){
  map[[length(map)+1]]=plotGoogleMaps(moffen_geo[moffen_geo$Change=="Added",],legend=FALSE,layerName="Moffen_Added",zcol="DateAdded",add=TRUE,colPalette="#00FF99")
}else{
  map[[length(map)+1]]=plotGoogleMaps(moffen_geo[moffen_geo$Change=="Added",],legend=FALSE,layerName="Moffen_Added",zcol="DateAdded",add=TRUE,colPalette="#00FF99",previousMap=map[[length(map)]])
}

map[[length(map)+1]]=plotGoogleMaps(moffen_geo[moffen_geo$Change=="Removed",],legend=FALSE,layerName="Moffen_Removed",zcol="DateRemoved",add=TRUE,colPalette="#00CCFF",previousMap=map[[length(map)]])
},silent=TRUE)

# Data prep KLAK_LS
try(
if (Type_KLAK == "LS"){
KLAK_LS_geo = KLAK_LS[!is.na(KLAK_LS$Co_X) &  invwhich(match(as.numeric(ID_KLAK),KLAK_LS$ID_KLAK_Melding), length(KLAK_LS$ID_KLAK_Melding)),]
coordinates(KLAK_LS_geo) = KLAK_LS_geo[,c("Co_X","Co_Y")]
proj4string(KLAK_LS_geo) <- CRS("+init=epsg:28992")
map[[length(map)+1]]=plotGoogleMaps(KLAK_LS_geo,layerName="LS Storingen", 
                                    filename = paste0("Moffen_Nr",ID_KLAK,"_PC6_",KLAK_LS_geo$PC_6,"_Maand_",KLAK_LS_geo$Maand,".html"),
                                    zcol="Maand",legend=FALSE,colPalette="#DB4D4D",previousMap=map[[length(map)]])
},silent=TRUE)

# Data prep KLAK_MS
if (Type_KLAK == "MS"){  
KLAK_MS_geo = KLAK_MS[!is.na(KLAK_MS$Co_X)&  invwhich(match(ID_KLAK,KLAK_MS$ID_KLAK_Melding), length(KLAK_MS$ID_KLAK_Melding)),]
coordinates(KLAK_MS_geo) = KLAK_MS_geo[,c("Co_X","Co_Y")]
proj4string(KLAK_MS_geo) <- CRS("+init=epsg:28992")
map[[length(map)+1]]=plotGoogleMaps(KLAK_MS_geo,legend=FALSE,layerName="MS Storingen",zcol="Maand",colPalette="#FF3300",previousMap=map[[length(map)]])
}
}
}

fixnumber = function(x) {
  val= strsplit(x,",")[[1]];
  
  if (suppressWarnings(!is.na(as.numeric(val[1])))){
    len=length(val); cor=ifelse(nchar(tail(val,1))==2,100,1000)
    if(len==1) {a=val[1]
    } else if(len==2) {
      a=(as.numeric(val[1])+as.numeric(val[2])/cor)
    } else if(len==3) {
      a=(as.numeric(val[1])*1000+as.numeric(val[2])+as.numeric(val[3])/cor)
    }
  }
  else{
    a=NA
  }
  #cat(paste0(a,", "))
  return(as.numeric(a))
}

invwhich = function(indices, totlength) is.element(seq_len(totlength), indices)
