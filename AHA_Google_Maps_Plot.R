AHA_Google_Maps_Plot = function(PC_4) 
{
# This function will plot the data files used in project AHA onto google maps
# Input is a series of PC4 areas or a single PC4 area PC_4 = 6810:6823 for Arnhem
# Method include proxy 
  
library("plotGoogleMaps")

# Load existing data into global environment
if (!exists("moffen")){
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data.Rda"),envir=globalenv())
}

# Data prep kabels
kabels[kabels$DateRemoved=="","Change"] = "Added"
kabels[!kabels$DateRemoved=="","Change"] = "Removed"
kabels_geo = kabels[!is.na(kabels$Coo_X_van) & pmatch(as.numeric(substring(kabels$PC_XY_van,1,4)),PC_4,nomatch=0,duplicates.ok=TRUE)>0,]
lst=list()
for(m in 1:length(kabels_geo[,1])) {
  a = as.matrix(kabels_geo[m,c("Coo_X_van","Coo_X_naar","Coo_Y_van","Coo_Y_naar")]);colnames(a)=NULL
  lst[[m]]=Polygons(list(Polygon(cbind (a[1,c(1,2,1)],a[1,c(3,4,3)]))),row.names(kabels_geo)[m])
}
kabels_geo_rem= SpatialPolygonsDataFrame(SpatialPolygons(lst[which(kabels_geo$Change=="Removed")],proj4string=CRS("+init=epsg:28992")),kabels_geo[kabels_geo$Change=="Removed",])
kabels_geo_add= SpatialPolygonsDataFrame(SpatialPolygons(lst[which(kabels_geo$Change=="Added")],proj4string=CRS("+init=epsg:28992")),kabels_geo[kabels_geo$Change=="Added",])
map1=plotGoogleMaps(kabels_geo_rem,legend=FALSE,layerName="Kabels_Removed",zcol="DateAdded",strokeColor="#00FF99",add=TRUE)
map2=plotGoogleMaps(kabels_geo_add,legend=FALSE,layerName="Kabels_Added",zcol="DateRemoved",strokeColor="#00CCFF",add=TRUE,previousMap=map1)

# Data prep moffen
moffen[moffen$DateRemoved=="","Change"] = "Added"
moffen[!moffen$DateRemoved=="","Change"] = "Removed"
moffen$PC_4=substring(moffen$PC_XY,1,4)
moffen_geo = moffen[!is.na(moffen$Coo_X) & pmatch(as.numeric(substring(moffen$PC_XY,1,4)),PC_4,nomatch=0,duplicates.ok=TRUE)>0,]
coordinates(moffen_geo) = (moffen_geo[,c("Coo_X","Coo_Y")])
proj4string(moffen_geo) <- CRS("+init=epsg:28992")
map3=plotGoogleMaps(moffen_geo[moffen_geo$Change=="Added",],legend=FALSE,layerName="Moffen_Added",zcol="DateAdded",add=TRUE,colPalette="#00FF99",previousMap=map2)
map4=plotGoogleMaps(moffen_geo[moffen_geo$Change=="Removed",],legend=FALSE,layerName="Moffen_Removed",zcol="DateRemoved",add=TRUE,colPalette="#00CCFF",previousMap=map3)

# Data prep KLAK_LS
KLAK_LS_geo = KLAK_LS[!is.na(KLAK_LS$Co_X)& pmatch(as.numeric(substring(KLAK_LS$PC_6,1,4)),PC_4,nomatch=0,duplicates.ok=TRUE)>0,]
coordinates(KLAK_LS_geo) = KLAK_LS_geo[,c("Co_X","Co_Y")]
proj4string(KLAK_LS_geo) <- CRS("+init=epsg:28992")
map5=plotGoogleMaps(KLAK_LS_geo,layerName="LS Storingen",zcol="Maand",add=TRUE,legend=FALSE,colPalette="#DB4D4D",previousMap=map4)

# Data prep KLAK_MS
KLAK_MS_geo = KLAK_MS[!is.na(KLAK_MS$Co_X)& pmatch(as.numeric(substring(KLAK_MS$PC_6,1,4)),PC_4,nomatch=0,duplicates.ok=TRUE)>0,]
coordinates(KLAK_MS_geo) = KLAK_MS_geo[,c("Co_X","Co_Y")]
proj4string(KLAK_MS_geo) <- CRS("+init=epsg:28992")
map6=plotGoogleMaps(KLAK_MS_geo,legend=FALSE,layerName="MS Storingen",zcol="Maand",colPalette="#FF3300",previousMap=map5)

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