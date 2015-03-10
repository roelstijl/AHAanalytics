# Contains all the geographical functions used in the project
# Asset Health Analytics, Roel Stijl, 2014-2015

AHA_RDCtoGPS = function(coordinates)
  # Converts for RDS to GPS coordinates
  # Created by R Stijl, Bearingpoint
  # ! legacy
{
  Convert_Coordinate_System (data.table( x = coordinates[,1,with=FALSE],y= coordinates[,2,with=FALSE])
                             ,from = "RDS", to = "lonlat",
                             xcol ="x",ycol="y", plotgooglemaps=F)[,list(Coo_X,Coo_Y)]
}

Convert_Coordinate_System = function(mindataset,from = "lonlat", to = "RDS",xcol ="LOC_X_COORDINAAT",ycol="LOC_Y_COORDINAAT", plotgooglemaps=F){
  # Function converts from coordinate system to other system and has the option to plot it
  # Takes the entire dataset, but could be faster if the input data is smaller or just xy
  # Expects a data table as entry and outputs a data table with coordinates added
  fromcrs = switch(from,lonlat = "+init=epsg:4326",RDS = "+init=epsg:28992",error("not supported, add CRS system"))
  tocrs   = switch(to,lonlat = "+init=epsg:4326",RDS = "+init=epsg:28992",error("not supported, add CRS system"))
  
  # Fix some issues with characters if present
  if(is.character(mindataset[[x]]))
  {
    mindataset[,lon:=as.numeric(strrep(mindataset[[xcol]],",","."))]
    mindataset[,lat:=as.numeric(strrep(mindataset[[ycol]],",","."))]
  }
  
  # Convert to desired coordinate system
  nona = (mindataset[[xcol]]!="")&(mindataset[[ycol]]!="")
  co    = mindataset[nona,]
  coordinates(co) = mindataset[nona ,list(lon,lat)]
  proj4string(co) <- CRS(fromcrs)
  
  # Plotting is required
  if(plotgooglemaps) plotGoogleMaps(co[1:min(ncol(co@data),15),2000])
  
  coo = coordinates(spTransform(co,CRS(tocrs)))
  mindataset[nona,Coo_X:=coo[,1]]
  mindataset[nona,Coo_Y:=coo[,2]]
  return(mindataset)
}

# Wrapper for the covnersion of MDSYS files to usable coordinates--------------------
processXY = function(file,mode,veld="Ligging",folder="1. BARlog"){
  load(paste0(settings$Ruwe_Datasets,"/",folder,"/",file,".Rda"));

  setnames(mindataset,veld,"veld")
  notveld = colnames(mindataset)[!colnames(mindataset)=="veld"]
  
  if (mode == "lines"){
    mindataset = AHA_Data_BAR_Geometry(mindataset[,list(ID_Object,veld)],mode,atype)
    save(mindataset,file=paste0(settings$Ruwe_Datasets,"/",folder,"/",file,"_Geospatial.Rda"))}
  else{
    mindataset = cbind(mindataset[,notveld,with=F],AHA_Data_BAR_Geometry(mindataset[,list(ID_Object,veld)],mode,atype))
    save(mindataset,file=paste0(settings$Ruwe_Datasets,"/",folder,"/",file,"_XY.Rda"))}
}

# Converts the BAR geoinfo to something usefull -----------------------------
AHA_Data_BAR_Geometry = function(dataset,mode="lines",atype="kabels",DT = "none"){
# MDsys is the collumn with the GEO-information ()
# Converts the following "MDSYS.SDO_GEOMETRY(2001,28992,MDSYS.SDO_POINT_TYPE(185462.693,436911.424,NULL),NULL,NULL)"
# cfg$modes: 
# lines (export plotable lines)
# beginend (export the beginning and end of each polygon)
# position (export the avg location of the polygon)

# Settings
cfg = list();
cfg$mode      = mode
cfg$noloops   = 20
cfg$pb        = pbarwrapper (title = paste0("AHA_Data_BAR_Geometry ",Sys.time()), label = "Starting...", max = cfg$noloops+1); pc=0;
cfg$atype     = ifelse(cfg$mode=="position","moffen","kabels") # Legacy

# cut the data into blocks, prevents memory issues
dataset[,loopy:=(ceil(seq(0.000000001,cfg$noloops,length.out=nrow(dataset))))]

# The magic
mdsysout =  dlply(dataset,
                  .(loopy),
                  calculatemdsys,
                   cfg = cfg,
                  .parallel  = settings$parallel)
   
setpbarwrapper (cfg$pb, cfg$noloops +1,label = "Done");

#   map= plotGoogleMaps(finaloutput,legend=FALSE,strokeColor = "Blue",strokeWeight = 100)
return(do.call(rbind,mdsysout))
}

# Loop over the values in order to prevent memory issues -------------------------------------
calculatemdsys = function(dataset,cfg=NA) {
  
 
  
  loopy = mean(dataset$loopy)
  mdsys = data.table(dataset)
  setpbarwrapper (cfg$pb, loopy,title = paste0("AHA_Data_BAR_GEOMETRY, loop: ", loopy," of ", cfg$noloops, " n=", length(mdsys)),label = "Splitting strings"); 
  mdsys = dataset$veld[!is.na(dataset$veld)]
  mdsys = strsplit(mdsys, "\\(|\\)"); 
  setpbarwrapper (cfg$pb, loopy,label = "Converting to data tables in list");
  
  # Extract the coordinated from the character array
  mdsys=switch(cfg$atype,
               kabels = {temp = llply(mdsys,function(x) t(matrix(as.numeric(do.call(rbind, strsplit(x[7],","))),2,)))
                         names(temp) = as.character(1:length(temp))
                         temp[which(!laply(temp,function(x) any(is.na(x))))]},
               
               moffen = laply(mdsys,function(x) t(matrix(as.numeric(strsplit(x[3],",")[[1]][1:2],2)))));   
  setpbarwrapper (cfg$pb, loopy,label = "Converting to selected output"); 
  
  # Preprocess the geospatial data
  mdsys= switch(cfg$mode,
                lines = llply(1:length(mdsys),
                              function(x) 
                              {return(Lines(list(Line(mdsys[x])),dataset[x,ID_Object]))}),
                
                beginend = llply(mdsys,function(x) cbind(x[1,],x[nrow(x),])),
                position = mdsys,
                points   = mdsys)
  
# Convert it to geospatial data
setpbarwrapper (cfg$pb, loopy,label = "Converting to geospatial output");
mdsys = switch(cfg$mode,
  lines = SpatialLines(mdsys,proj4string=CRS("+init=epsg:28992")),
  beginend = data.table(t(matrix(unlist(mdsys),4,length(mdsys)))),
  position = data.table(mdsys))

a  =  switch(cfg$mode,
             beginend = {setnames(mdsys,c("Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar"))},
             position = {setnames(mdsys,c("Coo_X","Coo_Y"))})
return(mdsys)
}

AHA_MDsysGeo_Conversion = function (){

#Written by Michiel Musterd - 23-02-2015
#---------------------------------------
#Function to convert geo data of the mdsys type to SpatialPolygon type for use in geoquery coupling
#This function takes mdsys input of the type: 
#MDSYS.SDO_GEOMETRY(2003,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1),MDSYS.SDO_ORDINATE_ARRAY(231088.1358,570469.305399999
#where the geometric data is available in the ordindate_array in x,y,x,y etc format

#This function was particularly written for the grondsoorten set, but can be fairly easily adapted to a different set


#Set the savefile location
savefile=paste0(settings$Ruwe_Datasets,"/23. Grondsoort/Grondsoorten_shp.Rda")

#first clean up the dataset a bit, we only need omschrijving (renamed to grondsoort) and SHAPE
mindataset=mindataset[,c("OMSCHRIJVI","SHAPE"),with=F]
setnames(mindataset,"OMSCHRIJVI","Grondsoort")

#class of Grondsoort should be factor
mindataset[,Grondsoort:=as.factor(Grondsoort)]


#read in the column of the dataset with the geo data
mdsys=mindataset$SHAPE


#check the length of each row in the set
charLength=nchar(mdsys)

#check where the word ORDINATE_ARRAY is located in each row
location=data.table(str_locate(mdsys, "ORDINATE_ARRAY"))

#we know that the geodata itself starts 2 characters AFTER
#the end of ORDINATE_ARRAY (because there is a bracket in between)
#return(location$end)
selecter=data.table(start=location$end+2,end=charLength-2)

geoData=substr(mdsys,selecter$start,selecter$end)

#Extract the X and Y coordinates row by row and store them as polygons
spatialList=list(1:length(geoData))
removeIDs=c((1:length(geoData))*0)
rownameShift=0

for (i in 1:length(geoData)){
  temp=as.numeric(unlist(strsplit(geoData[i],",")))
  
  if (is.na(selecter$start[i])){
    #this row should be removed from the entire set because there is no use in having it
    removeIDs[i]=1
    rownameShift=rownameShift+1
  }else
  {
    X=temp[seq(1,length(temp),by=2)]
    Y=temp[seq(2,length(temp),by=2)]
    setname=paste0(i-rownameShift)
    spatialList[i]=Polygons(list(Polygon(cbind(X,Y))),eval(setname))
  }
}
#remove the empty entries from all sets
spatialList[removeIDs==1]=NULL
mindataset=mindataset[!removeIDs,]

#Merge the list of polygons in SpatialPolygons
spatialset=SpatialPolygons(spatialList)  

#throw away the shape column in mindataset and save all sets
mindataset$SHAPE=NULL

#Group them with the original dataset
spatialsetdataframe=SpatialPolygonsDataFrame(spatialset,mindataset)


save(spatialset,spatialsetdataframe,mindataset,dataclasses,file=savefile)

return("Done") 
}

# Function calculates the PC6 of files ---------------------------------
processPC6 = function(file,mode,folder=paste0(settings$Ruwe_Datasets,"/","1. BARlog"),returndata=F){
  cat("starting\n")
  a=1
  
  switch (mode,
          naar= {
            load(paste0(folder,"/",file,".Rda"))
            setkeyv(masterdataset,c("Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar"))
            datatable = AHA_Data_Determine_PC(unique(masterdataset[,list(Coo_X_van,Coo_Y_van,Coo_Y_naar,Coo_X_naar)]),x="Coo_X_naar",y="Coo_Y_naar",PC="PC_6_naar")
            datatable = AHA_Data_Determine_PC(datatable,x="Coo_X_van",y="Coo_Y_van",PC="PC_6_van",extrainfo=TRUE)  
            
            setkeyv(masterdataset,c("Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar"))
            setkeyv(datatable,c("Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar"))
            mindataset=unique(datatable)[masterdataset]
            },
            
          van_naar= {
            load(paste0(folder,"/",file,"_XY.Rda"))
            datatable = AHA_Data_Determine_PC(mindataset[,list(Coo_X_van,Coo_Y_van,Coo_Y_naar,Coo_X_naar)],
            x="Coo_X_naar",y="Coo_Y_naar",PC="PC_6_naar")
            datatable = AHA_Data_Determine_PC(datatable,x="Coo_X_van",y="Coo_Y_van",PC="PC_6_van",extrainfo=TRUE)
            
            setkeyv(mindataset,c("Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar"))
            setkeyv(datatable,c("Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar"))
            mindataset=unique(datatable)[mindataset]
            },
          
          punt= {
            load(paste0(folder,"/",file,"_XY.Rda"));
            mindataset=AHA_Data_Determine_PC(mindataset,extrainfo=TRUE)}
  )
  if (returndata){
    save(mindataset,file=paste0(folder,"/",file,"_XY_PC6.Rda"),compress=F)
    return(mindataset)
  }else{
    save(mindataset,file=paste0(folder,"/",file,"_XY_PC6.Rda"),compress=F)
  }
}

# Determines the PC regions corresponding to XY coordinates ---------------------
AHA_Data_Determine_PC=function(datatable,x="Coo_X",y="Coo_Y",PC="PC_6",extrainfo=FALSE){
  # Function calculated the postal codes for regions that lack this (i.e BAR and NOR sets)
  # Prepare lines
  
  cfg=list()
  cfg$pb = pbarwrapper (title = paste0("AHA_Data_Determine_PC, ",as.character(Sys.time())), label = "Preparing lines for comparison to pc6 regions", min = 0, max = 10000, initial = 0, width = 450);
  try({datatable[[PC]] = NULL})
  
  # Load the datasets
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_6_Spatial.Rda"))
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_4_Spatial.Rda"))
  pc6$PC_4=substring(pc6$POSTCODE,1,4)
  pc6$POSTCODE=as.character(pc6$POSTCODE)
  
  proj4string(pc6) <- CRS("+init=epsg:28992")
  proj4string(pc4) <- CRS("+init=epsg:28992")
  
  # First check in what PC 4 region the points are
  setpbarwrapper (cfg$pb, 500,label = "Check what PC4 the points are in"); 
  datatable = datatable[!is.na(datatable[,x,with=F])[,1],]
  
  # Set the coordinates
  co = data.table(as.character(1:nrow(datatable)))
  datatable[,V1:=co$V1]
  coordinates(co) = datatable[,c(x,y),with=FALSE]
  proj4string(co) <- CRS("+init=epsg:28992")
  
  # Extract PC4
  ret = data.table(co %over% pc4)
  co$PC_4 = as.character(ret$PC4CODE)
  datatable[,PC_4 := as.character(ret$PC4CODE)]
  
  if(extrainfo){
    datatable[,Woonplaats     := as.character(ret$PC4NAAM)]
    datatable[,Gemeente       := as.character(ret$GEMNAAM)]
    datatable[,GemeenteCode   := as.character(ret$GEMCODE)]
  }
  
  # Next repeat proces with the PC 6 regions
  postcodes = sort(unique(datatable[datatable$PC_4 %in% pc6$PC_4,PC_4]))
  co        = co[!is.na(co$PC_4)&datatable$PC_4 %in% pc6$PC_4,]
  
  # The magic function, wont work with alply :-(
  ret = data.table(matrix(as.numeric(NA),nrow(co),8))
  setnames(ret,c(colnames(pc6@data)))
  ret[,OBJECTID:=as.integer(OBJECTID)]
  ret[,POSTCODE:=as.character(POSTCODE)]
  ret[,PC_4:=as.character(PC_4)]
  ret[,V1:=co$V1]
  
  for (pc in postcodes)
  {
    setpbarwrapper (cfg$pb, as.integer(pc),label = paste0("Calculating pc4: ",pc)); 
    set(ret,which(co$PC_4==pc),1:8,co[co$PC_4==pc,] %over% pc6[pc6$PC_4 == pc,])
  }
  
  # Stitch it all together
  setpbarwrapper (cfg$pb, 9900,label = paste0("Stitching results together"));
  setkey(ret,V1); setkey(datatable,V1)
  datatable = ret[,list(POSTCODE,V1)][datatable]
  datatable[,V1:=NULL]
  setnames(datatable,"POSTCODE",PC)
  
  # Done -------------------
  setpbarwrapper (cfg$pb,10000, label = "Done"); 
  
  return(datatable)
}