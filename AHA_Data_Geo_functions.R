# Contains all the geographical functions used in the project

# Wrapper for the covnersion of MDSYS files to usable coordinates--------------------
processXY = function(file,mode,atype){
  load(paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,".Rda"));
  cat(paste0("Starting ", mode," file: ",file, "\n"))
  
  veld = switch (file,
                 MH_NRG_MS_KABELS= "Ligging",
                 MH_NRG_LS_KABELS= "Ligging",
                 MH_NRG_MS_MOFFEN= "Locatie",
                 MH_NRG_LS_MOFFEN= "Lokatie")
  setnames(mindataset,veld,"veld")
  notveld = colnames(mindataset)[!colnames(mindataset)=="veld"]
  
  if (mode == "lines"){
    mindataset = AHA_Data_BAR_Geometry(mindataset[,list(ID_Object,veld)],mode,atype)
    save(mindataset,file=paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_Geospatial.Rda"))}
  else{
    mindataset = cbind(mindataset[,notveld,with=F],AHA_Data_BAR_Geometry(mindataset[,list(ID_Object,veld)],mode,atype))
    save(mindataset,file=paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_XY.Rda"))}
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
cfg$atype     = atype
cfg$mode      = mode
cfg$parallel  = T
cfg$noloops   = 20
cfg$loopblock = ceil(seq(0.000000001,cfg$noloops,length.out=nrow(dataset))) # cut into blocks
cfg$pb        = pbarwrapper (title = paste0("AHA_Data_BAR_Geometry ",Sys.time()), label = "Starting...", max = cfg$noloops+1); pc=0;

dataset[,loopy:=(cfg$loopblock)]

# Parallel functions
if (cfg$parallel)
  setpbarwrapper (cfg$pb, 0,label = "STarting parallel functions");
{cl <- makeCluster(7)
 registerDoParallel(cl)}

mdsysout =  dlply(dataset,
                  .(loopy),
                  calculatemdsys,
                   cfg = cfg,
                  .parallel  = cfg$parallel)
   
setpbarwrapper (cfg$pb, cfg$noloops +1,label = "Done");

#   map= plotGoogleMaps(finaloutput,legend=FALSE,strokeColor = "Blue",strokeWeight = 100)
return(do.call(rbind,mdsysout))
}


# Loop over the values in order to prevent memory issues -------------------------------------
calculatemdsys = function(dataset,cfg=NA) {
  loopy = mean(dataset$loopy)
  dataset = data.table(dataset)
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

# Function calculates the PC6 of files ---------------------------------
processPC6 = function(file,mode){
  cat("starting\n")
  a=1
  
  switch (cfg$mode,
          van_naar= {
            load(paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_XY.Rda"));
            cat("Coordinates naar\n")
            datatable = AHA_Data_Determine_PC(mindataset[,list(Coo_X_van,Coo_Y_van,Coo_Y_naar,Coo_X_naar)],
                                              x="Coo_X_naar",y="Coo_Y_naar",PC="PC_6_naar")
            cat("Coordinates van\n")
            datatable = AHA_Data_Determine_PC(datatable,x="Coo_X_van",y="Coo_Y_van",PC="PC_6_van",extrainfo=TRUE)  
            mindataset = cbind(mindataset,datatable[,list(PC_6_naar,PC_6_van,Woonplaats,Gemeente,GemeenteCode)])},
          
          punt= {
            load(paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_XY.Rda"));
            mindataset=AHA_Data_Determine_PC(mindataset,extrainfo=TRUE)}
  )
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_XY_PC6.Rda"))
}

# Determines the PC regions corresponding to XY coordinates ---------------------
AHA_Data_Determine_PC=function(datatable,x="Coo_X",y="Coo_Y",PC="PC_6",extrainfo=FALSE){
  # Function calculated the postal codes for regions that lack this (i.e BAR and NOR sets)
  #
  # Prepare lines
  cfg=list()
  cfg$pb <<- pbarwrapper (title = paste0("AHA_Data_Determine_PC, ",as.character(Sys.time())), label = "Preparing lines for comparison to pc6 regions", min = 0, max = 10000, initial = 0, width = 450);
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_6_Spatial.Rda"))
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_4_Spatial.Rda"))
  pc6$PC_4=substring(pc6$POSTCODE,1,4)
  pc6$POSTCODE=as.character(pc6$POSTCODE)
  
  proj4string(pc6) <- CRS("+init=epsg:28992")
  proj4string(pc4) <- CRS("+init=epsg:28992")
  
  # First check in what PC 4 region the points are
  setpbarwrapper (cfg$pb, 500,label = "Check what PC4 the points are in"); 
  datatable = datatable[(!is.na(datatable[,x,with=FALSE]))[,1],]
  co = data.table(as.character(1:nrow(datatable)))
  datatable[,V1:=co$V1]
  
  # Set the coordinates
  coordinates(co) = datatable[,c(x,y),with=FALSE]
  proj4string(co) <- CRS("+init=epsg:28992")
  
  # Extract PC4
  ret = data.table(co %over% pc4)
  co$PC_4 = as.character(ret$PC4CODE)
  datatable[,PC_4 := as.character(ret$PC4CODE)]
  
  if(extrainfo){
    datatable[,Woonplaats := as.character(ret$PC4NAAM)]
    datatable[,Gemeente   := as.character(ret$GEMNAAM)]
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