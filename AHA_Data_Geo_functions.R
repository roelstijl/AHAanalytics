
processXY = function(file,mode,atype){
  cat("Loading file\n");tic();
  load(paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,".Rda"));toc();
  cat(paste0("Starting ", mode," file: ",file, "\n"))
  
  veld = switch (file,
                 MH_NRG_MS_KABELS= "Ligging",
                 MH_NRG_LS_KABELS= "Ligging",
                 MH_NRG_MS_MOFFEN= "Locatie",
                 MH_NRG_LS_MOFFEN= "Lokatie")
  setnames(mindataset,veld,"veld")
  
  mindataset = switch (mode,
                       lines= SpatialLinesDataFrame(AHA_Data_BAR_Geometry(mindataset$veld,mode,atype),data=mindataset[,veld:=NULL]),
                       points  = AHA_Data_BAR_Geometry(mindataset$veld,mode,atype,mindataset),
                       cbind(mindataset,AHA_Data_BAR_Geometry(mindataset$veld,mode,atype)))
  try(mindataset[,veld:=NULL]) 
  
  cat("saving\n")
  if (mode == "polygons" | mode == "points"){
    save(mindataset,file=paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_Geospatial.Rda"))}
  else{
    save(mindataset,file=paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_XY.Rda"))}
}

AHA_Data_BAR_Geometry = function(mdsysori,index,mode="lines",atype="kabels",DT = "none"){
  # MDsys is the collumn with the GEO-information ()
  # Converts the following "MDSYS.SDO_GEOMETRY(2001,28992,MDSYS.SDO_POINT_TYPE(185462.693,436911.424,NULL),NULL,NULL)"
  # Modes: 
  # polygons (export plotable polygons)
  # beginend (export the beginning and end of each polygon)
  # position (export the avg location of the polygon)
  
  # Settings --------------------------
  cat(paste0("\n Started: ",as.character(Sys.time()),"\n"))
  
  noloops   = 20
  loopblock = ceil(seq(0.000000001,noloops,length.out=length(mdsysori)))
  pb        = tkProgressBar (title = "Import", label = "Starting...", min = 0, max = noloops, initial = 0, width = 450); pc=0;
  mdsysout = list()
  nu<<-1
  
  # Loop over the values in order to prevent memory issues -----------------
  for (loopy in 1:noloops)
  {
    mdsys = mdsysori[loopblock==loopy]
    setTkProgressBar (pb, loopy, 
                      title = paste0("AHA_Data_BAR_GEOMETRY, loop: ", loopy," of ", noloops, " n=", length(mdsys)),
                      label = "Splitting strings"); 
    
    mdsys = mdsys[!is.na(mdsys)]
    mdsys = strsplit(mdsys, "\\(|\\)"); 
    setTkProgressBar (pb, loopy,label = "Converting to data tables in list");
    
    # Extract the coordinated from the character array
    mdsys=switch(atype,
                 kabels = {temp = llply(mdsys,function(x) t(matrix(as.numeric(do.call(rbind, strsplit(x[7],","))),2,)))
                           names(temp) = as.character(1:length(temp))
                           temp[which(!laply(temp,function(x) any(is.na(x))))]},
                 
                 moffen = laply(mdsys,function(x) t(matrix(as.numeric(strsplit(x[3],",")[[1]][1:2],2))))
    ); 
    
    setTkProgressBar (pb, loopy,label = "Converting to selected output"); 
    
    # Preprocess the geospatial data --------------------------
    
    mdsys= switch(mode,
                  lines = llply(mdsys,function(x) 
                  {Lines(list(Line(x)),as.character(nu));
                   nu<<-nu+1}),
                  
                  beginend = llply(mdsys,function(x) cbind(x[1,],x[nrow(x),])),
                  position = mdsys,
                  points   = mdsys)
    
    # Convert it to geospatial data --------------------------
    setTkProgressBar (pb, loopy,label = "Converting to geospatial output");
    mdsys = switch(mode,
                   lines = SpatialLines(mdsys,proj4string=CRS("+init=epsg:28992")),
                   points   = {ifelse(is.character(DT),
{temp = data.table(Index = nu:(nu-1+nrow(mdsys)))},
{temp = DT[nu:(nu-1+nrow(mdsys))]})
nu<<-nrow(mdsys)+1
temp = temp[!is.na(mdsys[,1]),]
coordinates(temp) = mdsys[!is.na(mdsys[,1]),]
proj4string(temp) = CRS("+init=epsg:28992")
temp},
beginend = data.table(t(matrix(unlist(mdsys),4,length(mdsys)))),
position = data.table(mdsys))

a  =  switch(mode,
             beginend = {setnames(mdsys,c("Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar"))},
             position = {setnames(mdsys,c("Coo_X","Coo_Y"))})
mdsysout[[loopy]] = mdsys
  }

setTkProgressBar (pb, loopy,label = "Done");

#   map= plotGoogleMaps(finaloutput,legend=FALSE,strokeColor = "Blue",strokeWeight = 100)
return(do.call(rbind,mdsysout))
}

processPC6 = function(file,mode){
  # Function calculates the PC6 of files-----------------
  cat("starting\n")
  a=1
  
  switch (mode,
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

AHA_Data_Determine_PC=function(datatable,x="Coo_X",y="Coo_Y",PC="PC_6",extrainfo=FALSE){
  # Function calculated the postal codes for regions that lack this (i.e BAR and NOR sets)
  #
  # Prepare Polygons -----------------------
  pb <<- tkProgressBar (title = paste0("AHA_Data_Determine_PC, ",as.character(Sys.time())), label = "Preparing Polygons for comparison to pc6 regions", min = 0, max = 10000, initial = 0, width = 450);
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_6_Spatial.Rda"))
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_4_Spatial.Rda"))
  pc6$PC_4=substring(pc6$POSTCODE,1,4)
  pc6$POSTCODE=as.character(pc6$POSTCODE)
  
  proj4string(pc6) <- CRS("+init=epsg:28992")
  proj4string(pc4) <- CRS("+init=epsg:28992")
  
  # First check in what PC 4 region the points are------------------
  setTkProgressBar (pb, 500,label = "Check what PC4 the points are in"); 
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
  
  # Next repeat proces with the PC 6 regions-----------------
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
    setTkProgressBar (pb, as.integer(pc),label = paste0("Calculating pc4: ",pc)); 
    set(ret,which(co$PC_4==pc),1:8,co[co$PC_4==pc,] %over% pc6[pc6$PC_4 == pc,])
  }
  
  # Stitch it all together
  setTkProgressBar (pb, 9900,label = paste0("Stitching results together"));
  setkey(ret,V1); setkey(datatable,V1)
  datatable = ret[,list(POSTCODE,V1)][datatable]
  datatable[,V1:=NULL]
  setnames(datatable,"POSTCODE",PC)
  
  # Done -------------------
  setTkProgressBar (pb,10000, label = "Done"); 
  
  return(datatable)
}