AHA_Data_BAR_GEOMETRY = function(mdsysori,mode="polygons",atype="kabels",DT = "none"){
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
                  polygons = llply(mdsys,function(x) 
                  {Polygons(list(Polygon(rbind(x,x[(nrow(x)-1):1,]))),as.character(nu));
                   nu<<-nu+1}),
                  
                  beginend = llply(mdsys,function(x) cbind(x[1,],x[nrow(x),])),
                  position = mdsys,
                  points   = mdsys)
    
# Convert it to geospatial data --------------------------
    setTkProgressBar (pb, loopy,label = "Converting to geospatial output");
    mdsys = switch(mode,
                   polygons = SpatialPolygons(mdsys,proj4string=CRS("+init=epsg:28992")),
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