AHA_Data_BAR_GEOMETRY = function(mdsys,mode="polygons"){
  # MDsys is the collumn with the GEO-information ()
  # Converts the following "MDSYS.SDO_GEOMETRY(2001,28992,MDSYS.SDO_POINT_TYPE(185462.693,436911.424,NULL),NULL,NULL)"
  # Modes: 
  # polygons (export plotable polygons)
  # beginend (export the beginning and end of each polygon)
  # position (export the avg location of the polygon)
  #   cat("Starting cluster with multiple processor cores\n")
  
  #   tic();cl <<- makeCluster(getOption("cl.cores", 3));toc()
  
  cat("Extracting character\n"); tic()
  mdsys = mdsys[!is.na(mdsys)]
  mdsys = strsplit(mdsys, "\\(|\\)"); 
  toc(); cat("Converting to data tables in list\n")
  mdsys=llply(mdsys, 
              function(x) t(matrix(as.numeric(do.call(rbind, strsplit(x[7],","))),2,)) 
              ,.progress="text"); 
  names(mdsys) = as.character(1:length(mdsys))
  mdsys=mdsys[(which(!laply(mdsys,function(x)  any(is.na(x)))))]
  #   stopCluster(cl)
  
  cat("Converting to output\n"); tic()
  nu<<-1
  mdsys= switch(mode,
                polygons = llply(mdsys,function(x) 
                {nu<<-nu+1; Polygons(list(Polygon(rbind(x,x[(nrow(x)-1):1,]))),as.character(nu))},.progress="text"),
                beginend = llply(mdsys,function(x) cbind(x[1,],x[nrow(x),]),.progress="text"),
                position = llply(mdsys,function(x) sapply(x,mean)),.progress="text")
  
  toc(); cat("Converting to final\n"); tic()
  mdsys = switch(mode,
                 polygons = SpatialPolygons(mdsys,proj4string=CRS("+init=epsg:28992")),
                 beginend = data.table(t(matrix(unlist(mdsys),4,length(mdsys)))),
                 position = data.table(t(matrix(unlist(mdsys),2,length(mdsys)))))
  
  a = switch(mode,
             beginend = {setnames(finaloutput,c("Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar"))},
             position = {setnames(finaloutput,c("Coo_X","Coo_Y"))})
  toc()
  #   map= plotGoogleMaps(finaloutput,legend=FALSE,strokeColor = "Blue",strokeWeight = 100)
  return(mdsys)
}