AHA_Data_BAR_GEOMETRY = function(mdsys,mode="polygons"){
  # MDsys is the collumn with the GEO-information ()
  # Converts the following "MDSYS.SDO_GEOMETRY(2001,28992,MDSYS.SDO_POINT_TYPE(185462.693,436911.424,NULL),NULL,NULL)"
  # Modes: 
  # polygons (export plotable polygons)
  # beginend (export the beginning and end of each polygon)
  # position (export the avg location of the polygon)
  cl <- makeCluster(getOption("cl.cores", 3))
  
  output = list(); 
  
  cat("Converting to list of coordinates\n"); tic()
  seperated = strsplit(mdsys, "\\(|\\)"); 
  data=parLapply(cl,seperated, function(x) t(matrix(as.numeric(do.call(rbind, strsplit(x[7],","))),2,)) ); 
  names(data) = as.character(1:length(data))
  
  toc(); cat("Converting to output\n"); tic()
  nu<<-1
  output= switch(mode,
                       polygons = lapply(data,function(x) 
                         {nu<<-nu+1; Polygons(list(Polygon(rbind(x,x[(nrow(x)-1):1,]))),as.character(nu))}),
                       beginend = parLapply(cl,data,function(x) cbind(x[1,],x[nrow(x),])),
                       position = parLapply(cl,data,function(x) sapply(x,mean)))
  
    
  
  finaloutput = switch(mode,
                       polygons = SpatialPolygons(output,proj4string=CRS("+init=epsg:28992")),
                       beginend = data.table(t(matrix(unlist(output),4,length(output)))),
                       position = data.table(t(matrix(unlist(output),2,length(output)))))
  
            a = switch(mode,
                       beginend = {setnames(finaloutput,c("Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar"))},
                       position = {setnames(finaloutput,c("Coo_X","Coo_Y"))})
  toc()
#   map= plotGoogleMaps(finaloutput,legend=FALSE,strokeColor = "Blue",strokeWeight = 100)
  stopCluster(cl)
  return(finaloutput)
}