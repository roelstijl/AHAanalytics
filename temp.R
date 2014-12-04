AHA_Data_BAR_GEOMETRY = function(mdsys,mode="polygons"){
  #   mdsys = "MDSYS.SDO_GEOMETRY(2001,28992,MDSYS.SDO_POINT_TYPE(185462.693,436911.424,NULL),NULL,NULL)"
  cl <- makeCluster(getOption("cl.cores", 3))
  
  output = list(); 
  mdsys  = mindataset$Ligging[1:100]
  tic(); seperated = strsplit(mdsys, "\\(|\\)"); toc()
  tic(); data=parLapply(cl,seperated, function(x) t(matrix(as.numeric(do.call(rbind, strsplit(x[7],","))),2,)) ); toc()
  names(data) = as.character(1:length(data))
  
  tic(); nu<<-1
  output= switch(mode,
                       polygons = lapply(data,function(x) 
                         {nu<<-nu+1; Polygons(list(Polygon(rbind(x,x[(nrow(x)-1):1,]))),as.character(nu))}),
                       beginend = parLapply(cl,data,function(x) cbind(x[1,],x[nrow(x),])),
                       position = parLapply(cl,data,function(x) sapply(x,mean)))
  toc()
    
  tic()
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