AHA_Data_Determine_PC=function(datatable,polygon="PC_6",x="Coo_X",y="Coo_Y"){
# Prepare Polygons -----------------------
  cat("Preparing Polygons for comparison to PC_6 regions\n");tic()
  polyset = switch(polygon,
     PC_6 = {load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_6_Spatial.Rda")); pc6},
     PC_4 = readShapePoly(paste0(settings$Ruwe_Datasets,"/10. BAG/PC4.shp")))
   proj4string(polyset) <- CRS("+init=epsg:28992")
   polyset$PC_4=as.integer(substring(polyset$POSTCODE,1,4))
   
# Prepare datapoints --------------------------
  cat("Preparing dataset\n")
  datatable=assets$kabels[,c("Coo_X_naar","Coo_Y_naar","ID_unique","PC_6_van"),with=FALSE]

  datatable[,PC_4:= as.integer(substring(datatable$PC_6_van,1,4))]
  setkey(datatable,PC_4)
  datatable = datatable[as.logical(!is.na(datatable[,x,with=FALSE])),]  
  datatable = datatable[as.logical(!is.na(datatable[,"PC_4",with=FALSE])),]
  datatable = datatable[!duplicated(datatable,by=c("Coo_X_naar","Coo_Y_naar","PC_6_van"))]
  datatable$PC_6 = as.character(NA)

# Compare datapoints in polygons ----------------------
  cat("Processing XY inside PC\n")
  postcodes = unique(datatable$PC_4)
  pb <- txtProgressBar(min = 0, max = length(postcodes), initial = 0,style = 3)
  for (n in 1:length(postcodes))
  {    
    coordinatenset = datatable[PC_4==postcodes[n]]
    coordinates(coordinatenset) = coordinatenset[,c(x,y),with=FALSE]
    proj4string(coordinatenset) <- CRS("+init=epsg:28992")
    try(datatable[PC_4 == postcodes[n],PC_6:=over(coordinatenset,polyset[polyset$PC_4 %in% postcodes[n],])$POSTCODE])
    setTxtProgressBar(pb, n)
  }
  toc();
  return(datatable)
}