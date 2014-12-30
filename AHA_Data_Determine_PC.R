AHA_Data_Determine_PC=function(datatable,x="Coo_X",y="Coo_Y",PC="PC_6"){
  # Function calculated the postal codes for regions that lack this (i.e BAR and NOR sets)
  # Prepare Polygons -----------------------
  
  pb <<- tkProgressBar (title = paste0("AHA_Data_Determine_PC, ",as.character(Sys.time())), label = "Preparing Polygons for comparison to pc6 regions", min = 0, max = 10000, initial = 0, width = 450);
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_6_Spatial.Rda"))
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_4_Spatial.Rda"))
  pc6$PC_4=substring(pc6$POSTCODE,1,4)
  
  proj4string(pc6) <- CRS("+init=epsg:28992")
  proj4string(pc4) <- CRS("+init=epsg:28992")
  
  # First check in what PC 4 region the points are------------------
  setTkProgressBar (pb, 500,label = "Check what PC4 the points are in"); 
  co = data.table(as.character(1:nrow(datatable)))
  datatable[,V1:=as.character(1:nrow(datatable))]
  coordinates(co) = datatable[,c(x,y),with=FALSE]
  proj4string(co) <- CRS("+init=epsg:28992")
  ret = data.table(co %over% pc4)
  co$PC_4 = as.character(ret$PC4CODE)
  datatable[,PC_4 := as.character(ret$PC4CODE)]
  datatable[,Woonplaats := as.character(ret$PC4NAAM)]
  datatable[,Gemeente   := as.character(ret$GEMNAAM)]
  datatable[,GemeenteCode   := as.character(ret$GEMCODE)]
  
  # Next repeat proces with the PC 6 regions-----------------
  postcodes = sort(unique(datatable[datatable$PC_4 %in% pc6$PC_4,PC_4]))
  co        = co[!is.na(co$PC_4),]
  postcodes = postcodes[1:500]
  
  ret = 
    alply(
      .data= postcodes,
      .margins=1,
      .fun = function(x) {
        setTkProgressBar (pb, as.integer(x),label = paste0("Calculating pc4: ",x)); 
        data.table(co[co$PC_4==x,] %over% pc6[pc6$PC_4 == x,])
      })
  
  setkey(ret,V1); setkey(datatable,V1)
  datatable = ret[,c(PC,"V1"),with=FALSE][datatable]
  datatable[,V1:=NULL]
  
  # Done -------------------
  setTkProgressBar (pb,10000, label = "Done"); 
  
  return(datatable)
}