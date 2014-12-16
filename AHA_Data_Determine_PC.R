AHA_Data_Determine_PC=function(datatable,x="Coo_X",y="Coo_Y",PC="PC_6"){
# Prepare Polygons -----------------------
  cat("Preparing Polygons for comparison to PC_6 regions\n");tic()
  polyset = switch(polygon,
     PC_6 = {load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_6_Spatial.Rda")); pc6},
     PC_4 = readShapePoly(paste0(settings$Ruwe_Datasets,"/10. BAG/PC4.shp")))
   proj4string(polyset) <- CRS("+init=epsg:28992")
   polyset$PC_4=as.integer(substring(polyset$POSTCODE,1,4))
   
# # Prepare datapoints --------------------------
if (FALSE)
  
  cat("Preparing dataset\n")
  datatable=assets$kabels[,c("Coo_X_naar","Coo_Y_naar","ID_unique","PC_6_van"),with=FALSE]
  datatable[,PC_4:= as.integer(substring(datatable[[PC]],1,4))]
  setkey(datatable,PC_4)
  datatable = datatable[as.logical(!is.na(datatable[,x,with=FALSE])),]  
  datatable = datatable[as.logical(!is.na(datatable[,"PC_4",with=FALSE])),]
  datatable = datatable[!duplicated(datatable,by=c(x,y,PC))]
  datatable[[PC]] = as.character(NA)

# Compare datapoints in polygons ----------------------
{  cat("Processing XY inside PC\n")
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
  }

datatable = datatable[!is.na(datatable[[x]])]
coordinates(datatable) = datatable[!is.na(datatable[[x]]),c(x,y),with=FALSE]
proj4string(datatable) <- CRS("+init=epsg:28992")
datatable[,PC_6:=gIntersects(coordinatenset, polyset$POSTCODE)]
a=gIntersects(datatable, polyset);toc()
toc();
  return(datatable)
}

# par(mfrow=c(1, 1))
# 
# polyset$PC_5=(substring(polyset$POSTCODE,1,5))
# p=(polyset[polyset$PC_4=="1011","PC_5"])
# spplot(p)
# 
# gIntersects(coordinatenset)
# spplot(p)
# rgeos::plot(gSimplify(p,tol=1));
# title("tol: 10")
# rgeos::plot(gSimplify(p,tol=5));
# title("tol: 20")
# rgeos::plot(gSimplify(p,tol=20));
# 
# object.size(gSimplify(p,tol=1));
# object.size(gSimplify(p,tol=10));
# object.size(gSimplify(p,tol=20));
