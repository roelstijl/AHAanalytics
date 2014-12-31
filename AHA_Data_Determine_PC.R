AHA_Data_Determine_PC=function(datatableinit="global",x="Coo_X",y="Coo_Y",PC="PC_6",extrainfo=FALSE){
# Function calculated the postal codes for regions that lack this (i.e BAR and NOR sets)
#
# Prepare Polygons -----------------------
if(class (datatableinit)!="character")
{datatable = datatableinit; rm("datatableinit")}

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

if(class(datatable)!="character")
  {return(datatable)}
}