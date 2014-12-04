Tableau_Create_Polygons = function()
{
 ShapeFile <- readShapeSpatial(file.choose())
 FileName = paste0(settings$Visuals,"/PC_4")
 
 Data <- as(ShapeFile, "data.frame")
 Data$PolygonID <- as.numeric(rownames(Data))
 
 #extracts the coodinates and polygon IDs
 Polygons <- slot(ShapeFile,"polygons")
 coordinates <- list(Latitude = numeric(0),
                     Longitude = numeric(0),
                     PolygonID = numeric(0),
                     PlotOrder = numeric(0))
 
 #A slow looping aproach
 for(i in 1:length(Polygons)){
   Polygon <- Polygons[[i]]
   ID <- slot(Polygon, "ID")
   coords <- data.frame(slot(slot(Polygon,"Polygons")[[1]],"coords"))
   coords$PlotOrder <- c(1:nrow(coords))
   coordinates$Longitude <- c(coordinates$Longitude, coords[,1])
   coordinates$Latitude <- c(coordinates$Latitude, coords[,2])
   coordinates$PolygonID <- c(coordinates$PolygonID, rep(ID,nrow(coords)))
   coordinates$PlotOrder <- c(coordinates$PlotOrder, c(1:nrow(coords)))
 }
 
 out = AHA_RDCtoGPS(data.table(cbind(coordinates$Longitude,coordinates$Latitude)))
  coordinates$Longitude= out$V1
  coordinates$Latitude = out$V2

 CombinedData <- merge(Data,coordinates)
 
 filename <- paste(FileName,".csv", sep = "")
 write.csv(CombinedData, filename, row.names = FALSE)
}