Tableau_Create_Polygons = function(fileout="polygons",sources="spd",combine = FALSE)
{
# Roel Stijl, Bearingpoint 2015. 
# Converts shp or R spatial files to tableau.
# Select the source and which file to output (filename)
# Combine selects wether to attach the entire data table to the spatials (space inefficient)
# Next select the file(s) (prompted) to import and wait
#  
# Settings ---------------------
FileName = paste0(settings$Visuals,"/2. Tableau Polygons/",fileout)

# Load stuff -----------------------
switch (sources,
# shapefile
sf = {
  cat("Select a shapefile \n")
  ShapeFile <- readShapeSpatial(file.choose())
#   Data <- as(ShapeFile, "data.frame")
  Data <- as(ShapeFile, "data.table")
  Data$PolygonID <- as.numeric(rownames(Data))
},

# spatialpolygonsdataframe
spd = {
 cat("Select a spatialpolygons R file \n")
 fc = file.choose(); cat(paste0("Loading ",fc,"........\n\n" ))
 load(fc);  ShapeFile = mindataset
 
 cat("Select a datatable R file \n")
 fc = file.choose(); cat(paste0("Loading ",fc,"........\n\n" ))
 load(fc);  Data      = mindataset
 Data$PolygonID <- as.numeric(rownames(Data))
 remove(mindataset)
},

# else
 error("Invalid source filetype methode")
)
 
# Extracts the coodinates and polygon IDs -----------------------
Polygons <- slot(ShapeFile,"polygons")
# coordinates = llply(Polygons,createpoly, .progress = "text")
coordinates = data.tableldply(Polygons,createpoly, .progress = "text")
 
 out = AHA_RDCtoGPS(coordinates[,list(Longitude,Latitude)])
  coordinates[Longitude:= out$V1]
  coordinates[Latitude := out$V2]
  
if (combine) {coordinates <- merge(Data,coordinates)}

 write.csv(coordinates, paste0(FileName,".csv"), row.names = FALSE)
}

# Create the polygons ------------------------------
createpoly = function (Polygon)
{  
  ID <- slot(Polygon, "ID")
  coords <- data.frame(slot(slot(Polygon,"Polygons")[[1]],"coords"))
  coords$PlotOrder <- c(1:nrow(coords))
  
  return(
    data.frame(
      
        Longitude = coords[,1],
        Latitude  = coords[,2],
        PolygonID = rep(ID,nrow(coords)),
        PlotOrder = c(1:nrow(coords))
  ))
}