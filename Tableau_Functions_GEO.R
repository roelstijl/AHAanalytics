
Tableau_Create_Polygons_MDSYS = function()
{
  netvlak = "LS"
  cat("Load file with PC6 as suffix\n")
  dataset = LoadWrap()
    
  cat("Load source file\n")
  mindataset = LoadWrap()
  
  setkey(dataset,ID_NAN)
  setkey(mindataset,ID_NAN)
  mindataset = unique(mindataset[dataset[Gemeente=="Arnhem",ID_NAN],list(ID_NAN,Ligging)])
  mindataset[!is.na(Ligging),mdsys := strsplit(Ligging[!is.na(Ligging)], "\\(|\\)")]
  mindataset[!is.na(Ligging),Coo := llply(mdsys,function(x) t(matrix(as.numeric(do.call(rbind, strsplit(x[7],","))),2,)))]
  mindataset[,Ligging:=NULL]
  mindataset[,mdsys:=NULL]
  
  dataset=dataset[Gemeente=="Arnhem"]
    
  cooset = dlply(mindataset[,list(Coo,ID_NAN)],"ID_NAN",function(x) (cbind(x$Coo[[1]],x$ID_NAN)))
  
  cooset2 = Convert_Coordinate_System (setnames(data.table(do.call(rbind,cooset)),c("Coo_X","Coo_Y","ID_NAN")),
                             from = "RDS",to = "lonlat",
                             xcol ="Coo_X",ycol="Coo_Y",
                             xcolout ="Lon",ycolout ="Lat")
  cooset2[,pathorder := 1:5000,by=ID_NAN]
  cooset2[,index := 1:nrow(cooset2)]
  
  setkey(cooset2,ID_NAN)
  SaveWrap(cooset2,
           file = paste0(settings$Analyse_Datasets,"/4. Tableau sets/",netvlak,"_Kabels_Geo.csv"))
  
  SaveWrap(unique(dataset)[cooset2],
  file = paste0(settings$Analyse_Datasets,"/4. Tableau sets/",netvlak,"_Kabels_Geo_Data.csv"))
  
}

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
            Data      <- as(ShapeFile, "data.table")
            Data$PolygonID <- as.numeric(rownames(Data))
          },
          
          # spatialpolygonsdataframe
          spd = {
            cat("Select a spatiallines R file \n")
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
  Lines <- slot(ShapeFile,"lines")
  # coordinates = llply(Polygons,createpoly, .progress = "text")
  coordinates = data.tableldply(Lines,createpoly, .progress = "text")
  
  out = AHA_RDCtoGPS(coordinates[,list(Longitude,Latitude)])
  coordinates[Longitude:= out$V1]
  coordinates[Latitude := out$V2]
  
  if (combine) {coordinates <- merge(Data,coordinates)}
  
  write.csv(coordinates, paste0(FileName,".csv"), row.names = FALSE)
}

AHA_RDCtoGPS = function(data,veld_x,veld_y,veld_lon="lon",veld_lat="lat"){
  x=data[,veld_x,with=F]
  y=data[,veld_y,with=F]
  
  data[,lon := 
         5.387206+((5260.52916 * ((x - 155000) * 10 ^ -5)) + (105.94684 * ((x - 155000) * 10 ^ -5) 
                                                              * (y - 463000) * 10 ^ -5) + (2.45656 * ((x - 155000) * 10 ^ -5) * (y - 463000) * 10 ^ -5 ^ 2) + 
                     (-0.81885 * ((x - 155000) * 10 ^ -5) ^ 3) + (0.05594 * ((x - 155000) * 10 ^ -5) * 
                                                                    (y - 463000) * 10 ^ -5 ^ 3) + (-0.05607 * ((x - 155000) * 10 ^ -5) ^ 3 * (y - 463000) * 
                                                                                                     10 ^ -5) + (0.01199 * (y - 463000) * 10 ^ -5) + 
                     (-0.00256 * ((x - 155000) * 10 ^ -5) ^ 3 * (y - 463000) * 10 ^ -5 ^ 2) + (0.00128 * 
                                                                                                 ((x - 155000) * 10 ^ -5) * (y - 463000) * 10 ^ -5 ^ 4) + (0.00022 * (y - 463000) * 10 ^ -5 ^ 2) + 
                     (-0.00022 * ((x - 155000) * 10 ^ -5) ^ 2) + (0.00026 * ((x - 155000) * 10 ^ -5) ^ 5))/3600]
  
  data[,lat := 
         52.15517+((3235.65389 * (y - 463000) * 10 ^ -5) + (-32.58297 * ((x - 155000) * 10 ^ -5) ^ 2) 
                   + (-0.2475 * (y - 463000) * 10 ^ -5 ^ 2) + 
                     (-0.84978 * ((x - 155000) * 10 ^ -5) ^ 2 * (y - 463000) * 10 ^ -5) + (-0.0655 * 
                                                                                             (y - 463000) * 10 ^ -5 ^ 3) + (-0.01709 * ((x - 155000) * 10 ^ -5) ^ 2 * (y - 463000) 
                                                                                                                            * 10 ^ -5 ^ 2) + (-0.00738 * ((x - 155000) * 10 ^ -5)) + 
                     (0.0053 * ((x - 155000) * 10 ^ -5) ^ 4) + (-0.00039 * ((x - 155000) * 10 ^ -5) ^ 2 
                                                                * (y - 463000) * 10 ^ -5 ^ 3) + (0.00033 * ((x - 155000) * 10 ^ -5) ^ 4 * 
                                                                                                   (y - 463000) * 10 ^ -5) + (-0.00012 * ((x - 155000) * 10 ^ -5) * (y - 463000) * 10 ^ -5))/3600]
  
  setnames(data,c("lon","lat"),c(veld_lon,veld_lat))
  return(data)
}

# Create the polygons ------------------------------
createpoly = function (Line)
{  
  ID <- slot(Lines, "ID")
  coords <- data.table(slot(slot(Line,"Lines")[[1]],"coords"))
  coords$PlotOrder <- c(1:nrow(coords))
  
  return(
    data.table(
      Longitude = coords[,1],
      Latitude  = coords[,2],
      LineID = rep(ID,nrow(coords)),
      PlotOrder = c(1:nrow(coords))
    ))
}