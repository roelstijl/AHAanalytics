

AHA_MVA_BAGimport = function (){
  #Written by Michiel Musterd 06-03-2015
  #This script is intended to read in the 50+ shapefiles of the BAG panden set
  #and store them as XY sets
  
  
  folder=paste0(settings$Bron_Datasets,"/10. BAG/ExtractOutput")
  
  for (i in 11:54){
    gc()
    cat("Working on set number: ",i,"\n")
  if (i==1){
    sourcefile=paste0(folder,"/panden.shp")
  }else{
    sourcefile=paste0(folder,"/panden0",i-1,".shp")
    #NOTE: generalize the paste0 command to work both with i<11 and i=>11
  }  

  
  # read the set as spatiallinesdataframe
  ptm=proc.time()
  spatialsetdataframe = as(readShapeSpatial(sourcefile),"SpatialLinesDataFrame")
  Deltaptm=proc.time()-ptm
  cat("Read linedataset ",i," in :",Deltaptm[3]," s\n")
  
  #write the set to a Rda file for backup in brondata
  save(spatialsetdataframe,file=paste0(folder,"/panden.Rda"))
  cat("Set saved \n")
  
  #pointsample the line set with 5 m distance
  ptm=proc.time()
  pointDataframe=sample.line(spatialsetdataframe,sdist=5)
  Deltaptm=proc.time()-ptm
  cat("Sampled line to points in: ",Deltaptm[3]," s \n")

  #store the data part of the frame and add a column with rownumbers
  dataset=data.table(spatialsetdataframe@data)
  dataset[,mID:=0:(nrow(dataset)-1)]

  #now postprocess this set further into a normal table with x,y coordinates for each point by coupling
  #first we extract the point coords and IDs into a data.table
  XYID=data.table(pointDataframe@coords,as.numeric(pointDataframe@data$ID))
  setnames(XYID,names(XYID),c("Coo_X","Coo_Y","ID"))
  
  #then we set the keys
  setkey(dataset,mID)
  setkey(XYID,ID)
  
  #then we merge the sets
  mindataset=dataset[XYID]
  
  #rename the mID column
  setnames(mindataset,"mID","Gebouw_mID")
  
  #and save it to file in ruwe datasets
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/10. BAG/panden0",i-1,"_XY.Rda"))
    
  cat("Set ",i," saved to ruwe datasets \n")
  
  }
  
  return("Done!")
  
}


AHA_MVA_BAGcleanup = function(){
  #This function opens all BAG panden files and takes out all the non-needed columns and rows
  
  SetNameCheck=load(paste0(settings$Ruwe_Datasets,"/10. BAG/panden000_XY.Rda"))
  SetBase=get(SetNameCheck)
  
  SetBase=SetBase[STATUS=="Pand in gebruik",list(Gebouw_mID,Coo_X,Coo_Y)]
  
  save(SetBase,file=paste0(settings$Ruwe_Datasets,"/10. BAG/panden000_XY_clean.Rda"))
  
  for (i in 1:53){
    ptm=proc.time()[3]
    if (i<10){
      sourcefile=paste0(settings$Ruwe_Datasets,"/10. BAG/panden00",i,"_XY.Rda")
      savefile=paste0(settings$Ruwe_Datasets,"/10. BAG/panden00",i,"_XY_clean.Rda")
    }else{
      sourcefile=paste0(settings$Ruwe_Datasets,"/10. BAG/panden0",i,"_XY.Rda")
      savefile=paste0(settings$Ruwe_Datasets,"/10. BAG/panden0",i,"_XY_clean.Rda")
    }
  
    SetNameCheck=load(sourcefile)
    SetBase=get(SetNameCheck)
  
    SetBase=SetBase[STATUS=="Pand in gebruik",list(Gebouw_mID,Coo_X,Coo_Y)]
    save(SetBase,file=savefile)
    
    cat(i,"is done in ",proc.time()[3]-ptm," s \n")
  }


  

}