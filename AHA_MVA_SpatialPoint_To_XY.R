
AHA_MVA_SpatialPoint_To_XY = function (){
  #Written by Michiel Musterd 03-03-2015
  #This function converts spatialPoints(Dataframe) to simply XY coords, in the current setup for
  #the Inrichtingselement set

  
  #Read in the spatialLines set
  filename_no_ext=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/Inrichtingselement_punt")
  load(paste0(filename_no_ext,".Rda"))
  
  #for now just use the loaded mindataset for spoorbaandelen
  #pointset=spsample(spatialsetdataframe@lines,n=100,type="regular")
  pointDataframe=spatialsetdataframe

  #store the data part of the frame and add a column with rownumbers
  dataset=data.table(spatialsetdataframe@data)
  dataset[,mID:=0:(nrow(dataset)-1)]
  
  #ok, so now we have a set of points with a data list specifying the ID of the
  #row to which the point applies
  
  #now postprocess this set further into a normal table with x,y coordinates for each point by coupling
  #first we extract the point coords and IDs into a data.table
  XYID=data.table(pointDataframe@coords,as.numeric(0:(nrow(dataset)-1)))
  setnames(XYID,names(XYID),c("Coo_X","Coo_Y","ID"))

  
  #then we set the keys
  setkey(dataset,mID)
  setkey(XYID,ID)
  
  #then we merge the sets
  mindataset=dataset[XYID]
  
  #clean up operations  
  mindataset=cleanup(mindataset,setname="inrichting")
  
  
  #split the set into a set for bomen and a set for overige inrichtingselementen
  boomset=mindataset[Type_Inrichting_Punt=="boom",list(Coo_X,Coo_Y)]
  overigset=mindataset[Type_Inrichting_Punt=="overig",list(Coo_X,Coo_Y)]
  
  #save both sets 
  save(boomset,file=paste0(filename_no_ext,"_XY_boom.Rda"))
  save(overigset,file=paste0(filename_no_ext,"_XY_overig.Rda"))
  
  
}

cleanup = function(DT,setname){
  #adjust the cleanup operation depending on the type of puntdata, selecting only the useful columns
  
  switch(setname,
         "spoor"={},
         
         "inrichting"={DTout=DT[,list(Coo_X,Coo_Y,TYPEINRICH)]
                       DTout=DTout[TYPEINRICH!="boom",TYPEINRICH:="overig"]
                       setnames(DTout,c("TYPEINRICH"),c("Type_Inrichting_Punt"))},
         
         "isolijn"={},      
         
    )
  
  return(DTout)
}

