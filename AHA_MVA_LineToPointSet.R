
AHA_MVA_LineToPointSet = function (){
  #Written by Michiel Musterd 02-03-2015
  #This function converts spatialLines(Dataframe) to spatialPoints and then further to simply XY coords
  #The code is meant to preprocess linesets such that a nearest neighbour search based on points can be
  #performed instead of the (very slow) coupling of point to line that is needed otherwise
  
  #Read in the spatialLines set
  #filename_no_ext=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/spoorbaandeel_lijn")
  filename_no_ext=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/Iso_hoogtelijn")
  #filename_no_ext=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/inrichtingselement_lijn")
  setname="isolijn"
  
  load(paste0(filename_no_ext,".Rda"))
  
  #for now just use the loaded mindataset for spoorbaandelen
  #pointset=spsample(spatialsetdataframe@lines,n=100,type="regular")
  pointDataframe=sample.line(spatialsetdataframe,sdist=20)

  
  #store the data part of the frame and add a column with rownumbers
  dataset=data.table(spatialsetdataframe@data)
  dataset[,mID:=0:(nrow(dataset)-1)]
  
  #ok, so now we have a set of points with a data list specifying the ID of the
  #row to which the point applies
  
  #now postprocess this set further into a normal table with x,y coordinates for each point by coupling
  #first we extract the point coords and IDs into a data.table
  XYID=data.table(pointDataframe@coords,as.numeric(pointDataframe@data$ID))
  setnames(XYID,names(XYID),c("Coo_X","Coo_Y","ID"))

  
  #then we set the keys
  setkey(dataset,mID)
  setkey(XYID,ID)
  
  #then we merge the sets
  mindataset=dataset[XYID]
  
  #clean up operations  
  mindataset=cleanup(mindataset,setname=setname)
  
  if (setname=="inrichting"){
    #split the output and save it as two sets (boom and overig)
    boomset=mindataset[Type_Inrichting=="bomenrij",list(Coo_X,Coo_Y)]
    overigset=mindataset[Type_Inrichting=="overig",list(Coo_X,Coo_Y)]
    
    #save both sets 
    save(boomset,file=paste0(filename_no_ext,"_XY_boom.Rda"))
    save(overigset,file=paste0(filename_no_ext,"_XY_overig.Rda"))
    
  }else{
    #save the output set to 1 file
    save(mindataset,file=paste0(filename_no_ext,"_XY.Rda"))
  }
  

  
  
}

cleanup = function(DT,setname){
  #adjust the cleanup operation depending on the type of linedata, selecting only the useful columns
  
  switch(setname,
         "spoor"={DTout=DT[,list(Coo_X,Coo_Y,TYPESPOORB,VERVOERFUN)]
                  setnames(DTout,c("TYPESPOORB","VERVOERFUN"),c("Type_Spoorbaan","Vervoersfuctie_Spoorbaan"))},
         
         "inrichting"={DTout=DT[,list(Coo_X,Coo_Y,TYPEINRICH)]
                       DTout=DTout[TYPEINRICH!="bomenrij",TYPEINRICH:="overig"]
                       setnames(DTout,c("TYPEINRICH"),c("Type_Inrichting"))},
         
         "isolijn"={DTout=DT[,list(Coo_X,Coo_Y,HOOGTE,TYPERELIEF,OBJECTID)]
                    setnames(DTout,c("HOOGTE","TYPERELIEF","OBJECTID"),
                             c("Hoogte_Isolijn","Type_Isolijn","Isolijn_ID"))},      
         
    )
  
  return(DTout)
}


sample.line = function(x, sdist=100)
{
  if (!require(sp)) stop("sp PACKAGE MISSING")
  if (!inherits(x, "SpatialLinesDataFrame")) stop("MUST BE SP SpatialLinesDataFrame OBJECT")
  
  #calculate the length of the line pieces
  lgth <- SpatialLinesLengths(x) 
  
  #calculate the number of points in the line with current desired spacing (sdist) and line length (lgth)
  #ns <- round( (lgth[1] / sdist), digits=0)
  #if (ns==0) {ns=1} #catch rounding to 0 by requiring ns to be at least 1
  
  #sample the line
  #lsamp <- spsample(x[1,], n=ns, type="regular", offset=c(0.5,0.5))
  
  #store the points in a dataframe together with their ID to be used in coupling
  #tempresults <- SpatialPointsDataFrame(lsamp, data=data.frame(ID=rep(rownames(x@data[1,]),ns))) 
  
  #repeat above procedure for each row
  Nstep=500 #number of steps before adding the current data to the results list, improves speed significantly
   for (i in 1:dim(x)[1] ) 
   {    
     ns <- round( (lgth[i] / sdist), digits=0)
     
     if (ns==0) {ns=1}
     cat(i," of ",dim(x)[1], ", Number of points in line: ",ns,", ")
     lsamp <- spsample(x[i,], n=ns, type="regular", offset=c(0.5,0.5))
     cat(length(lsamp)[1],"\n")
     lsamp <- SpatialPointsDataFrame(lsamp, data=data.frame(ID=rep(rownames(x@data[i,]),ns)))     
     if (i==dim(x)[1]){
       if (i %% Nstep==1){
         tempresults=lsamp
       }else{
         tempresults=rbind(tempresults,lsamp)}
       results=rbind(results,tempresults)
     }else if (i %% Nstep==0){
       if (i==Nstep){
         tempresults=rbind(tempresults,lsamp)
         results=tempresults
       }else
       {
         tempresults=rbind(tempresults,lsamp)
         results=rbind(results,tempresults)
       }
     }else if(i %% Nstep==1){
       tempresults = lsamp
     }else{
       tempresults=rbind(tempresults,lsamp)
     }
     
   }
  
  return(results)
}