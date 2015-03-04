#Written by Michiel Musterd - 25-02-2015
#---------------------------------------
#Script containing the necessary functions to group the geoquery information in risicokaart
#to an X,Y grid that can be coupled to the NOR later.

#Mini function to generate a row of X,Y coordinates that together form a grid [Xmin,Xmax]_[Ymin,Ymax]
AHA_GridGen = function (stepX=500,stepY=500){
  
#set the boundaries of the grid
Xmin=0
Xmax=300000
Ymin=300000
Ymax=650000

#calculate the necessary step
Nx=round((Xmax-Xmin)/stepX+1)
Ny=round((Ymax-Ymin)/stepY+1)

#Generate the grid
Coo_X=rep(seq(Xmin,Xmax,length.out=Nx),Ny)
Coo_Y=rep(seq(Ymin,Ymax,length.out=Ny),each=Nx)
XYgrid=data.table(cbind(Coo_X,Coo_Y))
cat("Total number of rows in the grid:",nrow(XYgrid),"\n")
return(XYgrid)
}

#Function to couple the risicokaart geoquery sets to a single X,Y grid
AHA_risicoMerge = function(stepX=20000,stepY=20000){
  
  #generate the grid and store it as spatialpoints
  Set1=AHA_GridGen(stepX,stepY)
  outputSet=Set1
  SPoints=SpatialPoints(Set1,proj4string=CRS(as.character("+init=epsg:28992")))
 
  ptmTotal=proc.time()
  cat("Starting t10 onbeschermd coupling... \n")
  ptm=proc.time()
  #load the first set to be coupled, t10 onbeschermd
  filename=paste0(settings$Ruwe_Datasets,"/14. Risicokaart/t10_onbeschermd_overstroming.Rda")
  outputSet[,t10onbeschermd:=as.factor(geoQuery(filename,SPoints))]
  cat("Finished in ",proc.time()[3]-ptm[3],"seconds \n")
  
  cat("Starting t100 onbeschermd coupling... \n")
  ptm=proc.time()
  #repeat the previous procedure for t100 onbeschermd
  filename=paste0(settings$Ruwe_Datasets,"/14. Risicokaart/t100_onbeschermd_overstroming.Rda")
  outputSet[,t100onbeschermd:=as.factor(geoQuery(filename,SPoints))]
  cat("Finished in ",proc.time()[3]-ptm[3],"seconds \n")
  
  cat("Starting t1000 onbeschermd coupling... \n")
  ptm=proc.time()
  #repeat the previous procedure for t1000 onbeschermd
  filename=paste0(settings$Ruwe_Datasets,"/14. Risicokaart/t1000_onbeschermd_overstroming.Rda")
  outputSet[,t1000onbeschermd:=as.factor(geoQuery(filename,SPoints))]
  cat("Finished in ",proc.time()[3]-ptm[3],"seconds \n")
  
  cat("Starting t100 beschermd coupling... \n")
  ptm=proc.time()
  #repeat the previous procedure for t100 beschermd
  filename=paste0(settings$Ruwe_Datasets,"/14. Risicokaart/t100_beschermd_overstroming.Rda")
  outputSet[,t100beschermd:=as.factor(geoQuery(filename,SPoints))]
  cat("Finished in ",proc.time()[3]-ptm[3],"seconds \n")
  A
  gc()
  
#for use with only the t1000beschermd file (for memory saving)
#   save(outputSet,file=paste0(settings$Ruwe_Datasets,"/14. Risicokaart/risicokaartIntermediate.Rda"))
#   
#   return('done')
#  load(paste0(settings$Ruwe_Datasets,"/14. Risicokaart/risicokaartIntermediate.Rda"))
# SPoints=SpatialPoints(outputSet[,c("Coo_X","Coo_Y"),with=F],proj4string=CRS(as.character("+init=epsg:28992")))

  cat("Starting t1000 beschermd coupling... \n")
  ptm=proc.time()
  #repeat the previous procedure for t1000 beschermd
  filename=paste0(settings$Ruwe_Datasets,"/14. Risicokaart/t1000_beschermd_overstroming.Rda")
  outputSet[,t1000beschermd:=as.factor(geoQuery(filename,SPoints))]
  cat("Finished in ",proc.time()[3]-ptm[3],"seconds \n")
  
  cat("Coupling finished in ",proc.time()[3]-ptmTotal[3],"seconds \n")
  gc()

  #now we translate to our new datastructure with risk(1/0), protected (1/0), norm10(1/0),norm100(1/0),norm1000(1/0)
  risicoCheck=ifelse(outputSet[,as.numeric(t10onbeschermd)+as.numeric(t100onbeschermd)
                                  +as.numeric(t1000onbeschermd)+as.numeric(t100beschermd)
                                  +as.numeric(t1000beschermd)]-5>0,1,0)
  protectedCheck=ifelse(outputSet[,as.numeric(t100beschermd)+as.numeric(t1000beschermd)]-2>0,1,ifelse(risicoCheck==0,2,0))
  norm10Check=ifelse(outputSet[,as.numeric(t10onbeschermd)]-1>0,1,0)
  norm100Check=ifelse(outputSet[,as.numeric(t100beschermd)+as.numeric(t100onbeschermd)]-2>0,1,0)
  norm1000Check=ifelse(outputSet[,as.numeric(t1000beschermd)+as.numeric(t1000onbeschermd)]-2>0,1,0)
  
  
  outputSet[,Risico_Overstroming:=as.factor(risicoCheck)]
  outputSet[,Risico_Bescherming:=as.factor(protectedCheck)]
  outputSet[,t10_Overstroming:=as.factor(norm10Check)]
  outputSet[,t100_Overstroming:=as.factor(norm100Check)]
  outputSet[,t1000_Overstroming:=as.factor(norm1000Check)]
            
  outputSet$t10onbeschermd=NULL
  outputSet$t100onbeschermd=NULL
  outputSet$t1000onbeschermd=NULL
  outputSet$t100beschermd=NULL
  outputSet$t1000beschermd=NULL

  mindataset=outputSet
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/14. Risicokaart/risicokaartXY.Rda"))
  
  return(summary(outputSet))
  
}

#Function to perform the geoquery on a given input set and return the matching indices


#find the rows where the index is set and return a vector of yes/no (1/0)
geoQuery=function(filename,PointSet){
  
  loadCatch=load(filename)
  PolygonSet=SpatialPolygons(spatialsetdataframe@polygons,proj4string=CRS("+init=epsg:28992"))
  cat("Polygon set created, removing spatialsetdataframe for memory space \n")
  rm(spatialsetdataframe)
  gc()
  
  
  NT=length(PointSet)
  ptm=proc.time()
  #loop to prevent memory issues
  index=data.table(rowID=as.numeric((1:NT)*0))
  NperSet=20000
  for (i in 1:max(round(NT/NperSet),1)){
    cat(i,"/",round(NT/NperSet),"\n")
    if (i==round(NT/NperSet)){
      cntStart=1+(i-1)*NperSet
      cntEnd=NT
    }else
    {
      cntStart=1+(i-1)*NperSet
      cntEnd=i*NperSet
    }
    index[cntStart:cntEnd]=data.table(rowID=as.numeric(sp::over(PointSet[cntStart:cntEnd],PolygonSet)))
  }
  cat("Time taken loop: ",proc.time()[3]-ptm[3],"\n")
  cat("Index created, removing PolygonSet and PointSet \n")
 
  rm(PolygonSet)
  gc()
  
  #add a column in outputSet with 1s on index and 0s on the rest of the entries
  TFvect=data.table((1:NT)*0)
  TFvect[!is.na(index$rowID),]=1
  rm(index)
  gc()
  
  #return the checkvector
  
  return(TFvect$V1)
}
