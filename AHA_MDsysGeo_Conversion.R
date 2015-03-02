#Written by Michiel Musterd - 23-02-2015
#---------------------------------------
#Function to convert geo data of the mdsys type to SpatialPolygon type for use in geoquery coupling
#This function takes mdsys input of the type: 
#MDSYS.SDO_GEOMETRY(2003,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1),MDSYS.SDO_ORDINATE_ARRAY(231088.1358,570469.305399999
#where the geometric data is available in the ordindate_array in x,y,x,y etc format

#This function was particularly written for the grondsoorten set, but can be fairly easily adapted to a different set

AHA_MDsysGeo_Conversion = function (){
  
  #Set the savefile location
  savefile=paste0(settings$Ruwe_Datasets,"/23. Grondsoort/Grondsoorten_shp.Rda")

  #first clean up the dataset a bit, we only need omschrijving (renamed to grondsoort) and SHAPE
  mindataset=mindataset[,c("OMSCHRIJVI","SHAPE"),with=F]
  setnames(mindataset,"OMSCHRIJVI","Grondsoort")
           
  #class of Grondsoort should be factor
  mindataset[,Grondsoort:=as.factor(Grondsoort)]
  

  #read in the column of the dataset with the geo data
  mdsys=mindataset$SHAPE
  
  
  #check the length of each row in the set
  charLength=nchar(mdsys)
  
  #check where the word ORDINATE_ARRAY is located in each row
  location=data.table(str_locate(mdsys, "ORDINATE_ARRAY"))
  
  #we know that the geodata itself starts 2 characters AFTER
  #the end of ORDINATE_ARRAY (because there is a bracket in between)
  #return(location$end)
  selecter=data.table(start=location$end+2,end=charLength-2)
  
  geoData=substr(mdsys,selecter$start,selecter$end)
  
  #Extract the X and Y coordinates row by row and store them as polygons
  spatialList=list(1:length(geoData))
  removeIDs=c((1:length(geoData))*0)
  rownameShift=0
  
  for (i in 1:length(geoData)){
    temp=as.numeric(unlist(strsplit(geoData[i],",")))
  
    if (is.na(selecter$start[i])){
      #this row should be removed from the entire set because there is no use in having it
      removeIDs[i]=1
      rownameShift=rownameShift+1
    }else
    {
      X=temp[seq(1,length(temp),by=2)]
      Y=temp[seq(2,length(temp),by=2)]
      setname=paste0(i-rownameShift)
      spatialList[i]=Polygons(list(Polygon(cbind(X,Y))),eval(setname))
    }
  }
  #remove the empty entries from all sets
  spatialList[removeIDs==1]=NULL
  mindataset=mindataset[!removeIDs,]
  

    
  #Merge the list of polygons in SpatialPolygons
  spatialset=SpatialPolygons(spatialList)  
    
  #throw away the shape column in mindataset and save all sets
  mindataset$SHAPE=NULL
  
  #Group them with the original dataset
  spatialsetdataframe=SpatialPolygonsDataFrame(spatialset,mindataset)
    
  
  save(spatialset,spatialsetdataframe,mindataset,dataclasses,file=savefile)
  
  return("Done")
    
}