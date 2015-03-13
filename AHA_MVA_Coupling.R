#######################################################################
# Written by Michiel Musterd - 09-02-2015 (last update:10-03-2015)
# -------------
# This code is meant to couple two sets based on their common keys. 
# Possible keys:
# - x,y (nearest neighbour coupling)
# - PC4
# - PC6
# - Essentially any other single key
# - Coupling on geoquery (do these x,y coordinates lie inside a certain region)(shape files: SP package)
#
# The base set is Set1, in which duplicates are allowed (e.g. cables in the same location)
# whereas the dependent set is Set2, in which duplicates are not allowed 
# (and in fact filtered out to catch errors)

# Functionality to be added: 
# ----
# Add coupling on more than 2 keys
# 

# Non-implemented nice to haves:
# -----------------
# - ...
#
######################################################################

AHA_MVA_Coupling = function(ProxyListFile=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/Proxylist.Rda")){
  #Wrapper function to call the coupling function multiple times with different settings
  #in order to provide 1 call coupling of the proxylist with all external datasources
  #
  #  Input: location of the proxylist file in ProxyListFile
  # Output: a large data.table file with all the relevant columns for the MVA analysis (saved and returned)
    
  #Apart from the proxylistfile, all other datafiles are assumed to be in the Ruwe_Datasets folder. Except
  #for using the proxylist as base table, the order of coupling is chosen arbitrarily, because it does not
  #matter for the eventual dataset
  
  #Specify the names of all sets to be coupled
  Nfiles=12
  InputFileList=list(as.character(1:Nfiles))
  InputFileList[1]=paste0(settings$Ruwe_Datasets,"/15. CBS/CBS_Gecombineerd_Gemeente_Wijk_Buurt.Rda")
  InputFileList[2]=paste0(settings$Ruwe_Datasets,"/16. Zakking/Zakking.Rda")
  InputFileList[3]=paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_grouped_2007_2014_RDS.Rda")
  InputFileList[4]=paste0(settings$Ruwe_Datasets,"/23. Grondsoort/Grondsoorten_shp.Rda")
  InputFileList[5]=paste0(settings$Ruwe_Datasets,"/14. Risicokaart/risicokaartXY.Rda")
  InputFileList[6]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/Iso_hoogtelijn_XY.Rda")
  InputFileList[7]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/spoorbaandeel_lijn_XY.Rda")
  InputFileList[8]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/inrichtingselement_lijn_XY_boom.Rda")
  InputFileList[9]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/inrichtingselement_lijn_XY_overig.Rda")
  InputFileList[10]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/inrichtingselement_punt_XY_boom.Rda")
  InputFileList[11]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/inrichtingselement_punt_XY_overig.Rda")
  InputFileList[12]=paste0(settings$Ruwe_Datasets,"/10. BAG/panden000_XY_clean.Rda")
  
  #Check existence of each of these files before starting the run  
  existCheck=1:Nfiles
  for (i in 1:length(InputFileList)){
    existCheck[i]=file.exists(InputFileList[[i]])*1
  }
    
    if (sum(existCheck)<length(InputFileList)){
      cat("Some inputfiles are missing. Missing files:\n")
      for (i in 1:length(InputFileList)){
        if (existCheck[i]==0){
         cat(InputFileList[i],"\n") 
        }
      }
      stop("Run aborted")
    }
  
     
  
  #Start the coupling sequence, each with their respective settings with respect to the 
  #method of coupling and things to be included on top of the coupling (e.g. distance to nearest neighbour)
  genericOutFileName=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/KoppelOutput")
  finalSetOutFileName=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/MVA_Coupled_AnalysisSet.Rda")
  
  #CBS - PC4 coupling
  SetNo=1
  cat("Starting ",SetNo," coupling \n")
  currentInFile=ProxyListFile
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(no_of_keys=1,couple_method=3,key1_nameA="PC_6_van",key2_nameA="PC_4",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  #Deltares - XY coupling
  SetNo=2
  cat("Starting ",SetNo," coupling \n")
  currentInFile=currentOutFile
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  #KNMI - XY coupling (preprocessed lat/lon into RDS)
  SetNo=3
  cat("Starting ",SetNo," coupling \n")
  currentInFile=currentOutFile
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",includeNNdist=1,NNdistName="Afstand_Weerstation",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  #Grondsoort - geoquery (polygon)
  SetNo=4
  cat("Starting ",SetNo," coupling \n")
  currentInFile=currentOutFile
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(no_of_keys=0,couple_method=2,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  #Risicokaart - XY
  SetNo=5
  cat("Starting ",SetNo," coupling \n")
  currentInFile=currentOutFile
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  #Iso lijnen - XY (preprocessed lines into x,y point sets)
  SetNo=6
  cat("Starting ",SetNo," coupling \n")
  currentInFile=currentOutFile
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",includeNNamount=1,amountRad=100,amountIDname="Isolijn_ID",
           amountName="Aantal_Isolijn",includeNNdist=1,NNdistName="Afstand_Isolijn",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  #Spoorbaan - XY (preprocessed lines into x,y point sets)
  SetNo=7
  cat("Starting ",SetNo," coupling \n")
  currentInFile=currentOutFile
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           includeNNdist=1,NNdistName="Afstand_Spoorbaan",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  #Inrichtingselementen lijn, boom - XY (preprocessed lines into x,y point sets)
  SetNo=8
  cat("Starting ",SetNo," coupling \n")
  currentInFile=currentOutFile
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           includeNNdist=1,NNdistName="Afstand_Boomrij",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  #Inrichtingselementen lijn, overig - XY (preprocessed lines into x,y point sets)
  SetNo=9
  cat("Starting ",SetNo," coupling \n")
  currentInFile=currentOutFile
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           includeNNdist=1,NNdistName="Afstand_Overige_Lijn_Inrichtingselement",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  #Inrichtingselementen punt, boom - XY
  SetNo=10
  cat("Starting ",SetNo," coupling \n")
  currentInFile=currentOutFile
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           includeNNdist=1,NNdistName="Afstand_Boom",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  #Inrichtingselementen punt, overig - XY
  SetNo=11
  cat("Starting ",SetNo," coupling \n")
  currentInFile=currentOutFile
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           includeNNdist=1,NNdistName="Afstand_Overige_Punt_Inrichtingselement",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  #BAG panden - XY
  SetNo=12
  currentInFile=currentOutFile
  cat("Starting BAG panden coupling \n")
  currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  coupling(key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",pandensetRepeat=0,includeNNdist=1,NNdistName="Afstand_Pand",
           includeNNamount=1,amountRad=5,amountIDname="Gebouw_mID",amountName="Aantal_Panden_Binnen_5m",
           outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  
  for (i in 1:53){
    cat("Coupling pandenfile ",i," of 53 \n")
    if (i<10){
      Set2NameInput=paste0(settings$Ruwe_Datasets,"/10. BAG/panden00",i,"_XY_clean.Rda")
    }else{
      Set2NameInput=paste0(settings$Ruwe_Datasets,"/10. BAG/panden0",i,"_XY_clean.Rda")
    }
    currentInFile=currentOutFile
    currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
    coupling(key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
                key2_nameA="Coo_X",key2_nameB="Coo_Y",pandensetRepeat=1,includeNNdist=1,NNdistName="Afstand_Pand",
                includeNNamount=1,amountRad=5,amountIDname="Gebouw_mID",amountName="Aantal_Panden_Binnen_5m",
                outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=Set2NameInput)
  }
  
 
  #save the final file with a more descriptive name
  load(currentOutFile)
  save(mindataset,file=finalSetOutFileName)

}


coupling = function(no_of_keys=2,couple_method=1,includeNNdist=0,NNdistName="-",
                    includeNNamount=0,amountRad=100,key1_nameA="Coo_X_van",amountIDname="-",amountName="-",
                    key1_nameB="Coo_Y_van",key2_nameA="Coo_X",key2_nameB="Coo_Y",pandensetRepeat=0,
                    outFileName=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/KoppelOutput.Rda"),
                    Set1Name,Set2Name){
  

  
  
#   #############################
#   #Start of user input section#
#   #############################
#   
#   #set the number of keys to be used per dataset (0 (geoquery), 1 or 2)
#   no_of_keys=0
#   
#   #set the method to couple, 
#   #0 is direct comparison, 1 is nearest neighbour, 2 is geoquery on polygon, 3 PC6 (in set 1) to PC4, 4 is geoquery on line
#   couple_method=4
#   
#   #whether to include the distance to the nearest neighbour (in case of couple_method=1), 1 for yes, 0 for no
#   includeNNdist=0 
#   NNdistName="Afstand_Weerstation"
#   
#   #whether to include the amount of elements within a certain radius (1=yes, 0=no)
#   includeNNamount=0
#   amountRad=100
#   
#   #Set the primary column names to be used as key in set 1 and set 2
#   key1_nameA="Coo_X_van"
#   key2_nameA="Coo_X"
#   
#   #Set the secondary column names to be used as key. These keys are only used if no_of_keys is set to 2
#   key1_nameB="Coo_Y_van"
#   key2_nameB="Coo_Y"
#   
#   #Set the location and names of the input datasets
#   #Set1Name=paste0(settings$Testcodes,"/Set1.Rda")
#   #Set1Name=paste0(settings$Ruwe_Datasets,"/MS_kabels_BAR_KLAK_Zakking.Rda")
#   Set1Name=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/MVA_sample_CBS_zakking_KNMI_Grondsoort.Rda")
#   #Set2Name=paste0(settings$Ruwe_Datasets,"/15. CBS/CBS_Gecombineerd_Gemeente_Wijk_Buurt.Rda")
#   #Set2Name=paste0(settings$Ruwe_Datasets,"/16. Zakking/Zakking.Rda")
#   #Set2Name=paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_grouped_2007_2014_RDS.Rda")
#   #Set2Name=paste0(settings$Ruwe_Datasets,"/23. Grondsoort/Grondsoorten_shp.Rda")
#   #Set2Name=paste0(settings$Ruwe_Datasets,"/14. Risicokaart/risicokaartXY.Rda")
#   Set2Name=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/Iso_hoogtelijn.Rda")
#   
#   #Set2Name=paste0(settings$Testcodes,"/Set2.Rda")
#   
#   
#   #Set the location and name of the output dataset
#   outFileName=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/MVA_sample_CBS_zakking_KNMI_Grondsoort_Risico_Hoogtelijn.Rda")
#   
#   #############################
#   #End of user input section  #
  #############################
   
  #Load in the sets to be coupled. I do this by loading the object pointer into a variable
  #(Set?NameCheck) and then performing a get on that to store the object in my preferred data table
  Set1NameCheck=load(Set1Name)
  Set1=get(Set1NameCheck)
  
  #IMPORTANT NOTE: if the loaded set contains more than 1 data.table you have to directly specify mindataset
  
  if (couple_method==2){ #geoquery coupling so the input set is expected to have a different structure
    Set2NameCheck=load(Set2Name) 
    Set2=data.table(spatialsetdataframe@data)
    Set2spatial=SpatialPolygons(spatialsetdataframe@polygons,proj4string=CRS("+init=epsg:28992"))
    
  }else if (couple_method==4) {
    Set2NameCheck=load(Set2Name) 
    Set2=data.table(spatialsetdataframe@data)
    Set2spatial=SpatialLines(spatialsetdataframe@lines,proj4string=CRS("+init=epsg:28992"))
  }
  
  else{
    cat(Set2Name,"\n")
    
  Set2NameCheck=load(Set2Name)
  Set2=get(Set2NameCheck)
  }
  cat('Sets loaded, proceeding...\n')

  #check whether both sets are data.tables to avoid errors down the line
  if (is.data.table(Set1)==F || is.data.table(Set2)==F ){
    stop("One of the input sets is not in data table format, please correct")
  }
  
  #Here we do some error catching for:
  #A: coordinates with type character that should be numeric
  #B: coordinates in RDC should be converted to lat/lon format
    
  #A Check whether the class of key 1 is numeric and make sure that key 2 has the same class. Note that this
  #fails if key 2 column content is not supposed to be the same class, but in that case the coupling will fail anyway

  if (no_of_keys==1){
    if (is.numeric(Set1[,get(key1_nameA)])==T)
    {
      Set2[,(key2_nameA):=as.numeric(get(key2_nameA))]
    }
  }
  else if (no_of_keys==2 && couple_method==1){ 
    #in this case the x,y data has to be numeric in both sets
    Set1[,(key1_nameA):=as.numeric(get(key1_nameA))]
    Set2[,(key2_nameA):=as.numeric(get(key2_nameA))]
    Set1[,(key1_nameB):=as.numeric(get(key1_nameB))]
    Set2[,(key2_nameB):=as.numeric(get(key2_nameB))]
  }else if (no_of_keys==0 && (couple_method==2 || couple_method==4) ){
    #in this case the geoquery approach is used
    Set1[,(key1_nameA):=as.numeric(get(key1_nameA))]
    Set1[,(key1_nameB):=as.numeric(get(key1_nameB))]
  } else{
    stop("Invalid combination of couple_method and no_of_keys selected")
  }
  
  #B: can use the existing function AHA_RDCtoGPS for this
  #First check whether the coordinates are in RDC by just checking whether they
  #exceed say 1000. Check this somewhere halfway, since NA are grouped at the beginning or end because of keying
  
  set1LonLat=0
  set2LonLat=0
  if (no_of_keys==2 && couple_method==1){
    if (Set1[round(nrow(Set1)/2)+1,get(key1_nameA)]>1000){
      set1LonLat=1
    }  

    if (Set2[round(nrow(Set2)/2)+1,get(key2_nameA)]>1000){
      set2LonLat=1
    }

    if (set1LonLat+set2LonLat==1)
      #they are not in the same format. Convert the set that is not in that format yet to lon/lat for
      #the nearest neighbour search and name it the same as the other set
      if (set1LonLat==1){
        Set1[,c(eval(key2_nameA),eval(key2_nameB)):=
               AHA_RDCtoGPS(subset(Set1,select=c(get(key1_nameA),get(key1_nameB))))]
        key1_nameA=key2_nameA
        key1_nameB=key2_nameB
      }
    
  }
  
  
  #In case of a single key, set the key such that unique will know how to filter
  if (no_of_keys==1){
    setkeyv(Set1,key1_nameA)
    setkeyv(Set2,key2_nameA)
  }
  
  
  #Merge the datasets
  #----
  #We merge in the following fashion: the "NOR" dataset (Set 1 for now) is the base table in which duplicates (in the key)
  #are allowed to exist (for example because we have low and medium powerlines in the same place). The "environmental" dataset 
  #(Set 2 for now) is the dependent table in which duplicates are not allowed, because that would mean that we have e.g.
  #at the same time 10-20 and 21-50 trees nearby, which should not be possible
  
  #first run unique on Set2 to make sure that there are no double entries
  
  cat("Identifying unique elements of Set2 \n")
  if (couple_method==2 || couple_method==4){
    uniSet2=Set2
  }else{
    uniSet2=unique(Set2)
  }

  
    
  if (couple_method==1){ #coupling on nearest neighbour
    
    # if pandenset=1 I need to loop over all panden files so I rewrote the
    # nearest neighbour coupling a bit
    
    #Select only x and y columns from both sets and work with that for the neighbour ID
    Set1Sub=Set1[,c(eval(key1_nameA),eval(key1_nameB)),with=F]
    Set2Sub=uniSet2[,c(eval(key2_nameA),eval(key2_nameB)),with=F]

    
    
    #identify the rows with NA in X or Y in both sets
    S1X_NA_IDs=which(is.na(Set1Sub[,get(key1_nameA)]))
    S1Y_NA_IDs=which(is.na(Set1Sub[,get(key1_nameB)]))
    S2X_NA_IDs=which(is.na(Set2Sub[,get(key2_nameA)]))
    S2Y_NA_IDs=which(is.na(Set2Sub[,get(key2_nameB)]))
    
    #change their values temporarily to large numbers
    Set1Sub[S1X_NA_IDs,eval(key1_nameA):=1e99]
    Set1Sub[S1Y_NA_IDs,eval(key1_nameB):=1e99]
    Set2Sub[S2X_NA_IDs,eval(key2_nameA):=-1e99]
    Set2Sub[S2Y_NA_IDs,eval(key2_nameB):=-1e99]
    
    cat("Starting nearest neighbour identification \n")
    
    #find the nearest neighbour indices using nn2 from the RANN package
    
    if (includeNNamount==1){
      #important note: this approach assumes that there are never more than 100 
      #neighbour POINTS within amountRad
      indexNearest=nn2(Set2Sub,Set1Sub,k=100)
      
      #identify the points within the desired radius
      withinRad=indexNearest$nn.dists<amountRad
    
      nnWithRad=(1:size(withinRad,1))*0
      for (i in 1:size(withinRad,1)){
        #extract the IDs within the radius
        IDswithinRad=indexNearest$nn.idx[i,withinRad[i,]]
        
        #count the number of unique elements within the radius
        nnWithRad[i]=length(unique(uniSet2[IDswithinRad,get(amountIDname)]))
      }
      
      #add this column in Set1
      if (pandensetRepeat==0){
        Set1[,NNamount:=as.numeric(nnWithRad)]
        setnames(Set1,"NNamount",eval(amountName))
      }else{
        Set1[,eval(amountName):=get(amountName)+nnWithRad]
      }
    
    }
    
      indexNearest=nn2(Set2Sub,Set1Sub,k=1)
    
      cat("Nearest neighours identified, proceed to coupling \n")
      
      #Correcting the identified neighbours which were actually NAs
      indexNearest$nn.idx[S1X_NA_IDs]=0
      indexNearest$nn.idx[S1Y_NA_IDs]=0
      
      #Create a new column in Set1 with the merge ID, 
      #also insert a column in uniSet2 with these IDs (essentially row numbers for Set2)
      Set1[,mID:=indexNearest$nn.idx]
      
      if (includeNNdist==1){
        if (pandensetRepeat==0){
          Set1[,NNdist:=indexNearest$nn.dists]
          setnames(Set1,"NNdist",eval(NNdistName))
        }else{
          #return(data.table(X=Set1[,get(NNdistName)],Y=indexNearest$nn.dists,Z=pmin(Set1[,get(NNdistName)],indexNearest$nn.dists)))
          Set1[,eval(NNdistName):=pmin(get(NNdistName),indexNearest$nn.dists)]
        }
      }
        
      uniSet2[,mID:=1:nrow(uniSet2)]
    
    
    #Now the indices have been added to the sets and they are unique in uniSet2 so we can
    #do the coupling if we set the key to only mID
    
    
  }
  
  #coupling based on polygon (i.e. geoquery type)
  if (couple_method==2){
    cat("Starting geocoupling \n")
    #first read the x,y data from Set1 as SpatialPoints
    Set1Sub=Set1[,c(eval(key1_nameA),eval(key1_nameB)),with=F]
    SPoints=SpatialPoints(Set1Sub,proj4string=CRS(as.character("+init=epsg:28992")))
    
    #then perform the geoquery on the polygon shape set, assuming that the ordering
    #in the dataset and the spatialset are the same (which is true when extracted
    #from the same spatialdataframe without keying afterwards) and store it
    index=data.table(rowID=sp::over(SPoints,Set2spatial))

    
    #Create a new column in Set1 with the merge ID, 
    #also insert a column in uniSet2 with these IDs (essentially row numbers for Set2)
    Set1[,mID:=index$rowID]
    uniSet2[,mID:=1:nrow(uniSet2)]
    
  }
  
  #coupling PC6 to PC4
  if (couple_method==3){
    #read the PC_6 column and extract PC4
    PC6=Set1[,get(key1_nameA)]         
    Set1[,PC_4:=substring(PC6,1,4)]
    
    #reset the key to PC_4
    key1_nameA="PC_4"    
  }
  
  if (couple_method==4){
    #first read the x,y data from Set1 as SpatialPoints
    Set1Sub=Set1[,c(eval(key1_nameA),eval(key1_nameB)),with=F]
    SPoints=SpatialPoints(Set1Sub,proj4string=CRS(as.character("+init=epsg:28992")))

    ## untested code
    shortestDists = numeric(length(SPoints))
    
    #lapply(SPoints[1:10,],gDistance(SPoints[i,], Set2spatial))

    
    ptm=proc.time()
    #this apply returns row by row for SPoints what the distance to the nearest line is
    #if necessary we can also include which.min into this
    
    
    temp=gDistance( SPoints[1:20] , Set2spatial , byid = TRUE )
    cat(proc.time()[3]-ptm[3],"\n")
    return("gDistance( SPoints[1:5] , Set2spatial , byid = TRUE )")
    
  }
  

  #Set the keys for the different datasets, note that having different column names only works 
  #if I use the X[Y] merge option, not the merge(X,Y). Additionally, note that I use setkeyv instead of setkey, because
  #this allows me to use variables to set the names for the columns and keep the programming clean
  if (no_of_keys==1){
    setkeyv(Set1,key1_nameA)
    setkeyv(uniSet2,key2_nameA)
  }
  else if (couple_method==1 || couple_method==2) { #in this situation the merge is x,y so we switch to mID as key to merge
    setkey(Set1,mID)
    setkey(uniSet2,mID)
  }
  
  cat("Start coupling \n")
  #finally we merge uniSet2 into Set1
  coupledSet=uniSet2[Set1] #this notation couples according to the key in Set1, so an entry that doesn't exist
  #in Set 2 gets NA, but an entry that doesn't exist in Set1 (and does in Set2) does not appear at all
  #coupledSet=uniSet2[Set1,nomatch=0] #This is perhaps a cleaner option, then above, because it eliminates rows for which we don't
  #have the "environmental" information
  
  cat("Coupling done, starting cleanup \n")
  
  #throw away the merge identifier to clean up the data and the x/y data in the second set, because
  #it isn't descriptive
  if (couple_method==1 || couple_method==2){
    coupledSet[,mID:=NULL]
    if (amountIDname=="Gebouw_mID"){coupledSet[,Gebouw_mID:=NULL]}
    
    if (couple_method==1){
      coupledSet[,eval(key2_nameA):=NULL]
      coupledSet[,eval(key2_nameB):=NULL]
      
      #if column names were the same in both dataset, adjust them to their old name here
      if (key1_nameA==key2_nameA){
        setnames(coupledSet,paste0("i.",key1_nameA),key1_nameA)
      }
      if (key1_nameB==key2_nameB){
        setnames(coupledSet,paste0("i.",key1_nameB),key1_nameB)
      }
      
      if (set1LonLat+set2LonLat==1){ #if a RDC to GPS transformation was performed, remove the GPS coordinates again
                          #leaving only the RDC coords
        coupledSet[,eval(key2_nameA):=NULL]
        coupledSet[,eval(key2_nameB):=NULL]
      }
      

    }
  }
  mindataset=coupledSet
  
  cat("Saving coupled set \n")
  save(mindataset,file=outFileName)
  
  cat("Freeing memory \n")
  gc()
  
  cat("Done! \n")
  
  return(mindataset[sample(1:nrow(mindataset),10),])
}

