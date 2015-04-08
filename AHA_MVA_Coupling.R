#-----------------------------------------------------------------#
#-------- Written by Michiel Musterd (BearingPoint) 2015 ---------#
#-----------------------------------------------------------------#
# PURPOSE: 
# This file contains several functions that are used in coupling
# the NOR dataset to a variety of internal and external datasources.
# The core of this file is coupling() which performs the coupling 
# itself, whereas other functions perform brief calculations or
# more specialized types of coupling
#
# 
# INCLUDED FUNCTIONS (see each function for a description):
# ---
# AHA_MVA_Coupling(NORfile,Proxyfile,Settype,ProxyThreshold) - batch to run all coupling
# coupling(no_of_keys,couple_method,includeNNdist,NNdistName
#         includeNNamount,amountRad,key1_nameA,amountIDname,amountName,
#         key1_nameB,key2_nameA,key2_nameB,pandensetRepeat,
#         outFileName,Set1Name,Set2Name,cleantextkey,memorySet)
# splitDate(inputSet)
# CoupleNORproxy(NORset,ProxySet,Settype)
# CalcLoadIndicators(inputSet)
# TargetVariables(coupledNOR,threshold)
# ExtractMofData(mindataset)
# ExtractCableData(mindataset)
#-----------------------------------------------------------------#




AHA_MVA_Coupling = function(NORfile=paste0(settings$Ruwe_Datasets,"/00. NOR input/MSmoffen_NOR.Rda"),
                            Proxyfile=paste0(settings$Analyse_Datasets,"/1. Proxylijsten/Proxy_koppellijst_2015-03-25 14.00.00.Rda"),
                            Settype="MSmoffen", ProxyThreshold=0.3){
  #Wrapper function to call the coupling function multiple times with different settings
  #in order to provide 1 call coupling of the proxylist with all external datasources
  #
  #  Input: location of the proxylist file in NORfile (can be "InMemory")
  #  Output: a large data.table file with all the relevant columns for the MVA analysis (saved (unless "InMemory") and returned)
    
  #Apart from the NORfile, all other datafiles are assumed to be in the Ruwe_Datasets folder. Except
  #for using the proxylist as base table, the order of coupling is chosen arbitrarily, because it does not
  #matter for the eventual dataset
  
  #######################################################################
  # Written by Michiel Musterd (BearingPoint) - 09-02-2015 (last update:07-04-2015)
  # -------------
  # This code is meant to couple multiple sets based on their common keys. 
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
  
  # Non-implemented nice to haves:
  # -----------------
  # - Coupling on more than 2 keys
  #
  ######################################################################
  
  
  
  #Specify the names of all sets to be coupled
  Nfiles=17
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
  InputFileList[12]=paste0(settings$Ruwe_Datasets,"/2. Kabelgegevens/KoppelFabrikanttypeBelasting.Rda")
  InputFileList[13]=paste0(settings$Ruwe_Datasets,"/2. Kabelgegevens/MS_HLD_belasting.Rda")
  InputFileList[14]=paste0(settings$Ruwe_Datasets,"/10. BAG/panden000_XY_clean.Rda")
  InputFileList[15]=paste0(settings$Ruwe_Datasets,"/11. Nettopologie/LS_HLDbelastingindicators.Rda")
  InputFileList[16]=paste0(settings$Ruwe_Datasets,"/19. CDB/MS_HLDbelastingindicators.Rda")
  InputFileList[17]=paste0(settings$Ruwe_Datasets,"/30. Tariefstoring/Tarief_data.Rda")
  
  
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
  cat("External data files checked: all exist \n")
  
  ptm=proc.time()   
  
  #Start the coupling sequence, each with their respective settings with respect to the 
  #method of coupling and things to be included on top of the coupling (e.g. distance to nearest neighbour)
  genericOutFileName=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/KoppelOutput")
  finalSetOutIntermediateFileName=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/MVA_Coupled_AnalysisSet_",Settype,"Intermediate.Rda")
  
  finalSetOutFileName=paste0(settings$Analyse_Datasets,"/2. CoupledWithoutProxy//MVA_Coupled_AnalysisSet_",Settype,".Rda")
  finalSetTargetOutFileName=paste0(settings$Analyse_Datasets,"/3. CoupledWithProxy/MVA_Coupled_AnalysisSet_",Settype,"_withTarget_TH",ProxyThreshold,".Rda")
  
  #First we process the fabrikanttype column into more useful data about the cable
  #This is the first, and usually only load of the inputlist, after this all operations can be done in memory
  if (Settype=="LSkabels" | Settype=="MSkabels"){
    SetName=load(NORfile)
    Set=get(SetName)
    coupledSet=AHA_MVA_ExtractCableData(Set)

  }else if (Settype=="LSmoffen" | Settype=="MSmoffen"){
    SetName=load(NORfile)
    Set=get(SetName)
    coupledSet=AHA_MVA_ExtractMofData(Set)

  }else{
    stop("Unknown settype, abort coupling")
  }
  
  
  #Add dag, maand, jaar fields for each date field
  cat("Started adding dag,maand, jaar, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=AHA_MVA_splitDate(coupledSet)
  
  
  #CBS - PC4 coupling
  SetNo=1
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=1,couple_method=3,key1_nameA="PC_6_van",key2_nameA="PC_4",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  #Deltares - XY coupling
  SetNo=2
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  #KNMI - XY coupling (preprocessed lat/lon into RDS)
  SetNo=3
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",includeNNdist=1,NNdistName="Afstand_Weerstation",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  #Grondsoort - geoquery (polygon)
  SetNo=4
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=0,couple_method=2,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  #Risicokaart - XY
  SetNo=5
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  #Iso lijnen - XY (preprocessed lines into x,y point sets)
  SetNo=6
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",includeNNamount=1,amountRad=100,amountIDname="Isolijn_ID",
           amountName="Aantal_Isolijn",includeNNdist=1,NNdistName="Afstand_Isolijn",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  #Spoorbaan - XY (preprocessed lines into x,y point sets)
  SetNo=7
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           includeNNdist=1,NNdistName="Afstand_Spoorbaan",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  #Inrichtingselementen lijn, boom - XY (preprocessed lines into x,y point sets)
  SetNo=8
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           includeNNdist=1,NNdistName="Afstand_Boomrij",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  #Inrichtingselementen lijn, overig - XY (preprocessed lines into x,y point sets)
  SetNo=9
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           includeNNdist=1,NNdistName="Afstand_Overige_Lijn_Inrichtingselement",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  #Inrichtingselementen punt, boom - XY
  SetNo=10
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           includeNNdist=1,NNdistName="Afstand_Boom",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  #Inrichtingselementen punt, overig - XY
  SetNo=11
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",
           includeNNdist=1,NNdistName="Afstand_Overige_Punt_Inrichtingselement",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  #Determine the max belasting (=capaciteit) of each cable 
  if (Settype=="LSkabels" | Settype=="LSmoffen"){
    SetNo=12
    cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
    coupledSet=coupling(no_of_keys=1,couple_method=0,key1_nameA="Fabrikanttype",
                        key2_nameA="Fabrikanttype",cleantextkey=1,
                        outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  }else if (Settype=="MSkabels" | Settype=="MSmoffen"){
    SetNo=13
    cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
    coupledSet=coupling(no_of_keys=1,couple_method=0,key1_nameA="ID_Hoofdleiding_present",
                        key2_nameA="ID_Hoofdleiding_present",
                        outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  }
  
  cat("Performing intermediate save \n")
  save(coupledSet,file=finalSetOutIntermediateFileName,compress=F)

  #BAG panden - XY
  SetNo=14
  cat("Starting BAG panden coupling \n")
  coupledSet=coupling(key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
           key2_nameA="Coo_X",key2_nameB="Coo_Y",pandensetRepeat=0,includeNNdist=1,NNdistName="Afstand_Pand",
           includeNNamount=0,amountRad=5,amountIDname="Gebouw_mID",amountName="Aantal_Panden_Binnen_5m",
           outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)

  for (i in 1:53){
    cat("Coupling pandenfile ",i," of 53, runtime (s):",proc.time()[3]-ptm[3] ," \n")
    if (i<10){
      Set2NameInput=paste0(settings$Ruwe_Datasets,"/10. BAG/panden00",i,"_XY_clean.Rda")
    }else{
      Set2NameInput=paste0(settings$Ruwe_Datasets,"/10. BAG/panden0",i,"_XY_clean.Rda")
    }
    coupledSet=coupling(key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
                key2_nameA="Coo_X",key2_nameB="Coo_Y",pandensetRepeat=1,includeNNdist=1,NNdistName="Afstand_Pand",
                includeNNamount=0,amountRad=5,amountIDname="Gebouw_mID",amountName="Aantal_Panden_Binnen_5m",
                outFileName="InMemory", Set1Name="InMemory",Set2Name=Set2NameInput,memorySet=coupledSet)
  }

  save(coupledSet,file=finalSetOutIntermediateFileName,compress=F)
  
  #Belastingindicator coupling
  if (Settype=="LSkabels"){
    SetNo=15
    cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
    coupledSet=coupling(no_of_keys=1,couple_method=0,key1_nameA="ID_Hoofdleiding_present",key2_nameA="ID_Hoofdleiding_present",
                        outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  }else if (Settype=="MSkabels"){
    SetNo=16
    cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
    coupledSet=coupling(no_of_keys=1,couple_method=0,key1_nameA="ID_Verbinding_present",key2_nameA="ID_Verbinding_present",
                        outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  }

  if (Settype=="LSkabels" | Settype=="LSmoffen"){
    #couple tariefstoringen
    SetNo=17
    cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
    coupledSet=coupling(no_of_keys=1,couple_method=0,key1_nameA="ID_Hoofdleiding_present",
                        key2_nameA="ID_Hoofdleiding_present",
                        outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
    
    #set all NAs in tariefstoringen to 0 because that is what they are
    coupledSet[is.na(Aantal_tariefstoringen)==T,Aantal_tariefstoringen:=0]
  }

  
  cat("Performing intermediate save \n")
  save(coupledSet,file=finalSetOutIntermediateFileName,compress=F)
  
  if (Settype=="LSkabels" | Settype=="MSkabels"){
    #Calculate belasting metrics
    cat("Calculating load metrics \n")
    coupledSet=AHA_MVA_CalcLoadIndicators(coupledSet)

  }else if (Settype=="LSmoffen" | Settype=="MSmoffen") {
    coupledSet$Max_Belasting=NULL
  }
    
  
  #save the set without target variable
  save(coupledSet,file=finalSetOutFileName,compress=F)
 
  
  #finally couple the proxylist into the set to get the target variable
  ProxySet=get(load(Proxyfile))
  coupledSet=AHA_MVA_CoupleNORproxy(coupledSet,ProxySet,Settype=Settype,threshold=ProxyThreshold)
 
  #save the final file with a more descriptive name
  save(coupledSet,file=finalSetTargetOutFileName,compress=F)

 return("Done!")
}


coupling = function(no_of_keys=2,couple_method=1,includeNNdist=0,NNdistName="-",
                    includeNNamount=0,amountRad=100,key1_nameA="Coo_X_van",amountIDname="-",amountName="-",
                    key1_nameB="Coo_Y_van",key2_nameA="Coo_X",key2_nameB="Coo_Y",pandensetRepeat=0,
                    outFileName=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/KoppelOutput.Rda"),
                    Set1Name,Set2Name,cleantextkey=0,memorySet){
  #   ############################################
  #   #Explanation of user input to the function #
  #   ############################################
  #   
  #   no_of_keys      : set the number of keys to be used per dataset (0 (geoquery), 1 or 2)
  #   couple_method   : 0 is direct comparison, 1 is nearest neighbour, 2 is geoquery on polygon, 3 PC6 (in set 1) to PC4
  #   includeNNdist   : whether to include the distance to the nearest neighbour (in case of couple_method=1), (1/0)
  #   NNdistName      : name of the nearest neighbour column to be added to the table
  #   includeNNamount : whether to include the amount of objects within amountRad (1/0)
  #   amountRad       : radius within which to look for objects
  #   amountIDname    : ID by which to collect objects in the radius
  #   amountName      : name of the object to look for in the radius
  #   pandensetRepeat : switch whether the coupling is the first of the pandenset (0) or a repeat (1)
  #   memorySet       : input for the coupling when Set1Name is "inMemory"
  #   key1_nameA      : first name for key 1
  #   key2_nameA      : first name for key 2
  #   key1_nameB      : second name for key 1 (if used)
  #   key2_nameB      : second name for key 2 (if used)
  #   
  #   ############################################
  #   #End of explanation of user input          #
  #   ############################################
  
  
   
  #Load in the sets to be coupled. I do this by loading the object pointer into a variable
  #(Set?NameCheck) and then performing a get on that to store the object in my preferred data table
  if(Set1Name=="InMemory"){
    Set1=memorySet
  }else{
    Set1NameCheck=load(Set1Name)
    Set1=get(Set1NameCheck)
  }
  
  
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

  if (cleantextkey==1){
    Set1[,eval(key1_nameA):=gsub(" ","",tolower(get(key1_nameA)))]
    Set2[,eval(key2_nameA):=gsub(" ","",tolower(get(key2_nameA)))]
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
      indexNearest=nn2(Set2Sub,Set1Sub,k=10)
      
      #identify the points within the desired radius
      withinRad=indexNearest$nn.dists<amountRad
    
      cat("REWRITE THIS LOOP INTO SOMETHING FASTER!!!, \n")
      
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

  if (outFileName=="InMemory"){
    cat("Keeping coupled set in memory \n")
  }else{
    mindataset=coupledSet
    cat("Saving coupled set \n")
    save(mindataset,file=outFileName,compress=F)
  }
  cat("Freeing memory \n")
  gc()
  
  cat("Done! \n")
  
  return(coupledSet)
}

splitDate = function(inputSet=mindataset){
  
  Names_Date_Cols=which(sapply(inputSet,class)=="Date")
  
  for (i in names(Names_Date_Cols)){
    Dag=substr(inputSet[,get(i)],9,10)
    Maand=substr(inputSet[,get(i)],6,7)
    Jaar=substr(inputSet[,get(i)],1,4)
    
    DagName=paste0(i,"_Dag")
    MaandName=paste0(i,"_Maand")
    JaarName=paste0(i,"_Jaar")
    
    inputSet[,eval(DagName):=as.numeric(Dag)]
    inputSet[,eval(MaandName):=as.numeric(Maand)]
    inputSet[,eval(JaarName):=as.numeric(Jaar)]
  }
  
  return(inputSet)
}

CoupleNORproxy = function(NORset,ProxySet,Settype="LSkabels"){
  cat("Starting coupling of NOR to Proxy \n")
  cat("We assume that the NORset is already subsampled for settype, if not, please correct \n")
  
  #first select the correct proxylist based on the settype
  switch(Settype,
         LSkabels={CurrentProxyList=ProxySet$LSkabels},
         MSkabels={CurrentProxyList=ProxySet$MSkabels},
         LSmoffen={CurrentProxyList=ProxySet$LSmoffen},
         MSmoffen={CurrentProxyList=ProxySet$MSmoffen})
  
  
  #add a column with failure reason in 3 categories
  load(paste0(settings$Analyse_Datasets,"/1. Proxylijsten/Oorzaakclassificatie.Rda"))
  setkey(OorzaakClassificatie,Oorzaak)
  setkey(CurrentProxyList,Oorzaak)
  
  CurrentProxyList=OorzaakClassificatie[CurrentProxyList]
  
  
  #check whether ID_unique in the koppelijst has PC6 by making it numeric, if so: cut off the last 4 characters because we couple to PC2
  if (sum(is.na(as.numeric(CurrentProxyList$ID_unique)))>100){
    CurrentProxyList$ID_unique=substr(CurrentProxyList$ID_unique,1,nchar(CurrentProxyList$ID_unique)-4)
  }
  
  #select only the most likely failed asset for each ID_KLAK_Melding
#   CurrentProxyList[,maxpuntenKLAK := max(punten),by=ID_KLAK_Melding]
#   CurrentProxyList=CurrentProxyList[punten==maxpuntenKLAK,]
#   setkey(CurrentProxyList,ID_KLAK_Melding)
#   CurrentProxyList=unique(CurrentProxyList)
  
  #Sum punten by ID_unique when it is found in more failures
  CurrentProxyList[,punten := sum(punten),by=ID_unique]
  
  #select one of the ID_uniques at random when punten is equal
  setkey(CurrentProxyList,ID_unique)
  CurrentProxyList=unique(CurrentProxyList)
  
  #clean the koppel table to only include the useful fields
  CurrentProxyList=CurrentProxyList[,list(ID_unique,punten,Oorzaak,Oorzaakklasse,ID_KLAK_Melding)]
  
  #Now do the coupling to the nor set
  #First set the keys to couple on
  setkey(CurrentProxyList,ID_unique)
  if ("i.ID_unique" %in% names(NORset)){
    setkey(NORset,i.ID_unique)
  }else{setkey(NORset,ID_unique)}
  
  #setkey(CurrentProxyList,ID_unique)
  
  #perform the coupling and set anything that doesn't couple to 0 (because there is no failure in that case)
  coupledNOR=CurrentProxyList[NORset]
  coupledNOR[is.na(punten),punten:=0]
    
  #correct missing PC6 values
  if("i.PC_6_van" %in% colnames(coupledNOR)){
    coupledNOR[is.na(PC_6_van),PC_6_van:=i.PC_6_van]
    coupledNOR[is.na(PC_6_naar),PC_6_naar:=i.PC_6_naar]
  }
  
  #remove rows with missing X,Y coordinates
  coupledNOR=coupledNOR[is.na(Coo_X_van)==F | is.na(Coo_Y_van)==F,]
  
  
  #return the coupled set
  return(coupledNOR)
  
  
}

CalcLoadIndicators=function(inputSet){
  #Note, inputSet needs to contain a column Max_Belasting, ID_Hoofdleiding and the five load metrics
  
  #First we calculate the value of the max belasting of the HLD, assuming that this is the one the highest in 
  #the tree and that load fractions are equally distributed over the HLD.
  #We also add metrics for mean I squared to be weighted against
  inputSet[is.na(Max_Belasting),Max_Belasting:=-100]
  inputSet[,Max_Belasting_HLD := max(Max_Belasting),by=ID_Hoofdleiding_present]  
  inputSet[,Max_Belasting_HLD_squared := max(Max_Belasting)*abs(max(Max_Belasting)),by=ID_Hoofdleiding_present]
  inputSet[Max_Belasting==-100,Max_Belasting:=NA]
  inputSet[Max_Belasting_HLD==-100,Max_Belasting_HLD:=NA]
  inputSet[Max_Belasting_HLD_squared==-10000,Max_Belasting_HLD_squared:=NA]
  
  
  #Now we calculate the fractional numerics
  inputSet[,Max_Load_Fraction:=Max_I/Max_Belasting_HLD] #fractional maximum load
  inputSet[,Mean_Load_Fraction:=Mean_I/Max_Belasting_HLD] #fractional mean load
  inputSet[,MeanSq_Load_Fraction:=Mean_I_squared/Max_Belasting_HLD_squared] #fractional mean squared load
  inputSet[,Max2h_Load_Fraction:=Maxyear_min2h/Max_Belasting_HLD] #fractional maximum load over 2 hour span
  
  
  return(inputSet)
}

TargetVariables = function(coupledNOR,threshold=0.3){
  
  #Now calculate the gestoord/niet gestoord markers based on the provided threshold for all different permutations of oorzaakklasse
  for (i in 1:length(threshold)){
    currentTH=threshold[i]
    
    #targetvariable names
    allname=paste0("gestoordAll_th",currentTH)
    assetfailname=paste0("gestoordAsset_th",currentTH)
    digfailname=paste0("gestoordGraaf_th",currentTH)
    otherfailname=paste0("gestoordOverig_th",currentTH)
    notassetfailname=paste0("gestoordNietAsset_th",currentTH)
    notdigfailname=paste0("gestoordNietGraaf_th",currentTH)
    nototherfailname=paste0("gestoordNietOverig_th",currentTH)
    
    coupledNOR[punten>currentTH,eval(allname):="T"]
    coupledNOR[!(punten>currentTH),eval(allname):="F"]
    
    coupledNOR[punten>currentTH & Oorzaakklasse=="Assetfalen",eval(assetfailname):="T"]
    coupledNOR[!(punten>currentTH) | !Oorzaakklasse=="Assetfalen",eval(assetfailname):="F"]
    
    coupledNOR[punten>currentTH & Oorzaakklasse=="Graafschade",eval(digfailname):="T"]
    coupledNOR[!(punten>currentTH) | !Oorzaakklasse=="Graafschade",eval(digfailname):="F"]
    
    coupledNOR[punten>currentTH & Oorzaakklasse=="Overig",eval(otherfailname):="T"]
    coupledNOR[!(punten>currentTH) | !Oorzaakklasse=="Overig",eval(otherfailname):="F"]
    
    coupledNOR[punten>currentTH & !Oorzaakklasse=="Assetfalen",eval(notassetfailname):="T"]
    coupledNOR[!(punten>currentTH) | Oorzaakklasse=="Assetfalen",eval(notassetfailname):="F"]
    
    coupledNOR[punten>currentTH & !Oorzaakklasse=="Graafschade",eval(notdigfailname):="T"]
    coupledNOR[!(punten>currentTH) | Oorzaakklasse=="Graafschade",eval(notdigfailname):="F"]
    
    coupledNOR[punten>currentTH & !Oorzaakklasse=="Overig",eval(nototherfailname):="T"]
    coupledNOR[!(punten>currentTH) | Oorzaakklasse=="Overig",eval(nototherfailname):="F"]
    
  }
  
  return(coupledNOR)
}


ExtractMofData=function(mindataset){
  #Written by Michiel Musterd - 10-03-2014
  #This script is meant to extract useful categorical data from the constructie and soort entries in the NOR for moffen
  MofSet=data.table(LowConstructie=gsub(" ","",tolower(as.character(mindataset[,Constructie]))),
                    LowSoort=tolower(as.character(mindataset[,Soort])))
  
  #Categorize the constructie in a few useful groups
  # Wikkelmof Filoform
  # Wikkelmof Cellpack
  # Wikkelmof overig
  # Kunstharsmof
  # Nekaldietmof
  # Krimpmof Raychem
  # Krimpmof Cellpack
  # Krimpmof overig
  # Gietharsmof
  # Oliemof
  # Overig
  # Onbekend  
  MofSet[grep("wikkelmof",MofSet$LowConstructie),Constructie:="Wikkelmof overig"]
  MofSet[grep("krimpmof",MofSet$LowConstructie),Constructie:="Krimpmof overig"]
  #note, the two commands above should be run first because they are more general then two below
  
  MofSet[grep("wikkelmoffiloform",MofSet$LowConstructie),Constructie:="Wikkelmof Filoform"]
  MofSet[grep("wikkelmofcellpack",MofSet$LowConstructie),Constructie:="Wikkelmof Cellpack"]
  MofSet[grep("krimpmofraychem",MofSet$LowConstructie),Constructie:="Krimpmof Raychem"]
  MofSet[grep("krimpmofcellpack",MofSet$LowConstructie),Constructie:="Krimpmof Cellpack"]
  MofSet[grep("kunsthars",MofSet$LowConstructie),Constructie:="Kunstharsmof"]
  MofSet[grep("nekaldiet",MofSet$LowConstructie),Constructie:="Nekaldietmof"]
  MofSet[grep("giethars",MofSet$LowConstructie),Constructie:="Gietharsmof"]
  MofSet[grep("olie",MofSet$LowConstructie),Constructie:="Oliemof"]
  MofSet[is.na(MofSet$Constructie),Constructie:="Overig"]
  MofSet[is.na(MofSet$LowConstructie),Constructie:="Onbekend"]
  MofSet[grep("onbekend",MofSet$LowConstructie),Constructie:="Onbekend"]
  MofSet$Constructie=as.factor(MofSet$Constructie)
  
  #Categorize the soort and change into more readable names
  MofSet[grep("tn|vm|verbindingsmof",MofSet$LowSoort),Soort:="Verbindingsmof"]
  MofSet[grep("em|gm",MofSet$LowSoort),Soort:="Eindmof"]
  MofSet[grep("ae|tae",MofSet$LowSoort),Soort:="Aftak-Eindmof"]
  MofSet[grep("vam|am",MofSet$LowSoort),Soort:="Aftakmof"]
  MofSet[grep("aam|ap",MofSet$LowSoort),Soort:="Aansluitmof"]
  MofSet[is.na(MofSet$Soort),Soort:="Overig"]
  MofSet[grep("onbekend",MofSet$LowSoort),Constructie:="Onbekend"]
  MofSet[is.na(MofSet$LowSoort),Constructie:="Onbekend"]
  MofSet$Soort=as.factor(MofSet$Soort)
  
  
  mindataset$Soort=MofSet$Soort
  mindataset$Constructie=MofSet$Constructie
  
  return(mindataset)
}


ExtractCableData=function(mindataset){
  
  #Written by Michiel Musterd - 10-03-2014
  #This script is meant to extract useful categorical data from the Fabrikanttype entry in the NOR for cables
  
  #Note: the extra cable information after the + in the fabrikanttype is about extra cables for specific 
  #goals e.g. street lighting or grounding. We will ignore those in our specification
  
  #load and preprocess the data (lowercase and , to . conversion)
  FabrikantSet=data.table(LowFabrikanttype=gsub(",",".",tolower(as.character(mindataset[,Fabrikanttype]))))
  
  
  #Isolatiemedium herkenning: Algemene classificering en bijbehorende namen
  # YMvK-asmb
  # VMvKh-sas
  # GPLK
  # XLPE:  Overige YMvK en XLPE
  # Overig Kunststof: VM (en VMvK)
  # Overig
  # Onbekend
  FabrikantSet[grep("xlpe|ymvk|ymek",FabrikantSet$LowFabrikanttype),Isolatie:="XLPE"]
  FabrikantSet[grep("vm",FabrikantSet$LowFabrikanttype),Isolatie:="Overig Kunststof"]
  #note, the two commands above should be run first because they are more general then two below
  
  FabrikantSet[grep("ymvk-asmb",FabrikantSet$LowFabrikanttype),Isolatie:="YMvK-asmb"]
  FabrikantSet[grep("vmvkh-sas",FabrikantSet$LowFabrikanttype),Isolatie:="VMvKh-sas"]
  FabrikantSet[grep("gplk",FabrikantSet$LowFabrikanttype),Isolatie:="GPLK"]
  FabrikantSet[is.na(FabrikantSet$Isolatie),Isolatie:="Overig"]
  FabrikantSet[is.na(FabrikantSet$LowFabrikanttype),Isolatie:="Onbekend"]
  FabrikantSet[grep("onbekend",FabrikantSet$LowFabrikanttype),Isolatie:="Onbekend"]
  FabrikantSet$Isolatie=as.factor(FabrikantSet$Isolatie)
  
  #First strip away anything after the +
  FabrikantSet[,CutType:=gsub("\\+.*","",FabrikantSet$LowFabrikanttype)]
  
  #Then also remove the spaces around an x, first remove XLPE to avoid issues
  FabrikantSet[,CutType:=gsub("xlpe","",CutType)]
  FabrikantSet[,CutType:=gsub(" x|x ","x",CutType)]
  
  
  #recognize rows with multiple x's
  # FabrikantSet[grep(".*x.*x.*",FabrikantSet$CutType),]
  
  
  #Herkenning aantal aders - is het eerste getal voor de x (aanname: aders<10)
  x_location=gregexpr("x",FabrikantSet$CutType)
  FabrikantSet[,xloc:=rapply(x_location, function(x) head(x, 1))]
  FabrikantSet[,Aantal_Aders:=as.factor(as.numeric(substr(FabrikantSet$CutType,xloc-1,xloc-1)))]
  FabrikantSet[is.na(Aantal_Aders),Aantal_Aders:="Onbekend"]
  
  
  #Herkenning geleidermateriaal
  FabrikantSet[grep("al",CutType),Geleidermateriaal:="Al"]
  FabrikantSet[grep("cu",CutType),Geleidermateriaal:="Cu"]
  FabrikantSet[is.na(Geleidermateriaal),Geleidermateriaal:="Onbekend"]
  FabrikantSet$Geleidermateriaal=as.factor(FabrikantSet$Geleidermateriaal)
  
  #Herkenning diameter kabel
  char1=as.numeric(substr(FabrikantSet$CutType,FabrikantSet$xloc+1,FabrikantSet$xloc+1))
  char2=as.numeric(substr(FabrikantSet$CutType,FabrikantSet$xloc+1,FabrikantSet$xloc+2))
  char3=as.numeric(substr(FabrikantSet$CutType,FabrikantSet$xloc+1,FabrikantSet$xloc+3))
  char4=as.numeric(substr(FabrikantSet$CutType,FabrikantSet$xloc+1,FabrikantSet$xloc+4))
  FabrikantSet[,Diameter_Kabel:=pmax(char1,char2,char3,char4,na.rm=T)]
  
  mindataset=cbind(mindataset,FabrikantSet[,list(Isolatie,Aantal_Aders,Geleidermateriaal,Diameter_Kabel)])
  
  return(mindataset)
  
}


