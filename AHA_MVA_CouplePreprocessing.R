#-----------------------------------------------------------------#
#-------- Written by Michiel Musterd (BearingPoint) 2015 ---------#
#-----------------------------------------------------------------#
# PURPOSE: 
# This file contains several functions that are used to clean,
# preprocess and perform calculations on raw datasets with the 
# purpose of producing datasets directly compatible with the 
# coupling routines in AHA_MVA_Coupling.R
#
# 
# INCLUDED FUNCTIONS (see each function for a description):
# ---
# AHA_MVA_CouplePreprocessing() - batch script to run all other functions
# BAGimport()
# BAGcleanup()
# CreateLoadIndicators()
# ReadNOR()
# CDBreadin()
# CDBcalcmetrics()
# LoadPerLSHLD()
# RisicoGridGen()
# RisicoMerge()
# geoQuery()
# KNMIconvertMonth()
# KNMIconvertYear()
# KNMIconvert20072014()
# LineToPointSet()
# LtoPcleanup()
# sample.line()
# GrondsoortsysGeo_Conversion()
# SpatialPoint_To_XY()
#-----------------------------------------------------------------#



AHA_MVA_CouplePreprocessing = function ()
{
  #Wrapper to call all preprocessing for the coupling
}


BAGimport = function (){
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


BAGcleanup = function(){
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

CreateLoadIndicators=function(){
  #koppel LS kabels aan belastingmetrics
  load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/LS_HLDbelastingindicators.Rda"))
  
  setkey(LSkabels,ID_Hoofdleiding_present)
  setkey(HLDmetrics,ID_Hoofdleiding)
  
  coupledSetLS=HLDmetrics[LSkabels] 
  #99.3% gevonden :)
  
  
  #koppel MS kabels aan belastingmetrics
  load(paste0(settings$Ruwe_Datasets,"/00. NOR input/MSkabels_NOR.Rda"))
  #load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/LS_HLDbelastingindicators.Rda"))
  
  fileHSDkoppelMpoint1=paste0(settings$Bron_Datasets,"/19. CDB/HSD_Meetpunt_koppel_MS_Hoofdleiding.ssv")
  fileNRGkoppelMpoint1=paste0(settings$Bron_Datasets,"/19. CDB/NRG_Meetpunt_koppel_MS_Hoofdleiding.ssv")
  HSD1koppel   = data.table(read.table(fileHSDkoppelMpoint1                  , sep = ";", dec="." ,header = TRUE))
  NRG1koppel   = data.table(read.table(fileNRGkoppelMpoint1                  , sep = ",", dec="." ,header = TRUE))
  
  fileHSDkoppelMpoint=paste0(settings$Bron_Datasets,"/19. CDB/HSD_Meetpunt_koppel_MS_Hoofdleiding2.ssv")
  fileNRGkoppelMpoint=paste0(settings$Bron_Datasets,"/19. CDB/NRG_Meetpunt_koppel_MS_Hoofdleiding2.ssv")
  HSDkoppel   = data.table(read.table(fileHSDkoppelMpoint                  , sep = ";", dec="." ,header = TRUE))
  NRGkoppel   = data.table(read.table(fileNRGkoppelMpoint                  , sep = ";", dec="." ,header = TRUE))
  
  setnames(NRG1koppel,"NUMMER","ID_HOOFDLEIDING")
  HSDkoppel1Clean=HSD1koppel[,list(M_POINT,MS_HLD_ID,ID_HOOFDLEIDING)]
  NRGkoppel1Clean=NRG1koppel[,list(M_POINT,MS_HLD_ID,ID_HOOFDLEIDING)]
  HSDkoppelClean=HSDkoppel[,list(M_POINT,MS_HLD_ID,ID_HOOFDLEIDING)]
  NRGkoppelClean=NRGkoppel[,list(M_POINT,MS_HLD_ID,ID_HOOFDLEIDING)]
  
  MSkoppelClean=rbind(HSDkoppelClean,NRGkoppelClean,HSDkoppel1Clean,NRGkoppel1Clean)
  
  
  #eerst koppelen we NRG en HSD koppels op Mpoint aan de indicators
  load(paste0(settings$Ruwe_Datasets,"/19. CDB/Mpoint_BelastingIndicators.Rda"))
  setkey(mindataset,M_Point)
  setkey(MSkoppelClean,M_POINT)
  
  
  coupledSetOnMP=mindataset[MSkoppelClean]
  setkey(coupledSetOnMP,MS_HLD_ID)
  setkey(MSkabels,ID_Verbinding_present)
  
  unicoupledSetOnMPIDV=(unique(coupledSetOnMP))
  unicoupledSetOnMPIDV$MS_HLD_ID=as.character(unicoupledSetOnMPIDV$MS_HLD_ID)
  setkey(unicoupledSetOnMPIDV,MS_HLD_ID)
  setkey(newcouple,ID_Verbinding_present)
  
  coupledSetMS=unicoupledSetOnMPIDV[newcouple]
  setnames(coupledSetMS,"MS_HLD_ID","ID_Verbinding_present")
  
  setkey(coupledSetOnMP,ID_HOOFDLEIDING)
  setkey(coupledSetMS,ID_Hoofdleiding_present)
  unicoupledSetOnMPIDH=(unique(coupledSetOnMP))
  
  
  
  coupledSetMSVcombined=unicoupledSetOnMPIDH[coupledSetMS]
  setnames(coupledSetMSVcombined,"ID_HOOFDLEIDING","ID_Hoofdleiding_present")
  
  View(coupledSetMSVcombined[!is.na(M_Point) | !is.na(i.M_Point),])
  nrow(coupledSetMSVcombined[!is.na(M_Point) | !is.na(i.M_Point),])
  nrow(coupledSetOnMP[!is.na(M_Point),])
  nrow(coupledSetMS[!is.na(M_Point),])
  #11.3 procent met M_point of i.M_point gevuld
}

ReadNOR = function (){
  #Voorbereiding NOR naar LS, MS kabels en moffen
  
  #splitsing naar moffen en kabels
  moffen=assets$moffen
  kabels=assets$kabels
  
  #splitsing naar LS en MS
  #kabels
  setkey(kabels,Netvlak)
  LSkabels=kabels[Netvlak=="LS",]
  MSkabels=kabels[Netvlak=="MS",]
  
  #moffen
  setkey(moffen,Netvlak)
  LSmoffen=moffen[Netvlak=="LS",]
  MSmoffen=moffen[Netvlak=="MS",]
  
  #remove entries with Coo_X_van NA
  LSkabels=LSkabels[!is.na(Coo_X_van),]
  MSkabels=MSkabels[!is.na(Coo_X_van),]
  LSmoffen=LSmoffen[!is.na(Coo_X_van),]
  MSmoffen=MSmoffen[!is.na(Coo_X_van),]
  
  
  #save all the sets (without compression)
  save(LSkabels,file=paste0(settings$Ruwe_Datasets,"/00. NOR input/LSkabels_NOR.Rda"),compress=F)
  save(MSkabels,file=paste0(settings$Ruwe_Datasets,"/00. NOR input/MSkabels_NOR.Rda"),compress=F)
  save(LSmoffen,file=paste0(settings$Ruwe_Datasets,"/00. NOR input/LSmoffen_NOR.Rda"),compress=F)
  save(MSmoffen,file=paste0(settings$Ruwe_Datasets,"/00. NOR input/MSmoffen_NOR.Rda"),compress=F)
  
}

#Written by Michiel Musterd 17-03-2015
library("caTools")

CDBreadin=function(){
  #This function reads in the CDB file (put in a directory on my C-drive because that is an SSD)
  #It subsquently splits the file based on MeetID, where we use a trick to avoid searching 
  #of IDs (which is extremely slow): we count how many of each ID are there, then we merge
  #this as an extra column in the CDB set and sort on both frequency and MeetID (only MeetID would also work). We then
  #loop over the CDB file, using the frequency column to calculate where the current MeetID ends
  #and the next begins.
  
  
  #needs 3 seconds per 1 million lines, so for the ~600 million lines it will need about half an hour
  temp=read.csv2.ffdf(file="C:/Users/michiel.musterd/Documents/Alliander project/14. CDBfile/cdb_spool.txt",
                      colClasses=c("factor", "factor","double","integer"),sep="*",header=F,dec=".",VERBOSE=T,
                      first.rows=1000000, next.rows=1000000)
  
  
  CDBset=ffdf(MeetID=temp$V1,DatumTijd=temp$V2,Meetwaarde=temp$V3,Flag=temp$V4)
  
  #Now split this set in its different meetIDs and store each file separately
  
  
  countMeetIDs=as.ffdf(data.table(count(CDBset$MeetID[1:length(CDBset$MeetID),])))
  tempout=merge(CDBset,countMeetIDsff,by="MeetID")
  
  idx=ffdforder(tempout[c("freq","MeetID")])
  CDBorderedSet=tempout[idx,]
  
  
  startID=1
  endID=0
  
  for (i in 1:length(levels(CDBorderedSet$MeetID))){
    ptm=proc.time()
    
    endID=startID+CDBorderedSet[startID,"freq"]-1
    
    CDBsingleMeetID=as.data.table(as.data.frame(CDBorderedSet[startID:endID,]))
    
    CDBsingleMeetID$DatumTijd=as.character(CDBsingleMeetID$DatumTijd)
    CDBsingleMeetID[,Datum:=paste0(substr(CDBsingleMeetID$DatumTijd,7,10),"-",substr(CDBsingleMeetID$DatumTijd,4,5),"-",substr(CDBsingleMeetID$DatumTijd,1,2))]
    CDBsingleMeetID[,Tijd:=substr(CDBsingleMeetID$DatumTijd,12,20)]
    CDBsingleMeetID$DatumTijd=NULL
    
    setkey(CDBsingleMeetID,Datum,Tijd)
    
    filename=paste0(settings$Ruwe_Datasets,"/19. CDB/",CDBsingleMeetID$MeetID[1],".Rda")
    save(CDBsingleMeetID,file=filename,compress=F)
    
    cat(paste0(i, " done -- runtime (s):", proc.time()[3]-ptm[3],"\n"))
    
    cat(startID," ",endID," ",as.character(CDBorderedSet[startID,"MeetID"]), " ",as.character(CDBorderedSet[endID,"MeetID"])," ",
        as.character(CDBorderedSet[startID,"freq"])," ",as.character(CDBorderedSet[endID,"freq"]),"\n")
    
    startID=endID+1
  }
  
  save(countMeetIDs,file=paste0(settings$Ruwe_Datasets,"/19. CDB/ListOfMeetIDs.Rda"))
  
}

CDBcalcmetrics=function(){
  
  #This function takes the output of the CDBreadin function and uses it to construct
  #5 metrics for each MeetID: mean(I), I_max, mean(I^2), mean ((dI/dt)^2), max_year(min_2h I) where Delta
  #is the derivative of I at each timestep
  
  #read in ListOfMeetIDs to know which files to load
  load(paste0(settings$Ruwe_Datasets,"/19. CDB/ListOfMeetIDs.Rda"))
  namesSelect=countMeetIDs$MeetID
  
  #now calculate the metrics for each MeetID
  cntr=0
  Isqmean=(1:length(namesSelect))*0
  Imean=(1:length(namesSelect))*0
  Imax=(1:length(namesSelect))*0
  dIdTsquared=(1:length(namesSelect))*0
  maxyear_min2h=(1:length(namesSelect))*0
  
  
  ptm=proc.time()  
  for (i in namesSelect){
    cat("Cntr:", cntr," -- Mpoint: ",i," -- Runtime (s): ",proc.time()[3]-ptm[3],"\n")
    load(paste0(settings$Ruwe_Datasets,"/19. CDB/",i,".Rda"))
    cntr=cntr+1
    
    
    #calculate the mean I and mean of square I
    Isqmean[cntr]=mean((CDBsingleMeetID$Meetwaarde)^2)
    Imean[cntr]=mean(CDBsingleMeetID$Meetwaarde)
    Imax[cntr]=max(CDBsingleMeetID$Meetwaarde)
    
    
    
    
    #add a time/date field usuable for time computations
    #     CDBsingleMeetID[,DatumTijd:=paste0(CDBsingleMeetID$Datum," ",CDBsingleMeetID$Tijd)]
    #     strptime(CDBsingleMeetID$DatumTijd,"%Y-%m-%d %H:%M:%S")
    #     
    #     CDBsingleMeetIDtemp[,DatumTijdCalc:=strptime(CDBsingleMeetIDtemp$DatumTijd,"%Y-%m-%d %H:%M:%S")]
    
    
    #we assume the timestep to be 300 seconds, we could calculate it, but won't for now
    DeltaTijd=rep(300,nrow(CDBsingleMeetID))
    #note Tijd is in seconds
    
    
    
    #calculate Delta I for all   
    DIend=CDBsingleMeetID$Meetwaarde[nrow(CDBsingleMeetID)]+(CDBsingleMeetID$Meetwaarde[nrow(CDBsingleMeetID)]-CDBsingleMeetID$Meetwaarde[nrow(CDBsingleMeetID)-1])
    DeltaI=c(CDBsingleMeetID$Meetwaarde[2:nrow(CDBsingleMeetID)],DIend)-CDBsingleMeetID$Meetwaarde
    
    
    
    #Now we can calculate the derivatives and the derivative of I squared
    dIdTsquared[cntr]=mean((DeltaI/DeltaTijd)^2)
    
    #Finally we compute the minimum per 2h (7200 s) frame and then the max of that for the year
    
    
    
    maxyear_min2h[cntr]=max(runmin(CDBsingleMeetID$Meetwaarde, round(7200/mean(DeltaTijd))))
    
    
  }
  
  mindataset=data.table(M_Point=namesSelect,Mean_I=Imean,Max_I=Imax,Mean_I_squared=Isqmean,Mean_squared_dIdt=dIdTsquared,Maxyear_min2h=maxyear_min2h)
  
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/19. CDB/Mpoint_BelastingIndicators.Rda"))
  
  return(mindataset)
  
}

LoadPerLSHLD = function(){
  #### 1. load data and create variables for profiles and standard yearly use
  library(slam)       #Used for sparse matrices
  library(tictoc)    
  library(ggplot2)
  library(doSNOW)
  library(utils)
  library(caTools)
  
  #### 2. Create interconnectionmatrix function from KVEAN to HLD 
  CreateInterconnectionMatrix <- function(FromTo,FromReferenceList,ToReferenceList,makeFromUnique) {
    #INPUT: 
    #FromTo            = elements which will be coupled (example c(MSR$HOOFDLEIDING,MSR$MSR))
    #FromReferenceList = unique list of From-elements (example: variable 'HLD')
    #ToReferenceList   = unique list of To-elements (example: variable 'MSRlist')
    
    # First determine dimensions of the "from" side (e.g. columns of the interconnection matrix)
    # We can either make an interconnection matrix which makes connections from each unique element in "From" 
    # to each unique element in "To", or we can make a matrix which allows duplicate entries in the "From"
    if(makeFromUnique) { 
      FromTo        = unique(FromTo)
      Fromindexlist = match(FromTo[,1],FromReferenceList)
      ncols         = length(FromReferenceList)
      v_weights      = data.matrix(table(Fromindexlist))
      v             = 1/v_weights[match(Fromindexlist,row.names(v_weights))]
    } else {
      Fromindexlist = 1:length(FromTo[,1])
      ncols         = length(FromTo[,1])
      v_index       = FromTo[Fromindexlist,1]
      v_weights     = data.matrix(table(v_index))
      v             = 1/v_weights[match(v_index,row.names(v_weights))]
    }
    
    Toindexlist   = match(FromTo[,2],ToReferenceList)     # Find the "to" element which corresponds to each "from"  
    NANlist       = is.na(Toindexlist)==FALSE  
    i             = Toindexlist[NANlist]
    j             = Fromindexlist[NANlist] 
    v             = v[NANlist]
    nrows         = length(ToReferenceList)
    
    # Create output matrix
    Outputmatrix = simple_triplet_matrix(i, j, v, nrows, ncols, dimnames = NULL) 
    return(Outputmatrix)
  }
  
  #create metrics function
  CalcMetrics=function(HLDloadTimeSeries){
    #calculate the metrics on the profiles
    
    
    Isqmean=mean((HLDloadTimeSeries)^2)
    Imean=mean(HLDloadTimeSeries)
    Imax=max(HLDloadTimeSeries)
    
    
    #we assume the timestep to be 900 seconds, we could calculate it, but won't for now
    DeltaTijd=rep(900,length(HLDloadTimeSeries))
    #note Tijd is in seconds
    
    
    #calculate Delta I for all   
    DIend=HLDloadTimeSeries[length(HLDloadTimeSeries)]+(HLDloadTimeSeries[length(HLDloadTimeSeries)]-HLDloadTimeSeries[length(HLDloadTimeSeries)-1])
    DeltaI=c(HLDloadTimeSeries[2:length(HLDloadTimeSeries)],DIend)-HLDloadTimeSeries
    
    #Now we can calculate the derivatives and the derivative of I squared
    dIdTsquared=mean((DeltaI/DeltaTijd)^2)
    
    #Finally we compute the minimum per 2h (7200 s) frame and then the max of that for the year
    maxyear_min2h=max(runmin(HLDloadTimeSeries, round(7200/mean(DeltaTijd))))
    
    return(c(Imean,Imax,Isqmean,dIdTsquared,maxyear_min2h))
    
  }
  
  
  #### 3. Load files and start processing
  EDSNfilename=paste0(settings$Ruwe_Datasets,"/11. Nettopologie/EDSN2014.csv")
  Aansluitingenname=paste0(settings$Ruwe_Datasets,"/11. Nettopologie/EAN_LS_HLDtable.Rda")
  #Aansluitingenname=paste0(settings$Ruwe_Datasets,"/11. Nettopologie/EAN_LS_Aansluitingen_XY_PC6.Rda")
  
  
  EDSN   = read.table(EDSNfilename                     , sep = ";", dec=".", colClasses = c(rep("character",3),rep("numeric",10)) ,header = TRUE)
  load(Aansluitingenname)
  
  #remove empty rows that cannot be coupled anyway
  mindataset=mindataset[ID_Hoofdleiding!="",]
  
  #prepare the HLDmetrics table
  HLDmetrics=data.table(ID_Hoofdleiding="",Mean_I=0,Max_I=0,Mean_I_squared=0,Mean_squared_dIdt=0,Maxyear_min2h=0)
  
  
  ##MM - opzet voor loop over de HDLs
  #sort the dataset on hoofdleiding to allow cutting it into pieces for the calculations (to avoid memory issues)
  setkey(mindataset,ID_Hoofdleiding)
  
  N_rows_per_cycle=200 #note, this is approximate, we need to check where the next ID_hoofdleiding starts to know where to cut
  cycle_IDHLD=mindataset[N_rows_per_cycle+1,ID_Hoofdleiding]
  Actual_row_start=1
  Actual_row_end=max(which(mindataset$ID_Hoofdleiding==cycle_IDHLD))
  
  #MM TODO: build a while loop around this to step through all rows of mindataset instead of only 1:Actual_row_end
  EndSwitch=1
  ptm=proc.time()
  while (EndSwitch<2){
    MSR=as.data.frame(mindataset[Actual_row_start:Actual_row_end],)
    
    KVbaseprofile = data.matrix(EDSN[,4:dim(EDSN)[2]])
    SJV    = as.numeric(sub(",",".",MSR$SJV))           #Yearly electricity use in kWh
    SJVlow = as.numeric(sub(",",".",MSR$SJV_Laag))      #SJVlow is the SJV during the 'daluren'
    SJVlow[is.na(SJVlow)] = 0                                              #Remove missing entries
    SJV[is.na(SJV)] = 0                                                    #Remove missing entries
    SJV = SJV + SJVlow
    
    #### 4. Get list of unique KV-EANs and HLDs
    KVEAN   = unique(MSR$ID_EAN)
    HLD     = sort(unique(MSR$ID_Hoofdleiding))
    
    
    
    tic()
    KVEANtoHLD  = CreateInterconnectionMatrix(MSR[c("ID_EAN","ID_Hoofdleiding")],KVEAN,HLD,FALSE)
    toc()
    nHLD        = dim(KVEANtoHLD)[1]
    # nHLDstart=nHLDend+1
    # nHLDend=nHLDstart+nHLD
    
    #### 5. Generate KV baseload profiles
    EDSNperEAN       = matrix(0,length(SJV),length(colnames(KVbaseprofile)))     # matrix with nrow = EAN and ncol = nEDSNprofiles
    profilenumber    = match(MSR$Profiel_Type,colnames(KVbaseprofile))           # indexvector to store the correct profiles in the correct places
    for (i in 1:length(SJV)) {EDSNperEAN[i,profilenumber[i]]=SJV[i]*4}           # fill matrix with per EDSNperEAN[EAN,profile] = SJV (* 4 to convert kWh to kW)
    EDSNperHLD       = t(matprod_simple_triplet_matrix(KVEANtoHLD, EDSNperEAN))  # matrix with nrow = nHLD, ncol = nEDSNprofiles
    KVbaseloadperHLD = matrix(NA,nHLD,365*24*4)                                  # This can become quite big depending on the number of HLDs
    for (i in 1:nHLD)  {
      #cat(i," of ",nHLD,"\n")
      KVbaseloadperHLD[i,] = KVbaseprofile %*% EDSNperHLD[,i]} # matrix multiplication, output = nrow = nHLD, ncol = 1
    
    #put the matrix in a datatable for easy handling
    KVloadperHLDTable=data.table(t(KVbaseloadperHLD))
    setnames(KVloadperHLDTable,HLD)
    
    #for each HLD, calculate the metrics
    tempHLDmetrics=data.table(ID_Hoofdleiding="",Mean_I=0,Max_I=0,Mean_I_squared=0,Mean_squared_dIdt=0,Maxyear_min2h=0)
    cntr=0
    for (i in names(KVloadperHLDTable)){
      output=CalcMetrics(KVloadperHLDTable[,get(i)])
      cntr=cntr+1
      rowDT=data.table(i,t(output))
      setnames(rowDT,names(rowDT),c("ID_Hoofdleiding","Mean_I","Max_I","Mean_I_squared","Mean_squared_dIdt","Maxyear_min2h"))
      
      if (cntr==1){tempHLDmetrics=rowDT}else{tempHLDmetrics=rbind(tempHLDmetrics,rowDT)}
      
    }
    
    if (nrow(HLDmetrics)==1){HLDmetrics=tempHLDmetrics}else{HLDmetrics=rbind(HLDmetrics,tempHLDmetrics)}
    
    Actual_row_start=Actual_row_end+1
    if ((Actual_row_end+1+N_rows_per_cycle)<nrow(mindataset)) {
      cycle_IDHLD=mindataset[Actual_row_end+1+N_rows_per_cycle,ID_Hoofdleiding]
    }else{
      cycle_IDHLD=mindataset[nrow(mindataset),ID_Hoofdleiding]
    }
    
    Actual_row_end=max(which(mindataset$ID_Hoofdleiding==cycle_IDHLD))
    
    
    if (Actual_row_end==nrow(mindataset)){
      EndSwitch=EndSwitch+1
    }
    cat("Row start: ",Actual_row_start," --- Row end: ",Actual_row_end," --- Runtime: ",proc.time()[3]-ptm[3],"\n")
    
  }#end of while loop on endswitch
  
  #save the HLDmetrics
  save(HLDmetrics,file=paste0(settings$Ruwe_Datasets,"/11. Nettopologie/LS_HLDbelastingindicators.Rda"),compress=F)
}


#Written by Michiel Musterd - 25-02-2015
#---------------------------------------
#Script containing the necessary functions to group the geoquery information in risicokaart
#to an X,Y grid that can be coupled to the NOR later.

#Mini function to generate a row of X,Y coordinates that together form a grid [Xmin,Xmax]_[Ymin,Ymax]
RisicoGridGen = function (stepX=500,stepY=500){
  
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
RisicoMerge = function(stepX=20000,stepY=20000){
  
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

#Written by Michiel Musterd - 26-02-2015
#Script to convert the KNMI set into the set needed for the MVA

KNMIconvertMonth = function (){
  
  #read in the KNMI set
  load(paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_20150223_v02.Rda"))
  
  count(mindataset[,Plaatsnaam_Weerstation])
  
  #add info to the dataset indicating the month
  
  maand_index=data.table(regexpr("-",mindataset$Datum_Meting))
  mindataset[,Maand_Jaar_Meting:=substr(Datum_Meting,maand_index$V1+1,nchar(Datum_Meting))]
  
  
  
  #add info indicating whether this is a dry day or not
  mindataset[,Dry_Days:=ifelse(is.na(as.numeric(Etmaalsom_Neerslag)),NA,ifelse(as.numeric(Etmaalsom_Neerslag)>0,0,1))]
  
  #select the different pieces of information per station and month
  meanTemp=mindataset[,mean(as.numeric(Etmaalgemiddelde_Temperatuur)),by=c("Plaatsnaam_Weerstation","Maand_Jaar_Meting")]
  sumRain=mindataset[,sum(as.numeric(Etmaalsom_Neerslag)),by=c("Plaatsnaam_Weerstation","Maand_Jaar_Meting")]
  dryDays=mindataset[,sum(as.numeric(Dry_Days)),by=c("Plaatsnaam_Weerstation","Maand_Jaar_Meting")]
  
  setkey(meanTemp,Plaatsnaam_Weerstation,Maand_Jaar_Meting)
  setkey(sumRain,Plaatsnaam_Weerstation,Maand_Jaar_Meting)
  setkey(dryDays,Plaatsnaam_Weerstation,Maand_Jaar_Meting)
  setkey(mindataset,Plaatsnaam_Weerstation,Maand_Jaar_Meting)
  
  unimindataset=unique(mindataset)
  
  CoupledT=unimindataset[meanTemp]
  CoupledTR=CoupledT[sumRain]
  CoupledTRD=CoupledTR[dryDays]
  setnames(CoupledTRD,c("V1","i.V1","i.V1.1"),c("gem_temperatuur","som_neerslag","aantal_drogedagen"))
  
  jr_index=data.table(regexpr("-",CoupledTRD$Maand_Jaar_Meting))
  CoupledTRD[,Maand_Meting:=substr(Maand_Jaar_Meting,1,jr_index$V1-1)]
  CoupledTRD[,Jaar_Meting:=substr(Maand_Jaar_Meting,jr_index$V1+1,nchar(Maand_Jaar_Meting))]
  
  mindataset=CoupledTRD[,c("Coo_X","Coo_Y","Plaatsnaam_Weerstation","Maand_Jaar_Meting","Maand_Meting","Jaar_Meting","gem_temperatuur","som_neerslag","aantal_drogedagen"),with=F]
  
  #set the classes properly
  mindataset$Coo_X=as.numeric(mindataset$Coo_X)
  mindataset$Coo_Y=as.numeric(mindataset$Coo_Y)
  mindataset$Maand_Meting=as.factor(mindataset$Maand_Meting)
  mindataset$Jaar_Meting=as.factor(mindataset$Jaar_Meting)
  
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_ByMonth.Rda"))
  
  
}

KNMIconvertYear = function (){
  
  #read in the KNMI set
  load(paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_20150223_v02.Rda"))
  
  count(mindataset[,Plaatsnaam_Weerstation])
  
  #add info to the dataset indicating the month
  
  maand_index=data.table(regexpr("-",mindataset$Datum_Meting))
  mindataset[,Maand_Jaar_Meting:=substr(Datum_Meting,maand_index$V1+1,nchar(Datum_Meting))]  
  jaar_index=data.table(regexpr("-",mindataset$Maand_Jaar_Meting))
  mindataset[,Jaar_Meting:=substr(Maand_Jaar_Meting,jaar_index$V1+1,nchar(Maand_Jaar_Meting))]  
  
  
  #add info indicating whether this is a dry day or not
  mindataset[,Dry_Days:=ifelse(is.na(as.numeric(Etmaalsom_Neerslag)),NA,ifelse(as.numeric(Etmaalsom_Neerslag)>0,0,1))]
  
  #select the different pieces of information per station and month
  meanTemp=mindataset[,mean(as.numeric(Etmaalgemiddelde_Temperatuur)),by=c("Plaatsnaam_Weerstation","Jaar_Meting")]
  sumRain=mindataset[,sum(as.numeric(Etmaalsom_Neerslag)),by=c("Plaatsnaam_Weerstation","Jaar_Meting")]
  dryDays=mindataset[,sum(as.numeric(Dry_Days)),by=c("Plaatsnaam_Weerstation","Jaar_Meting")]
  
  setkey(meanTemp,Plaatsnaam_Weerstation,Jaar_Meting)
  setkey(sumRain,Plaatsnaam_Weerstation,Jaar_Meting)
  setkey(dryDays,Plaatsnaam_Weerstation,Jaar_Meting)
  setkey(mindataset,Plaatsnaam_Weerstation,Jaar_Meting)
  
  unimindataset=unique(mindataset)
  
  CoupledT=unimindataset[meanTemp]
  CoupledTR=CoupledT[sumRain]
  CoupledTRD=CoupledTR[dryDays]
  setnames(CoupledTRD,c("V1","i.V1","i.V1.1"),c("gem_temperatuur","som_neerslag","aantal_drogedagen"))
  
  
  mindataset=CoupledTRD[,c("Coo_X","Coo_Y","Plaatsnaam_Weerstation","Jaar_Meting","gem_temperatuur","som_neerslag","aantal_drogedagen"),with=F]
  
  #set the classes properly
  mindataset$Coo_X=as.numeric(mindataset$Coo_X)
  mindataset$Coo_Y=as.numeric(mindataset$Coo_Y)
  mindataset$Jaar_Meting=as.factor(mindataset$Jaar_Meting)
  
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_ByYear.Rda"))
  
  
}

KNMIconvert20072014 = function (){
  
  #read in the KNMI set
  load(paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_20150223_v02.Rda"))
  
  count(mindataset[,Plaatsnaam_Weerstation])
  
  #add info to the dataset indicating the month
  
  maand_index=data.table(regexpr("-",mindataset$Datum_Meting))
  mindataset[,Maand_Jaar_Meting:=substr(Datum_Meting,maand_index$V1+1,nchar(Datum_Meting))]  
  jaar_index=data.table(regexpr("-",mindataset$Maand_Jaar_Meting))
  mindataset[,Jaar_Meting:=substr(Maand_Jaar_Meting,jaar_index$V1+1,nchar(Maand_Jaar_Meting))]  
  
  
  #add info indicating whether this is a dry day or not
  mindataset[,Dry_Days:=ifelse(is.na(as.numeric(Etmaalsom_Neerslag)),NA,ifelse(as.numeric(Etmaalsom_Neerslag)>0,0,1))]
  
  #select only the rows with year>2007
  mindataset=mindataset[as.numeric(mindataset$Jaar_Meting)>2007,]
  
  #select the different pieces of information per station and month
  meanTemp=mindataset[,mean(as.numeric(Etmaalgemiddelde_Temperatuur),na.rm=T),by=c("Plaatsnaam_Weerstation")]
  sumRain=mindataset[,sum(as.numeric(Etmaalsom_Neerslag),na.rm=T),by=c("Plaatsnaam_Weerstation")]
  dryDays=mindataset[,sum(as.numeric(Dry_Days),na.rm=T),by=c("Plaatsnaam_Weerstation")]
  
  setkey(meanTemp,Plaatsnaam_Weerstation)
  setkey(sumRain,Plaatsnaam_Weerstation)
  setkey(dryDays,Plaatsnaam_Weerstation)
  setkey(mindataset,Plaatsnaam_Weerstation)
  
  unimindataset=unique(mindataset)
  
  CoupledT=unimindataset[meanTemp]
  CoupledTR=CoupledT[sumRain]
  CoupledTRD=CoupledTR[dryDays]
  setnames(CoupledTRD,c("V1","i.V1","i.V1.1"),c("gem_temperatuur","som_neerslag","aantal_drogedagen"))
  
  
  mindataset=CoupledTRD[,c("Coo_X","Coo_Y","Plaatsnaam_Weerstation","gem_temperatuur","som_neerslag","aantal_drogedagen"),with=F]
  
  #set the classes properly
  mindataset$Coo_X=as.numeric(mindataset$Coo_X)
  mindataset$Coo_Y=as.numeric(mindataset$Coo_Y)
  
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_grouped_2007_2014.Rda"))
  
  
}


LineToPointSet = function (){
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

LtoPcleanup = function(DT,setname){
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
  Nstep=2000 #number of steps before adding the current data to the results list, improves speed significantly
  for (i in 1:dim(x)[1] ) 
  {    
    ns <- round( (lgth[i] / sdist), digits=0)
    
    if (ns==0) {ns=1}
    lsamp <- spsample(x[i,], n=ns, type="regular", offset=c(0.5,0.5))
    
    if(i %% 10000==1){
      cat(i," of ",dim(x)[1], ", Number of points in line: ",ns,", ",length(lsamp)[1],"\n")
    }
    
    ns2=length(lsamp)[1]
    lsamp <- SpatialPointsDataFrame(lsamp, data=data.frame(ID=rep(rownames(x@data[i,]),ns2)))     
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

#Written by Michiel Musterd - 23-02-2015
#---------------------------------------
#Function to convert geo data of the mdsys type to SpatialPolygon type for use in geoquery coupling
#This function takes mdsys input of the type: 
#MDSYS.SDO_GEOMETRY(2003,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1),MDSYS.SDO_ORDINATE_ARRAY(231088.1358,570469.305399999
#where the geometric data is available in the ordindate_array in x,y,x,y etc format

#This function was particularly written for the grondsoorten set, but can be fairly easily adapted to a different set

GrondsoortsysGeo_Conversion = function (){
  
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


SpatialPoint_To_XY = function (){
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
  mindataset=mindataset[,list(Coo_X,Coo_Y,TYPEINRICH)]
  mindataset=mindataset[TYPEINRICH!="boom",TYPEINRICH:="overig"]
  setnames(mindataset,c("TYPEINRICH"),c("Type_Inrichting_Punt"))
  
  
  #split the set into a set for bomen and a set for overige inrichtingselementen
  boomset=mindataset[Type_Inrichting_Punt=="boom",list(Coo_X,Coo_Y)]
  overigset=mindataset[Type_Inrichting_Punt=="overig",list(Coo_X,Coo_Y)]
  
  #save both sets 
  save(boomset,file=paste0(filename_no_ext,"_XY_boom.Rda"))
  save(overigset,file=paste0(filename_no_ext,"_XY_overig.Rda"))
  
  
}

