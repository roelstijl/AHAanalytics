#Written by Michiel Musterd 17-03-2015


AHA_MVA_CDBreadin=function(){
#This function reads in the CDB file (put in a directory on my C-drive because that is an SSD)
#It subsquently splits the file based on MeetID, where we use a trick to avoid searching 
#of IDs (which is extremely slow): we count how many of each ID are there, then we merge
#this as an extra column in the CDB set and sort on both frequency and MeetID (only MeetID would also work). We then
#loop over the CDB file, using the frequency column to calculate where the current MeetID ends
#and the next begins.


#needs 3 seconds per 1 million lines, so for the ~600 million lines it will need 
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
  
  filename=paste0(settings$Ruwe_Datasets,"/19. CDB/",levels(CDBorderedSet$MeetID)[i],".Rda")
  save(CDBsingleMeetID,file=filename,compress=F)
  
  cat(paste0(i, " done -- runtime (s):", proc.time()[3]-ptm[3],"\n"))

  cat(startID," ",endID," ",as.character(CDBorderedSet[startID,"MeetID"]), " ",as.character(CDBorderedSet[endID,"MeetID"])," ",
      as.character(CDBorderedSet[startID,"freq"])," ",as.character(CDBorderedSet[endID,"freq"]),"\n")
  
  startID=endID+1
}

save(countMeetIDs,file=paste0(settings$Ruwe_Datasets,"/19. CDB/ListOfMeetIDs.Rda"))

}

AHA_MVA_CDBcalcmetrics=function(){
  #This function takes the output of the CDBreadin function and uses it to construct
  #5 metrics for each MeetID: mean(I), I_max, mean(I^2), mean ((dI/dt)^2), max_year(min_2h I) where Delta
  #is the derivative of I at each timestep
  
  #read in ListOfMeetIDs to know which files to load
  load(paste0(settings$Ruwe_Datasets,"/19. CDB/ListOfMeetIDs.Rda"))
  namesSelect=countMeetIDs$MeetID
  
  #now calculate the metrics for each MeetID
  cntr=0
  for (i in namesSelect[10]){
    load(paste0(settings$Ruwe_Datasets,"/19. CDB/",i,".Rda"))
    cntr=cntr+1
    
    
    #calculate the mean I and mean of square I
    Isqmean[cntr]=mean((CDBsingleMeetID$Meetwaarde)^2)
    Imean[cntr]=mean(CDBsingleMeetID$Meetwaarde)
    Imax[cntr]=max(CDBsingleMeetID$Meetwaarde)
    
    #add a time/date field usuable for time computations
    CDBsingleMeetID[,DatumTijd:=paste0(CDBsingleMeetID$Datum," ",CDBsingleMeetID$Tijd)]
    CDBsingleMeetID[,DatumTijdCalc:=data.table(sapply(CDBsingleMeetID$DatumTijd,as.POSIXct))$V1]
    
    #calculate the timestep between every row
    DTend=CDBsingleMeetID$DatumTijdCalc[nrow(CDBsingleMeetID)]+(CDBsingleMeetID$DatumTijdCalc[nrow(CDBsingleMeetID)]-CDBsingleMeetID$DatumTijdCalc[nrow(CDBsingleMeetID)-1])
    DeltaTijd=c(CDBsingleMeetID$DatumTijdCalc[2:nrow(CDBsingleMeetID)],DTend)-CDBsingleMeetID$DatumTijdCalc
    #note Tijd is in seconds
    
    #calculate Delta I for all   
    DIend=CDBsingleMeetID$Meetwaarde[nrow(CDBsingleMeetID)]+(CDBsingleMeetID$Meetwaarde[nrow(CDBsingleMeetID)]-CDBsingleMeetID$Meetwaarde[nrow(CDBsingleMeetID)-1])
    DeltaI=c(CDBsingleMeetID$Meetwaarde[2:nrow(CDBsingleMeetID)],DIend)-CDBsingleMeetID$Meetwaarde
    
    #Now we can calculate the derivatives and the derivative of I squared
    dIdTsquared[cntr]=mean((DeltaI/DeltaTijd)^2)
    
    #Finally we compute the minimum per 2h (7200 s) frame and then the max of that for the year
    min2h=1:nrow(CDBsingleMeetID)
    if (mean(DeltaTijd)==300){
      for (j in 1:nrow(CDBsingleMeetID)){
        if ((j+24)<nrow(CDBsingleMeetID)){
          min2h[j]=min(CDBsingleMeetID$Meetwaarde[j:(j+24)])
        }else{
          cat(j,"\n")
          min2h[j]=min(CDBsingleMeetID$Meetwaarde[(j-24):j]) 
        }
      }      
    }
    
    
  }

}


