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


