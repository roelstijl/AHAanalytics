#### 1. load data and create variables for profiles and standard yearly use

settings=list()

settings$Ruwe_Datasets="D:/1. Alliander Datasets/2. Asset health analytics/1. Ruwe Datasets"

EDSNfilename=paste0(settings$Ruwe_Datasets,"/11. Nettopologie/EDSN2014.csv")
Aansluitingenname=paste0(settings$Ruwe_Datasets,"/11. Nettopologie/EAN_LS_Aansluitingen.Rda")

EDSN   = read.table(EDSNfilename                     , sep = ";", dec=".", colClasses = c(rep("character",3),rep("numeric",10)) ,header = TRUE)
load(Aansluitingenname)
MSR=mindataset

KVbaseprofile = data.matrix(EDSN[,4:dim(EDSN)[2]])
SJV    = as.numeric(sub(",",".",MSR$STANDAARD_JAARVERBRUIK))           #Yearly electricity use in kWh
SJVlow = as.numeric(sub(",",".",MSR$STANDAARD_JAARVERBRUIK_LAAG))      #SJVlow is the SJV during the 'daluren'
SJVlow[is.na(SJVlow)] = 0                                              #Remove missing entries
SJV[is.na(SJV)] = 0                                                    #Remove missing entries
SJV = SJV + SJVlow

#### 2. Get list of unique KV-EANs and HLDs
KVEAN   = unique(MSR$EAN)
HLD     = sort(unique(MSR$HOOFDLEIDING))

#### 3. Create interconnectionmatrix from KVEAN to HLD 
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

KVEANtoHLD  = CreateInterconnectionMatrix(MSR[c("EAN","HOOFDLEIDING")],KVEAN,HLD,FALSE)
nHLD        = dim(KVEANtoHLD)[1]

#### 4. Generate KV baseload profiles
EDSNperEAN       = matrix(0,length(SJV),length(colnames(KVbaseprofile)))     # matrix with nrow = EAN and ncol = nEDSNprofiles
profilenumber    = match(MSR$PROFIEL_TYPE,colnames(KVbaseprofile))           # indexvector to store the correct profiles in the correct places
for (i in 1:length(SJV)) {EDSNperEAN[i,profilenumber[i]]=SJV[i]*4}           # fill matrix with per EDSNperEAN[EAN,profile] = SJV (* 4 to convert kWh to kW)
EDSNperHLD       = t(matprod_simple_triplet_matrix(KVEANtoHLD, EDSNperEAN))  # matrix with nrow = nHLD, ncol = nEDSNprofiles
KVbaseloadperHLD = matrix(NA,nHLD,365*24*4)                                  # This can become quite big depending on the number of HLDs
for (i in 1:nHLD)  {KVbaseloadperHLD[i,] = KVbaseprofile %*% EDSNperHLD[,i]} # matrix multiplication, output = nrow = nHLD, ncol = 1
