AHA_Proxy_KA_Postprocessing = function(){
load(file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data.Rda"))
load(file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))

akvalidatie=data.table(read.xlsx(paste0(settings$Analyse_Datasets,"/1. Proxy validatie/Proxi_validatie_all.xlsx"),1))
# akvalidatie=data.table(read.xlsx(paste0(settings$Analyse_Datasets,"/Koppeling KLAK-NRG.xlsx"),1))
setkey(akvalidatie,ID_NAN,ID_KLAK_Melding)

merge(akvalidatie,assets$moffen,all.x=TRUE,by="ID_NAN")

# Check with NOR data -----------------
NOR = list()
load(paste0(settings$Ruwe_Datasets,"/6. NOR/ELCVERBINDINGSKNOOPPUNTEN_1407.Rda"))
NOR$moffen =data.table(mindataset) 
NOR$moffen$asset = "moffen"

load(paste0(settings$Ruwe_Datasets,"/6. NOR/ELCVERBINDINGSDELEN_1407.Rda"))
NOR$kabels =data.table(mindataset)
NOR$kabels$asset = "kabels"
setnames(NOR$kabels,c("Coo_X_van","Coo_Y_van","PC_XY_van"),c("Coo_X","Coo_Y","PC_XY"))
rm(mindataset)

kcols = colnames(NOR$kabels) %in% colnames(NOR$moffen)
mcols = colnames(NOR$moffen) %in% colnames(NOR$kabels)
NOR$common =rbind(NOR$moffen[,mcols,with=FALSE],
                  NOR$kabels[,kcols,with=FALSE]) 
NOR$merged = (merge(akvalidatie,NOR$common ,all.x=TRUE,by="ID_NAN"))
NOR$merged[,c("lon.asset","lat.asset")] = AHA_RDCtoGPS(NOR$merged[,c("Coo_X","Coo_Y"),with=FALSE]) 

# Check with KLAK data ----------------
KLAK   = storingen
LScols = colnames(KLAK$LS) %in% colnames(KLAK$MS)
MScols = colnames(KLAK$MS) %in% colnames(KLAK$LS)
KLAK$common = rbind(KLAK$LS[,LScols,with=FALSE],
                    KLAK$MS[,MScols,with=FALSE])
merged = (merge(NOR$merged,KLAK$common  ,all.x=TRUE,by="ID_KLAK_Melding"))
merged$Coo_X.y = as.numeric(sapply(merged$Coo_X.y ,fixnumber))
merged$Coo_Y.y = as.numeric(sapply(merged$Coo_Y.y ,fixnumber))
merged[,c("lon.klak","lat.klak")] = AHA_RDCtoGPS(merged[,c("Coo_X.y","Coo_Y.y"),with=FALSE]) 

# Merge with minimized dataset -------------
merged$inNOR    = merged$ID_NAN %in% assets$moffen$ID_NAN | merged$ID_NAN %in% assets$kabels$ID_NAN
merged$inKLAK   = merged$ID_KLAK_Melding %in% storingen$LS$ID_KLAK_Melding | merged$ID_KLAK_Melding %in% storingen$MS$ID_KLAK_Melding

# Save to file -----------------
write.xlsx(merged,file=paste0(settings$Analyse_Datasets,"/Proxy validatie.xlsx"))
}

fixnumber = function(x) {
  val= strsplit(x,",")[[1]];  
  if (suppressWarnings(!is.na(as.numeric(val[1])))){
    len=length(val); 
    cor=switch(nchar(val[len]),"1"=10,"2"=100,"3"=1000)
    if(len==1) {a=val[1]    } else if(len==2) {
      a=(as.numeric(val[1])+as.numeric(val[2])/cor)
    } else if(len==3) {  a=(as.numeric(val[1])*1000+as.numeric(val[2])+as.numeric(val[3])/cor)
    }  }  else{    a=NA  }
  return(as.numeric(a))
}