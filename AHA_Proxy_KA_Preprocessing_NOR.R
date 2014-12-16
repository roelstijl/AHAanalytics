AHA_Data_KA_Proxy_Preprocessing_NOR = function(datasets=c("storingen","assets","nettopo"),
                                           firstdate="2014-01-01",lastdate="2015-01-01")
  {
  # Loads several months of AHA datasets and puts this into a dataset

# Settings ----------------------------------------------------------------  
firstdate = as.Date("2014-01-01")
lastdate  = as.Date("2015-01-01")

# switch between datasets
for (m in datasets)
  {switch (m,
          
assets = {        
# NOR Data ----------------------------------------------------------------
  cat("Load NOR data\n"); tic();
  # Laad de assets en converteer de datums als deze verkeerd staan 
   load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))
   assets$kabels[,DateLength_ch := as.Date(assets$kabel$DateLength_ch,"1970-01-01")]

  # Laad alleen dat deel van de assets dat binnen de periode valt
  assets$moffen = assets$moffen[(assets$moffen$DateAdded > firstdate & assets$moffen$DateAdded < lastdate)| 
                                (assets$moffen$DateRemoved > firstdate & assets$moffen$DateRemoved < lastdate)]
  try(setnames(assets$moffen,"PC_XY","PC_6"))
  assets$kabels = assets$kabels[(assets$kabels$DateAdded > firstdate & assets$kabels$DateAdded < lastdate)| 
                                (assets$kabels$DateLength_ch > firstdate & assets$kabels$DateLength_ch < lastdate)|  
                                (assets$kabels$DateRemoved > firstdate & assets$kabels$DateRemoved < lastdate)]
                                
  try(setnames(assets$kabels,c("PC_XY_van","PC_XY_naar"),c("PC_6_van","PC_6_naar")))
  try(setnames(assets$kabels,c("Coo_X","Coo_Y"),c("Coo_X_naar","Coo_Y_naar")))
  
  # Voeg routenamen toe aan de MS kabels
#   load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/nettopo_MSHLD_MSRing.Rda"))
#   setkey(nettopo_MSRing_hld,"ID_Hoofdleiding")
#   nettopo_MSRing_hld = unique(data.table(nettopo_MSRing_hld))
#   try(setnames(nettopo_MSRing_hld,"Routenaam","Routenaam_MS"))
#   try(setnames(nettopo_MSRing_hld,"Nummer","ID_Hoofdleiding"))
#   assets$kabels<-merge(assets$kabels,nettopo_MSRing_hld[,c("ID_Hoofdleiding","Routenaam_MS"),with=FALSE],all.x=TRUE)
#   
  # Bereken postcode 4
  assets$moffen[,PC_4:=substr(assets$moffen$PC_6,1,4)]
  assets$kabels[,PC_4_van:=substr(assets$kabels$PC_6_van,1,4)]
  assets$kabels[,PC_4_naar:=substr(assets$kabels$PC_6_naar,1,4)]  
  
  # Opsplitsen in MS en LS, zo zit het in de BARlog ook
  try(setnames(assets$kabels,"BRONTABEL","Brontabel"))

  assets$LSkabels = assets$kabels[Brontabel == "ls_kabels"]
  assets$MSkabels = assets$kabels[Brontabel == "ms_kabels"]
  assets$LSmoffen = assets$moffen[Brontabel == "ls_moffen"]
  assets$MSmoffen = assets$moffen[Brontabel == "ms_moffen"]
  assets$moffen = NULL
  assets$kabels = NULL

toc; cat("Save asset data\n"); tic();
  save(assets,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets.Rda"))
  remove("assets")
  
},
nettopo = {
# EAN-Hoofdleiding-XY-PC data ----------------
nettopo = list();  

toc(); cat("Load EAN data\n"); tic();
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/aansluitingen_stationinclbehuizing.Rda"))
aansluitingen1 = data.table(mindataset)
setkey(aansluitingen1,ID_EAN)

load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/aansluitingengeotrace.Rda"))
aansluitingen2 = data.table(mindataset)
setkey(aansluitingen2,ID_EAN)
tablecount<-data.frame(table(aansluitingen1$ID_EAN))
try(setnames(tablecount,"Var1","ID_EAN"))
data3=merge(aansluitingen1[-which(aansluitingen1$ID_EAN==""),], tablecount, by="ID_EAN")
data3=data3[which(data3$Freq==1)]
pm = pmatch(colnames(aansluitingen1),colnames(aansluitingen2))
aansluitingen2 = aansluitingen2[,pm,with=FALSE]

load(paste0(settings$Ruwe_Datasets,"/8. CAR/CAR_2013_XY.Rda"))
mindataset$ID_EAN= as.character(mindataset$ID_EAN)
EAN_to_XY_PC6 = (mindataset[,c("PC_6","Huisnr","Coo_X","Coo_Y","ID_EAN"),with=FALSE])
EAN_to_XY_PC6$PC_4=substr(EAN_to_XY_PC6$PC_6,1,4)
setkey(EAN_to_XY_PC6,ID_EAN)
nettopo$EAN_koppel<-merge(rbind(aansluitingen2,data3[,1:9,with=FALSE]),EAN_to_XY_PC6,by="ID_EAN",all.x=TRUE)

load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/nettopo_EAN_MSRING.Rda"))
nettopo_new = unique(data.table(nettopo_new),by=c("ID_EAN"))
try(setnames(nettopo_new,"Routenaam","Routenaam_MS"))
nettopo$EAN_koppel<-merge(nettopo$EAN_koppel,nettopo_new[,c("ID_EAN","Routenaam_MS"),with=FALSE],by="ID_EAN",all.x=TRUE)

save(nettopo,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo.Rda"))
},
storingen = {
  
# Storingsdata uit KLAK ------------------------
storingen=list()  

toc(); cat("Load KLAK data\n"); tic();
# Laad de data om adressen te koppelen
load("C:/Datasets/AHAdata/1. Ruwe Datasets/8. CAR/CAR_2013_XY.Rda")
EAN_to_XY_PC6 = (mindataset[,list(PC_6,Huisnr,Coo_X,Coo_Y,ID_EAN)])
EAN_to_XY_PC6[,PC_4:=substr(EAN_to_XY_PC6$PC_6,1,4)]
setkey(EAN_to_XY_PC6,ID_EAN)

# Laad LS
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_LS.Rda"))
mindataset[,Datum:=as.Date(mindataset$Datum)]
storingen$LS= mindataset[(mindataset$Datum > firstdate & mindataset$Datum < lastdate)]

# Laad MS
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_MS.Rda"))
mindataset[,Datum:=as.Date(mindataset$Datum)]
storingen$MS= mindataset[(mindataset$Datum > firstdate & mindataset$Datum < lastdate)]

# Laad compensatie
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_COMPENSATIE.Rda"))  
storingen$Compensatie = data.table(mindataset);

# Laad meldingen
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_KOPPEL_MELDING_GROEP.Rda"))
storingen$KLAKMELDERS = data.table(mindataset);
frequ          = data.table(data.frame(table(storingen$KLAKMELDERS$ID_Groep)))
try(setnames(frequ,c("ID_Groep","Aantal_Melders")))
setkey(frequ,ID_Groep);  
setkey(storingen$KLAKMELDERS,ID_Groep);
ugroep         = frequ[storingen$KLAKMELDERS[ST_Groep_eerste=="Ja"]]
setkey(ugroep,PC_6,Huisnr); 
setkey(EAN_to_XY_PC6,PC_6,Huisnr)
ugroep         = unique(EAN_to_XY_PC6)[ugroep]
ugroep[,PC_4:=substr(ugroep$PC_6,1,4)]
  
# Koppel meldingen en storingen
setkey(storingen$LS,ID_KLAK_Melding); 
setkey(storingen$MS,ID_KLAK_Melding); 
setkey(ugroep,ID_KLAK_Melding)
storingen$LS=ugroep[storingen$LS]
storingen$MS=ugroep[storingen$MS]
  
# Koppel de GIS mutaties
load(paste0(settings$Ruwe_Datasets,"/21. GIS-mutaties/GISMUTATIE.Rda"))
setkey(mindataset,ID_KLAK_Melding)
storingen$LS=mindataset[storingen$LS]
storingen$MS=mindataset[storingen$MS]

toc; cat("Save KLAK data\n"); tic();
save(storingen,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))

})}

}

fixnumber = function(x) {
  val= strsplit(x,",")[[1]];
  
  if (suppressWarnings(!is.na(as.numeric(val[1])))){
    len=length(val); 
    cor=switch(nchar(val[len]),"1"=10,"2"=100,"3"=1000)
    if(len==1) {a=val[1]
    } else if(len==2) {
      a=(as.numeric(val[1])+as.numeric(val[2])/cor)
    } else if(len==3) {
      a=(as.numeric(val[1])*1000+as.numeric(val[2])+as.numeric(val[3])/cor)
    }
  }
  else{
    a=NA
  }
  #cat(paste0(a,", "))
  return(as.numeric(a))
}

firstFri = function(initialdate)
{
  #   Aproximate date of NOR generation, first friday + 2 days
  date = as.Date(paste0(initialdate,"01"), "%y%m%d")
  dow = sapply(seq(0,6),function(x) wday(date+days(x)))
  firstFriday = date + days(which(dow==5)-1)+2
  return(firstFriday)
}