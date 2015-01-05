AHA_Data_KA_Proxy_Preprocessing = function(datasets=c("assetsBAR","assetsNOR","nettopo","storingen","validatieset"),
                                           firstdate="2014-01-01",lastdate="2015-01-01"){
# Merges the Delta data into small datasets for analysis
# Datasets selects only a certain set to process (default = all)
# firstdate is the startdate of the data subset
# last date is the last date to take
#
# Settings ----------------------------------------------------------------  
cat("Starting\n")
pb = tkProgressBar(title = paste0("AHA_Data_KA_Proxy_Preprocessing, ",as.character(Sys.time())), label = "Start", min = 0, max = 6, initial = 0, width = 450);

firstdate = as.Date("2014-01-01")
lastdate  = as.Date("2015-01-01")

# switch between datasets
setTkProgressBar(pb, 1,label = "Loading verbindingen");
for (m in datasets)
  {switch (m,
   assetsBAR = {        
# BAR Data ----------------------------------------------------------------
setTkProgressBar(pb, 1,label = "Load BAR data\n"); ;
# Laad de assets en converteer de datums als deze verkeerd staan 
load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_BAR_assets.Rda"))

# Laad alleen dat deel van de assets dat binnen de periode valt
assets$LSmoffen = assets$LSmoffen[(assets$LSmoffen$DateAdded > firstdate & assets$LSmoffen$DateAdded < lastdate)| 
                                   (assets$LSmoffen$DateRemoved > firstdate & assets$LSmoffen$DateRemoved < lastdate)]
assets$MSmoffen = assets$MSmoffen[(assets$MSmoffen$DateAdded > firstdate & assets$MSmoffen$DateAdded < lastdate)| 
                                   (assets$MSmoffen$DateRemoved > firstdate & assets$MSmoffen$DateRemoved < lastdate)]

assets$Mkabels = assets$Mkabels[(assets$Mkabels$DateAdded > firstdate & assets$Mkabels$DateAdded < lastdate)| 
                                 (assets$Mkabels$DateLength_ch > firstdate & assets$Mkabels$DateLength_ch < lastdate)|  
                                 (assets$Mkabels$DateRemoved > firstdate & assets$Mkabels$DateRemoved < lastdate)]
assets$Lkabels = assets$Lkabels[(assets$Lkabels$DateAdded > firstdate & assets$Lkabels$DateAdded < lastdate)| 
                                 (assets$Lkabels$DateLength_ch > firstdate & assets$Lkabels$DateLength_ch < lastdate)|  
                                 (assets$Lkabels$DateRemoved > firstdate & assets$Lkabels$DateRemoved < lastdate)]
assets$MSmoffen = assets$MSmoffen[DateAdded>min(DateAdded)]
assets$LSmoffen = assets$LSmoffen[DateAdded>min(DateAdded)]

assets$MSkabels = assets$MSkabels[DateAdded>min(DateAdded)]
assets$LSkabels = assets$LSkabels[DateAdded>min(DateAdded)]

# Bereken postcode 4
assets$LSmoffen[,PC_4:=substr(assets$LSmoffen$PC_6,1,4)]
assets$LSkabels[,PC_4_van:=substr(assets$LSkabels$PC_6_van,1,4)]
assets$LSkabels[,PC_4_naar:=substr(assets$LSkabels$PC_6_naar,1,4)]  
assets$MSmoffen[,PC_4:=substr(assets$MSmoffen$PC_6,1,4)]
assets$MSkabels[,PC_4_van:=substr(assets$MSkabels$PC_6_van,1,4)]
assets$MSkabels[,PC_4_naar:=substr(assets$MSkabels$PC_6_naar,1,4)]  

# Opsplitsen in MS en LS, zo zit het in de BARlog ook
toc;  setTkProgressBar(pb, 2,label = "Save BAR asset data\n"); ;
save(assets,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_BAR.Rda"))
remove("assets")
     
   },          
assetsNOR = {        
# NOR Data ----------------------------------------------------------------
   setTkProgressBar(pb, 1,label = "Load NOR data\n"); ;
  # Laad de assets en converteer de datums als deze verkeerd staan 
   load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))
   assets$kabels[,DateLength_ch := as.Date(assets$kabel$DateLength_ch,"1970-01-01")]

  # Laad alleen dat deel van de assets dat binnen de periode valt
  assets$moffen = assets$moffen[(assets$moffen$DateAdded > firstdate & assets$moffen$DateAdded < lastdate)| 
                                (assets$moffen$DateRemoved > firstdate & assets$moffen$DateRemoved < lastdate)]
  assets$moffen = assets$moffen[DateAdded>min(DateAdded)]

  try(setnames(assets$moffen,"PC_XY","PC_6"))
  assets$kabels = assets$kabels[(assets$kabels$DateAdded > firstdate & assets$kabels$DateAdded < lastdate)| 
                                (assets$kabels$DateLength_ch > firstdate & assets$kabels$DateLength_ch < lastdate)|  
                                (assets$kabels$DateRemoved > firstdate & assets$kabels$DateRemoved < lastdate)]
  assets$kabels = assets$kabels[DateAdded>min(DateAdded)]
                                
  try(setnames(assets$kabels,c("PC_XY_van","PC_XY_naar"),c("PC_6_van","PC_6_naar")))
  try(setnames(assets$kabels,c("Coo_X","Coo_Y"),c("Coo_X_naar","Coo_Y_naar")))
   
  # Bereken postcode 4
  assets$moffen[,PC_4:=substr(assets$moffen$PC_6,1,4)]
  assets$kabels[,PC_4_van:=substr(assets$kabels$PC_6_van,1,4)]
  assets$kabels[,PC_4_naar:=substr(assets$kabels$PC_6_naar,1,4)]  
  
  # Opsplitsen in MS en LS, zo zit het in de BARlog ook
  try(setnames(assets$kabels,"BRONTABEL","Brontabel"))

#   assets$LSkabels = assets$kabels[Brontabel == "ls_kabels"]
#   assets$MSkabels = assets$kabels[Brontabel == "ms_kabels"]

# temporary FIX - needs new export!
  assets$LSkabels = assets$kabels[Netvlak == "LS"]
  assets$MSkabels = assets$kabels[Netvlak == "MS"]
  assets$LSmoffen = assets$moffen[Brontabel == "ls_moffen"]
  assets$MSmoffen = assets$moffen[Brontabel == "ms_moffen"]
  assets$moffen = NULL
  assets$kabels = NULL

toc;  setTkProgressBar(pb, 2,label = "Save asset data\n"); ;
  save(assets,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_NOR.Rda"))
  remove("assets")
  
},
nettopo = {
# EAN-Hoofdleiding-XY-PC data ----------------
nettopo = list();  

;  setTkProgressBar(pb, 3,label = "Load EAN data\n"); ;
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

validatie = {
# Validatieset van de storingsregistratoren-----------------------------------
load(paste0(settings$Ruwe_Datasets,"/23. Validatie_data/Koppeling KLAK-NRG Dick Grollers..Rda"))
ValidatieSet = mindataset[,Regio := "Zuid-Oost"]

load(paste0(settings$Ruwe_Datasets,"/23. Validatie_data/Koppeling KLAK-NRG Robert Aikema..Rda"))
ValidatieSet = rbind(ValidatieSet, mindataset[,Regio := "Amsterdam"])

try(setnames(ValidatieSet,"ID_KLAK","ID_KLAK_Melding"))
ValidatieSet[,ID_Asset:=NULL]; 
ValidatieSet = ValidatieSet[!is.na(ValidatieSet$ID_KLAK_Melding)]
ValidatieSet$ID_KLAK_Melding = as.character(ValidatieSet$ID_KLAK_Melding)
save(ValidatieSet,file=paste0(settings$Input_Datasets,"/23. Validatie_data/Validatie koppelingen.Rda"))

},

storingen = {
  
# Storingsdata uit KLAK ------------------------
storingen=list()  

;  setTkProgressBar(pb, 4,label = "Load KLAK data\n"); ;
# Laad de data om adressen te koppelen
load(paste0(settings$Ruwe_Datasets,"/8. CAR/CAR_2013_XY.Rda"))
EAN_to_XY_PC6 = (mindataset[,list(PC_6,Huisnr,Coo_X,Coo_Y,ID_EAN)])
EAN_to_XY_PC6[,PC_4:=substr(EAN_to_XY_PC6$PC_6,1,4)]
setkey(EAN_to_XY_PC6,ID_EAN)

# Laad LS
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_LS.Rda"))
mindataset[,Datum:=as.Date(mindataset$Datum)]
storingen$LS= mindataset[(mindataset$Datum > firstdate & mindataset$Datum < lastdate)]
storingen$LS[,PC_6 := gsub(" ","",storingen$LS$PC_6)] 

# Laad MS
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_MS.Rda"))
mindataset[,Datum:=as.Date(mindataset$Datum)]
storingen$MS= mindataset[(mindataset$Datum > firstdate & mindataset$Datum < lastdate)]
storingen$MS[,PC_6 := gsub(" ","",storingen$MS$PC_6)] 

# Laad meldingen
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_KOPPEL_MELDING_GROEP.Rda"))
storingen$KLAKMelders = data.table(mindataset);
storingen$KLAKMelders[,PC_6 := gsub(" ","",storingen$KLAKMelders$PC_6)] 
storingen$KLAKMelders[,PC_4:=substr(storingen$KLAKMelders$PC_6,1,4)]
load(paste0(settings$Ruwe_Datasets,"/24. Adressendichtheid/Count Adresses.Rda"))
setkey(storingen$KLAKMelders,PC_4)
setkey(mindataset,PC_4)
storingen$KLAKMelders  = mindataset[storingen$KLAKMelders ]

load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_4_Spatial.Rda"))
pc4area = data.table(pc4@data[,c("PC4CODE","SHAPE_AREA")]);
setnames(pc4area,c("PC_4","Oppervlakte_PC4"))
setkey(pc4area,PC_4);
storingen$KLAKMelders  = pc4area[storingen$KLAKMelders ]
setkey(storingen$KLAKMelders ,PC_6,Huisnr); 
setkey(EAN_to_XY_PC6,PC_6,Huisnr)
storingen$KLAKMelders        = unique(EAN_to_XY_PC6)[storingen$KLAKMelders]
storingen$KLAKMelders[,PC_4:=NULL]; try(setnames(storingen$KLAKMelders,"i.PC_4","PC_4"))

# Voeg informatie uit de melders toe
frequ = data.table(data.frame(table(storingen$KLAKMelders$ID_Groep)))
try(setnames(frequ,c("ID_Groep","Aantal_Melders")))
setkey(frequ,ID_Groep);  
setkey(storingen$KLAKMelders,ID_Groep);
ugroep         = frequ[storingen$KLAKMelders[ST_Groep_eerste=="Ja"]]
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

setTkProgressBar(pb, 5,label = "Save KLAK data\n"); ;
save(storingen,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))

})}
setTkProgressBar(pb, 6,label = "Done")

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