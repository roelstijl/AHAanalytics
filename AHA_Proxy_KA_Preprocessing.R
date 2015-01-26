AHA_Data_KA_Proxy_Preprocessing = function(datasets=c("assetsBAR","assetsNOR","nettopo","storingen","validatieset"),
                           firstdate="2014-01-01",lastdate="2015-01-01"){
# Merges the Delta data into small datasets for analysis
# Datasets selects only a certain set to process (default = all)
# firstdate is the startdate of the data subset
# last date is the last date to take
#
# Settings ----------------------------------------------------------------  
cat("Starting\n")
pb = pbarwrapper(title = paste0("AHA_Data_KA_Proxy_Preprocessing: ",as.character(Sys.time())), 
                 label = "Start", min = 0, max = 3*length(datasets)+1, initial = 0, width = 450);

firstdate = as.Date("2014-02-16")
lastdate  = as.Date("2015-01-01")

# switch between datasets
for (m in datasets)
{switch (m,
assetsBAR = {        
# BAR Data ----------------------------------------------------------------
setpbarwrapper(pb, label = "Loading BAR data");
# Laad de assets en converteer de datums als deze verkeerd staan 
load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_BAR_assets.Rda"))
assets$LSkabel[,DateAdded:=as.Date(DateAdded)]
assets$LSkabel[,DateRemoved:=as.Date(DateRemoved)]
assets$LSkabel[,DateLength_ch:=as.Date(DateLength_ch)]

setpbarwrapper(pb, label = "Calculating BAR data");
# Laad alleen dat deel van de assets dat binnen de periode valt
# Moffen
MSm=(assets$MSmoffen$DateAdded > firstdate & assets$MSmoffen$DateAdded < lastdate)| 
                  (assets$MSmoffen$DateRemoved > firstdate & assets$MSmoffen$DateRemoved < lastdate)
MSm[is.na(MSm)] = FALSE
assets$MSmoffen = assets$MSmoffen[MSm]

LSm = (assets$LSmoffen$DateAdded > firstdate & assets$LSmoffen$DateAdded < lastdate)| 
                  (assets$LSmoffen$DateRemoved > firstdate & assets$LSmoffen$DateRemoved < lastdate)
LSm[is.na(LSm)] = FALSE
assets$LSmoffen = assets$LSmoffen[LSm]
  
# Kabels
MSk=(assets$MSkabels$DateAdded > firstdate & assets$MSkabels$DateAdded < lastdate)| 
                  (assets$MSkabels$DateLength_ch > firstdate & assets$MSkabels$DateLength_ch < lastdate)|  
                  (assets$MSkabels$DateRemoved > firstdate & assets$MSkabels$DateRemoved < lastdate)
MSk[is.na(MSk)] = FALSE
assets$MSkabels = assets$MSkabels[MSk]
  
LSk=(assets$LSkabels$DateAdded > firstdate & assets$LSkabels$DateAdded < lastdate)| 
  (assets$LSkabels$DateLength_ch > firstdate & assets$LSkabels$DateLength_ch < lastdate)|  
  (assets$LSkabels$DateRemoved > firstdate & assets$LSkabels$DateRemoved < lastdate)
LSk[is.na(LSk)]=FALSE
assets$LSkabels = assets$LSkabels[LSk]

assets$MSmoffen[,PC_4:=substr(assets$MSmoffen$PC_6,1,4)]
assets$LSmoffen[,PC_4:=substr(assets$LSmoffen$PC_6,1,4)]

assets$LSkabels[,PC_4_van:=substr(assets$LSkabels$PC_6_van,1,4)]
assets$LSkabels[,PC_4_naar:=substr(assets$LSkabels$PC_6_naar,1,4)]  
assets$MSkabels[,PC_4_van:=substr(assets$MSkabels$PC_6_van,1,4)]
assets$MSkabels[,PC_4_naar:=substr(assets$MSkabels$PC_6_naar,1,4)]  

assets$MSHLDROUTE = NULL

# Opsplitsen in MS en LS, zo zit het in de BARlog ook
setpbarwrapper(pb, label = "Saving BAR data");
save(assets,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_BAR.Rda"))
remove("assets")

},          
assetsNOR = {        
  
# NOR Data ----------------------------------------------------------------
setpbarwrapper(pb, label = "Loading NOR data");
# Laad de assets en converteer de datums als deze verkeerd staan 
load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))
assets$kabels[,DateLength_ch := as.Date(assets$kabel$DateLength_ch,"1970-01-01")]

LSm = (assets$moffen$DateAdded > firstdate & assets$moffen$DateAdded < lastdate)| 
  (assets$moffen$DateRemoved > firstdate & assets$moffen$DateRemoved < lastdate)
LSm[is.na(LSm)] = FALSE
assets$moffen = assets$moffen[LSm]

setpbarwrapper(pb, label = "Calculating NOR data");

# Kabels
MSk=(assets$kabels$DateAdded > firstdate & assets$kabels$DateAdded < lastdate)| 
  (assets$kabels$DateLength_ch > firstdate & assets$kabels$DateLength_ch < lastdate)|  
  (assets$kabels$DateRemoved > firstdate & assets$kabels$DateRemoved < lastdate)
MSk[is.na(MSk)]=FALSE
assets$kabels = assets$kabels[MSk]

# Bereken postcode 4

assets$moffen[,PC_4:=substr(assets$moffen$PC_6,1,4)]
assets$kabels[,PC_4_van:=substr(assets$kabels$PC_6_van,1,4)]
assets$kabels[,PC_4_naar:=substr(assets$kabels$PC_6_naar,1,4)]  

assets$LSkabels = assets$kabels[Brontabel == "ls_kabels"]
assets$MSkabels = assets$kabels[Brontabel == "ms_kabels"]
assets$LSmoffen = assets$moffen[Brontabel == "ls_moffen"]
assets$MSmoffen = assets$moffen[Brontabel == "ms_moffen"]

assets$moffen = NULL
assets$kabels = NULL

setpbarwrapper(pb, label = "Saving NOR data");
save(assets,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_NOR.Rda"))
remove("assets")

},
nettopo = {
  
# EAN-Hoofdleiding-XY-PC data ----------------
nettopo = list();  

# Load the data
setpbarwrapper(pb, label = "Loading Nettopo data");
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/aansluitingen_stationinclbehuizing.Rda")); 
Hoofdleiding_Station = mindataset
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/aansluitingengeotrace.Rda")); 
Hoofdleiding_Station2 = mindataset
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/EAN_LS_Aansluitingen_XY.Rda")); 
EAN_Aansluitingen = mindataset
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_Stations.Rda")); 
MS_Stations = mindataset
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_hoofdleidingen.Rda")); 
MS_Hoofdleidingen = mindataset
load(paste0(settings$Ruwe_Datasets,"/24. Adressendichtheid/Count Adresses.Rda"))
adressendichtheid = mindataset
load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_4_Spatial.Rda"))
pc4area = data.table(pc4@data[,c("PC4CODE","SHAPE_AREA")]);
rm("mindataset")

# Start calculating the nettopology
setkey(Hoofdleiding_Station,ID_EAN)
setkey(Hoofdleiding_Station2,ID_EAN)
setkey(EAN_Aansluitingen,ID_EAN)

setpbarwrapper(pb, label = "Calculating Nettopo data");
nettopo = unique(Hoofdleiding_Station)[unique(EAN_Aansluitingen),
                                         list(ID_EAN,ID_Hoofdleiding,ID_Station,ID_Kabel,Naam_Ruimte,ID_NR_Ruimte,
                                              ID_Stationbehuizing,Coo_X,Coo_Y,Adres)]

set(nettopo,which(is.na(nettopo$ID_Hoofdleiding)),j=1:ncol(nettopo),
    unique(Hoofdleiding_Station2)[unique(
      EAN_Aansluitingen)[is.na(nettopo$ID_Hoofdleiding)],
      list(ID_EAN,ID_Hoofdleiding,ID_Station,ID_Kabel,Naam_Ruimte,ID_NR_Ruimte,
           ID_Stationbehuizing,Coo_X,Coo_Y,Adres)])

# Add the MS routes
setkey(nettopo,ID_Stationbehuizing)
setkey(MS_Stations,ID_Stationbehuizing)

nettopo = unique(MS_Stations[,list(ID_Stationbehuizing,ID_NR_Ruimte,ID_Voedend_Station,Routenaam)])[nettopo]
nettopo[,PC_6:=substr(Adres, 1, 6)]
nettopo[,PC_4:=substr(Adres, 1, 4)]
nettopo[,Huisnr:=adrsplit(Adres)]


# Add adress density figures
setkey(nettopo,PC_4)
setkey(adressendichtheid,PC_4)
setnames(pc4area,c("PC_4","Oppervlakte_PC4"))
setkey(pc4area,PC_4);

nettopo = adressendichtheid[nettopo]
nettopo = pc4area[nettopo]

# Save
setpbarwrapper(pb, label = "Saving Nettopo data");
save(nettopo,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo_EAN.Rda"))
},

validatie = {
  
# Validatieset van de storingsregistratoren-----------------------------------
setpbarwrapper(pb, label = "Loading Validatie data");

load(paste0(settings$Ruwe_Datasets,"/23. Validatie_data/Koppeling KLAK-NRG Dick Grollers..Rda"))
ValidatieSet = mindataset[,Regio := "Zuid-Oost"]

load(paste0(settings$Ruwe_Datasets,"/23. Validatie_data/Koppeling KLAK-NRG Robert Aikema..Rda"))
ValidatieSet = rbind(ValidatieSet, mindataset[,Regio := "Amsterdam"])
setpbarwrapper(pb, label = "Loading Validatie data");

try(setnames(ValidatieSet,"ID_KLAK","ID_KLAK_Melding"))
ValidatieSet[,ID_Asset:=NULL]; 
ValidatieSet = ValidatieSet[!is.na(ValidatieSet$ID_KLAK_Melding)]
ValidatieSet$ID_KLAK_Melding = as.character(ValidatieSet$ID_KLAK_Melding)
setpbarwrapper(pb, label = "Saving Validatie data");

save(ValidatieSet,file=paste0(settings$Input_Datasets,"/23. Validatie_data/Validatie koppelingen.Rda"))

},

storingen = {

# Storingsdata uit KLAK ------------------------
storingen=list()  
setpbarwrapper(pb, label = "Loading KLAK data");

# Laad de data om adressen te koppelen
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo_EAN.Rda"))
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_LS.Rda"))
mindataset[,Datum:=as.Date(mindataset$Datum)]
storingen$LS= mindataset[(mindataset$Datum > firstdate & mindataset$Datum < lastdate)]
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_MS.Rda"))
mindataset[,Datum:=as.Date(mindataset$Datum)]
storingen$MS= mindataset[(mindataset$Datum > firstdate & mindataset$Datum < lastdate)]
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_KOPPEL_MELDING_GROEP.Rda"))
storingen$KLAKMelders = data.table(mindataset);

setpbarwrapper(pb, label = "Process KLAK data");

# Add and correct Postcodes
storingen$MS[,PC_6 := gsub(" ","",storingen$MS$PC_6)] 
storingen$LS[,PC_6 := gsub(" ","",storingen$LS$PC_6)] 
storingen$KLAKMelders[,PC_6 := gsub(" ","",storingen$KLAKMelders$PC_6)] 
storingen$KLAKMelders[,PC_4:=substr(storingen$KLAKMelders$PC_6,1,4)]

# Add the nettopology
setkey(storingen$KLAKMelders ,PC_6,Huisnr); 
setkey(storingen$LS ,PC_6,Huisnr); 
setkey(storingen$MS ,PC_6,Huisnr); 
setkey(nettopo,PC_6,Huisnr)
storingen$KLAKMelders = unique(nettopo)[storingen$KLAKMelders]
storingen$LS = unique(nettopo)[storingen$LS]
storingen$MS = unique(nettopo)[storingen$MS]
storingen$KLAKMelders$Melders = "Melder"

# Verwijder de foutieve KALK meldingen, want alleen eerste melders tellen
setnames(storingen$KLAKMelders,"ID_KLAK_Melding","ID_KLAK_Melding_oud")
setkey(storingen$KLAKMelders,ID_Groep)
temp = unique(storingen$KLAKMelders[ID_Groep!="" & !is.na(ID_Groep) & ST_Groep_eerste=="Ja",
                                    list(ID_Groep,ID_KLAK_Melding_oud)])
setnames(temp,"ID_KLAK_Melding_oud","ID_KLAK_Melding")
setkey(temp,ID_Groep)

# Bepaal het aantal melders

storingen$KLAKMelders=temp[storingen$KLAKMelders]
storingen$KLAKMelders[!(ID_Groep!="" & !is.na(ID_Groep)),ID_KLAK_Melding:=ID_KLAK_Melding_oud]
temp= data.table(Aantal_Melders=data.table(table(storingen$KLAKMelders$ID_KLAK_Melding)),ID_KLAK_Melding=unique(storingen$KLAKMelders$ID_KLAK_Melding))

setkey(temp,ID_KLAK_Melding)
setkey(storingen$KLAKMelders,ID_KLAK_Melding);
setnames(temp,"Aantal_Melders.V1","Aantal_Melders")
storingen$KLAKMelders = temp[storingen$KLAKMelders]

# Koppel meldingen en storingen
setkey(storingen$LS,ID_KLAK_Melding); 
setkey(storingen$MS,ID_KLAK_Melding); 
setkey(storingen$KLAKMelders,ID_KLAK_Melding)
storingen$LS=unique(storingen$KLAKMelders)[storingen$LS]
storingen$MS=unique(storingen$KLAKMelders)[storingen$MS]

# Koppel de GIS mutaties
load(paste0(settings$Ruwe_Datasets,"/21. GIS-mutaties/GISMUTATIE.Rda"))
setkey(mindataset,ID_KLAK_Melding)
storingen$LS=mindataset[storingen$LS]
storingen$MS=mindataset[storingen$MS]
 
setpbarwrapper(pb, label = "Loading Melding data");
save(storingen,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))
 
})}
setpbarwrapper(pb, label = "Done");
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

adrsplit = function (Adres){
  
  as.integer(laply(strsplit(Adres," "),function(x) ifelse(length(x)>1,x[[2]],NA)))
}

firstFri = function(initialdate)
{
  #   Aproximate date of NOR generation, first friday + 2 days
  date = as.Date(paste0(initialdate,"01"), "%y%m%d")
  dow = sapply(seq(0,6),function(x) wday(date+days(x)))
  firstFriday = date + days(which(dow==5)-1)+2
  return(firstFriday)
}
