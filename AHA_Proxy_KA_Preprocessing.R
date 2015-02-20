AHA_Data_KA_Proxy_Preprocessing = function(datasets=c("assetsBAR","assetsNOR","nettopo","storingen","validatieset"),
                           firstdate="2014-02-16",lastdate="2015-01-01"){
# Merges the Delta data into small datasets for analysis
# Datasets selects only a certain set to process (default = all)
# cfg$firstdate is the startdate of the data subset
# last date is the last date to take
#
# Settings  
cat("Starting\n")
cfg = list()
cfg$pb  = pbarwrapper(title = paste0("AHA_Data_KA_Proxy_Preprocessing: ",as.character(Sys.time())), 
                 label = "Start", min = 0, max = 3*length(datasets)+1, initial = 0, width = 450);

cfg$firstdate = as.Date("2007-02-16")
cfg$lastdate  = as.Date("2015-01-01")

# Set what dates unexplainable bumps in the data occured
cfg$BadDates = list()
cfg$BadDates$kabels = as.Date(c("2007-01-06","2010-07-03",'2011-02-05', "2011-04-06","2012-05-05"))
cfg$BadDates$moffen = as.Date(c("2007-01-06","2010-07-03",'2011-02-05', "2011-04-06","2012-05-05"))
cfg$BadDates$Verbindingen = as.Date(c("2007-01-06","2011-01-08","2011-04-06","2012-05-05"))

# Choose the correct functions 
switch (datasets,
  assetsBAR = AHA_assetsBAR(cfg),
  assetsNOR = AHA_assetsNOR(cfg),
  nettopo   = AHA_nettopo(cfg),
  storingen = AHA_storingen(cfg),
  validatie = AHA_validatie(cfg)
)

# Final message
setpbarwrapper(cfg$pb, label = "Done");
}

# Different functions for the dataset calculations----------------------------
AHA_assetsBAR = function(cfg){        
  # BAR Data ----------------------------------------------------------------
  setpbarwrapper(cfg$pb, label = "Loading BAR data");
  # Laad de assets en converteer de datums als deze verkeerd staan 
  load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_BAR_assets.Rda"))
  assets$LSkabel[,DateAdded:=as.Date(DateAdded)]
  assets$LSkabel[,DateRemoved:=as.Date(DateRemoved)]
  assets$LSkabel[,DateLength_ch:=as.Date(DateLength_ch)]
  
  setpbarwrapper(cfg$pb, label = "Calculating BAR data");
  # Laad alleen dat deel van de assets dat binnen de periode valt
  # moffen
  MSm=(assets$MSmoffen$DateAdded > cfg$firstdate & assets$MSmoffen$DateAdded < cfg$lastdate)| 
    (assets$MSmoffen$DateRemoved > cfg$firstdate & assets$MSmoffen$DateRemoved < cfg$lastdate)
  MSm[is.na(MSm)] = FALSE
  assets$MSmoffen = assets$MSmoffen[MSm]
  
  LSm = (assets$LSmoffen$DateAdded > cfg$firstdate & assets$LSmoffen$DateAdded < cfg$lastdate)| 
    (assets$LSmoffen$DateRemoved > cfg$firstdate & assets$LSmoffen$DateRemoved < cfg$lastdate)
  LSm[is.na(LSm)] = FALSE
  assets$LSmoffen = assets$LSmoffen[LSm]
  
  # kabels
  MSk=(assets$MSkabels$DateAdded > cfg$firstdate & assets$MSkabels$DateAdded < cfg$lastdate)| 
    (assets$MSkabels$DateLength_ch > cfg$firstdate & assets$MSkabels$DateLength_ch < cfg$lastdate)|  
    (assets$MSkabels$DateRemoved > cfg$firstdate & assets$MSkabels$DateRemoved < cfg$lastdate)
  MSk[is.na(MSk)] = FALSE
  assets$MSkabels = assets$MSkabels[MSk]
  
  LSk=(assets$LSkabels$DateAdded > cfg$firstdate & assets$LSkabels$DateAdded < cfg$lastdate)| 
    (assets$LSkabels$DateLength_ch > cfg$firstdate & assets$LSkabels$DateLength_ch < cfg$lastdate)|  
    (assets$LSkabels$Date_Status_ch > cfg$firstdate & assets$LSkabels$Date_Status_ch < cfg$lastdate)|  
    (assets$LSkabels$DateRemoved > cfg$firstdate & assets$LSkabels$DateRemoved < cfg$lastdate)
  LSk[is.na(LSk)]=FALSE
  assets$LSkabels = assets$LSkabels[LSk]
  
  assets$MSmoffen[,PC_4:=substr(assets$MSmoffen$PC_6,1,4)]
  assets$LSmoffen[,PC_4:=substr(assets$LSmoffen$PC_6,1,4)]
  
  assets$LSkabels[,PC_4_van:=substr(assets$LSkabels$PC_6_van,1,4)]
  assets$LSkabels[,PC_4_naar:=substr(assets$LSkabels$PC_6_naar,1,4)]  
  assets$MSkabels[,PC_4_van:=substr(assets$MSkabels$PC_6_van,1,4)]
  assets$MSkabels[,PC_4_naar:=substr(assets$MSkabels$PC_6_naar,1,4)]  
  
  assets$MSmoffen[,System:="BAR"]
  assets$LSmoffen[,System:="BAR"]
  assets$MSkabels[,System:="BAR"]
  assets$LSkabels[,System:="BAR"]
  
  assets$MSHLDROUTE = NULL
  
  # Opsplitsen in MS en LS, zo zit het in de BARlog ook
  setpbarwrapper(cfg$pb, label = "Saving BAR data");
  save(assets,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_BAR.Rda"))
  remove("assets")
  
}
AHA_assetsNOR = function(cfg){        
  
  # NOR Data ----------------------------------------------------------------
  setpbarwrapper(cfg$pb, label = "Loading NOR data");
  # Laad de assets en converteer de datums als deze verkeerd staan 
  load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))
  try(setnames(assets$kabels,"PC_6_naar.y","PC_6_naar"))
  assets$moffen[is.na(DateRemoved),DateRemoved:=as.Date("01-01-1970")]
  
  LSm = (assets$moffen$DateAdded > cfg$firstdate & assets$moffen$DateAdded < cfg$lastdate & !(assets$moffen$DateAdded %in% cfg$BadDates$moffen))|
    (assets$moffen$DateRemoved > cfg$firstdate & assets$moffen$DateRemoved < cfg$lastdate & !(assets$moffen$DateRemoved %in% cfg$BadDates$moffen))
  LSm[is.na(LSm)] = FALSE
  assets$moffen = assets$moffen[LSm]
  
  setpbarwrapper(cfg$pb, label = "Calculating NOR data");
  
  # kabels
  assets$kabels[is.na(DateRemoved),DateRemoved:=as.Date("01-01-1970")]
  assets$kabels[is.na(DateLength_ch),DateLength_ch:=as.Date("01-01-1970")]
  assets$kabels[is.na(Date_Status_ch),Date_Status_ch:=as.Date("01-01-1970")]
  assets$kabels[is.na(DateAdded),DateAdded:=as.Date("01-01-1970")]
  
  MSk=(assets$kabels$DateAdded > cfg$firstdate & assets$kabels$DateAdded < cfg$lastdate & !(assets$kabels$DateAdded %in% cfg$BadDates$kabels))| 
    (assets$kabels$Date_Status_ch > cfg$firstdate & assets$kabels$Date_Status_ch < cfg$lastdate & !(assets$kabels$Date_Status_ch %in% cfg$BadDates$kabels))|  
    (assets$kabels$DateLength_ch > cfg$firstdate & assets$kabels$DateLength_ch < cfg$lastdate & !(assets$kabels$DateLength_ch %in% cfg$BadDates$kabels))|  
    (assets$kabels$DateRemoved > cfg$firstdate & assets$kabels$DateRemoved < cfg$lastdate & !(assets$kabels$DateRemoved %in% cfg$BadDates$kabels))
    
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
  
  assets$MSmoffen[,System:="NOR"]
  assets$LSmoffen[,System:="NOR"]
  assets$MSkabels[,System:="NOR"]
  assets$LSkabels[,System:="NOR"]
  
  assets$moffen = NULL
  assets$kabels = NULL
  
  setpbarwrapper(cfg$pb, label = "Saving NOR data");
  save(assets,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_NOR.Rda"))
  remove("assets")
  
}
AHA_nettopo   = function(cfg){ 
  # EAN-Hoofdleiding-XY-PC data ----------------
  nettopo = list();  
  
  # Load the data
  setpbarwrapper(cfg$pb, label = "Loading Nettopo data");
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
  
  setpbarwrapper(cfg$pb, label = "Calculating Nettopo data");
  nettopo = unique(Hoofdleiding_Station)[unique(EAN_Aansluitingen),
                                         list(ID_EAN,ID_Hoofdleiding,ID_Station,ID_Kabel,Naam_Ruimte,ID_NR_Ruimte,
                                              ID_Stationbehuizing,Coo_X,Coo_Y,Adres,SJV,SJV_Laag,Profiel_Type)]
  
  set(nettopo,which(is.na(nettopo$ID_Hoofdleiding)),j=1:ncol(nettopo),
      unique(Hoofdleiding_Station2)[unique(
        EAN_Aansluitingen)[is.na(nettopo$ID_Hoofdleiding)],
        list(ID_EAN,ID_Hoofdleiding,ID_Station,ID_Kabel,Naam_Ruimte,ID_NR_Ruimte,
             ID_Stationbehuizing,Coo_X,Coo_Y,Adres,SJV,SJV_Laag,Profiel_Type)])
  
  # Add the MS routes
  setkey(nettopo,ID_Stationbehuizing)
  setkey(MS_Stations,ID_Stationbehuizing)
  
  nettopo = unique(MS_Stations[,list(ID_Stationbehuizing,ID_NR_Ruimte,ID_Voedend_Station,Routenaam)])[nettopo]
  nettopo[,PC_6:=as.character(substr(Adres, 1, 6))]
  nettopo[,PC_4:=as.character(substr(Adres, 1, 4))]
  nettopo[,Huisnr:=adrsplit(Adres)]
  
  
  # Add adress density figures
  setkey(nettopo,PC_4)
  setkey(adressendichtheid,PC_4)
  setnames(pc4area,c("PC_4","Oppervlakte_PC4"))
  setkey(pc4area,PC_4);
  
  nettopo = adressendichtheid[nettopo]
  nettopo = pc4area[nettopo]
  
  # Save
  setpbarwrapper(cfg$pb, label = "Saving Nettopo data");
  save(nettopo,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo_EAN.Rda"))
}
AHA_storingen = function(cfg){
  # Storingsdata uit KLAK ------------------------
  
  storingen=list()  
  setpbarwrapper(cfg$pb, label = "Loading KLAK data");
  
  # Laad de data om adressen te koppelen
  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_LS.Rda"))
  mindataset[,Datum:=as.Date(mindataset$Datum)]
  storingen$LS= mindataset[(mindataset$Datum > cfg$firstdate & mindataset$Datum < cfg$lastdate)]
  storingen$LS[,Brontabel := "LS storingen"]
  
  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_MS.Rda"))
  mindataset[,Datum:=as.Date(mindataset$Datum)]
  storingen$MS= mindataset[(mindataset$Datum > cfg$firstdate & mindataset$Datum < cfg$lastdate)]
  storingen$MS[,Brontabel := "MS storingen"]
  
  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_KOPPEL_MELDING_GROEP.Rda"))
  storingen$KLAKMelders = (mindataset);
  storingen$KLAKMelders$Brontabel = "Melder"
  
  load(paste0(settings$Ruwe_Datasets,"/21. GIS-mutaties/GISMUTATIE.Rda"))
  setkey(mindataset,ID_KLAK_Melding)
  gismutaties = mindataset
  
  setpbarwrapper(cfg$pb, label = "Loading Nettoplogy data");
  load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo_EAN.Rda"))

  load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_Stations.Rda")); 
  MS_Stations = mindataset
  
  # Add and correct Postcodes
  setpbarwrapper(cfg$pb, label = "Process KLAK data");
  
  storingen$LS[,Huisnr:=(onlynum(Adres))]
  
  setkey(storingen$LS,ID_KLAK_Melding)
  setkey(storingen$KLAKMelders,ID_KLAK_Melding)
  
  storingen$MS[,Huisnr := as.numeric(NA)] 
  storingen$MS[,Coo_X := as.numeric(NA)] 
  storingen$MS[,Coo_Y := as.numeric(NA)] 
  storingen$MS[,ID_Hoofdleiding := as.character(NA)] 
  storingen$MS[,Routenaam := as.character(NA)] 
  
  storingen$MS[,PC_6 := gsub(" ","",PC_6)] 
  storingen$MS[,PC_4:=substr(storingen$MS$PC_6,1,4)]
  
  storingen$LS[,PC_6 := gsub(" ","",PC_6)] 
  storingen$LS[,PC_4:=substr(storingen$LS$PC_6,1,4)]
  
  storingen$KLAKMelders[,PC_6 := gsub(" ","",PC_6)] 
  storingen$KLAKMelders[,PC_4 := substr(PC_6,1,4)]
  
  # Add the ms hoofdleidingen based on the sections
  MS_Stations[,Naam_Onderstation:=gsub("150/20KV |50KV |SH |SS | 10KV| \\(Normaal\\)|RS |OS ", "",Naam_voedend_station)]
  MS_Stations[,Sectie:=Lokale_naam]
  
  storingen$Routenamen = storingen$MS[,list(Naam_Onderstation,ID_KLAK_Melding)]
  storingen$Routenamen = data.table(Naam_Onderstation=storingen$MS$Naam_Onderstation,
                                    ID_KLAK_Melding = storingen$MS$ID_KLAK_Melding,
                                    Sectie=c(storingen$MS$Sectie_naar,storingen$MS$Sectie_van))
  
  setkey(MS_Stations,Sectie)
  setkey(storingen$Routenamen,Sectie)
  
  storingen$Routenamen = MS_Stations[,list(Sectie,Routenaam)][storingen$Routenamen,allow.cartesian=TRUE]
  
  setkey(storingen$Routenamen,ID_KLAK_Melding,Routenaam)
  storingen$Routenamen= unique(storingen$Routenamen)
  
  # Correct the KLAK meldingen, want alleen eerste melders tellen
  setnames(storingen$KLAKMelders,"ID_KLAK_Melding","ID_KLAK_Melding_oud")
  setkey(storingen$KLAKMelders,ID_Groep)
  temp = unique(storingen$KLAKMelders[ID_Groep!="" & !is.na(ID_Groep) & ST_Groep_eerste=="Ja",
                                      list(ID_Groep,ID_KLAK_Melding_oud)])
  setnames(temp,"ID_KLAK_Melding_oud","ID_KLAK_Melding")
  setkey(temp,ID_Groep)
  storingen$KLAKMelders=temp[storingen$KLAKMelders]
  
  # Pak alleen relevante meldingen
  storingen$KLAKMelders=storingen$KLAKMelders[(storingen$KLAKMelders$ID_KLAK_Melding %in% storingen$MS$ID_KLAK_Melding) | (storingen$KLAKMelders$ID_KLAK_Melding %in% storingen$LS$ID_KLAK_Melding)]
  
  # Bepaal het aantal melders  
  storingen$KLAKMelders[!(ID_Groep!="" & !is.na(ID_Groep)),ID_KLAK_Melding:=ID_KLAK_Melding_oud]
  storingen$KLAKMelders[,Aantal_Melders:=length(PC_6),by=ID_KLAK_Melding]
  
  # Koppel de GIS mutaties
  setkey(storingen$LS,ID_KLAK_Melding);
  setkey(storingen$MS,ID_KLAK_Melding);
  storingen$LS=gismutaties[storingen$LS];
  storingen$MS=gismutaties[storingen$MS];
  
  # Add the nettopology
  setkey(storingen$KLAKMelders ,PC_6,Huisnr); 
  setkey(storingen$LS ,PC_6,Huisnr); 
  setkey(nettopo,PC_6,Huisnr)
  storingen$KLAKMelders = unique(nettopo)[storingen$KLAKMelders]
  storingen$LS = unique(nettopo)[storingen$LS]
  
  save(storingen,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))
}
AHA_validatie = function(cfg){
  # Validatieset van de storingsregistratoren-----------------------------------
  
  setpbarwrapper(cfg$pb, label = "Loading Validatie data");
  
  load(paste0(settings$Ruwe_Datasets,"/23. Validatie_data/Koppeling KLAK-NRG Dick Grollers..Rda"))
  ValidatieSet = mindataset[,Regio := "Zuid-Oost"]
  
  load(paste0(settings$Ruwe_Datasets,"/23. Validatie_data/Koppeling KLAK-NRG Robert Aikema..Rda"))
  ValidatieSet = rbind(ValidatieSet, mindataset[,Regio := "Amsterdam"])
  setpbarwrapper(cfg$pb, label = "Loading Validatie data");
  
  try(setnames(ValidatieSet,"ID_KLAK","ID_KLAK_Melding"))
  ValidatieSet[,ID_Asset:=NULL]; 
  ValidatieSet = ValidatieSet[!is.na(ValidatieSet$ID_KLAK_Melding)]
  ValidatieSet$ID_KLAK_Melding = as.character(ValidatieSet$ID_KLAK_Melding)
  setpbarwrapper(cfg$pb, label = "Saving Validatie data");
  
  save(ValidatieSet,file=paste0(settings$Input_Datasets,"/23. Validatie_data/Validatie koppelingen.Rda"))
}