AHA_Data_KA_Proxy_Preprocessing = function(datasets=c("assetsBAR","assetsNOR","nettopo","storingen","validatieset")){
# Merges the Delta data into small datasets for analysis
# Datasets selects only a certain set to process (default = all)
# cfg$firstdate_NOR is the startdate of the data subset
# last date is the last date to take
#
# Settings  ----------------------
cat("Starting\n")
cfg = list()
cfg$pb  = pbarwrapper(title = paste0("AHA_Data_KA_Proxy_Preprocessing: ",as.character(Sys.time())), 
                      label = "Start", min = 0, max = 3*length(datasets)+1, initial = 0, width = 450);

# Specify the dates from to
cfg$firstdate_NOR = as.Date("2007-02-01")
cfg$firstdate_BAR = as.Date("2014-02-14")
cfg$lastdate      = as.Date("2015-02-01")


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
assets$MSHLDROUTE = NULL

setpbarwrapper(cfg$pb, label = "Calculating BAR data");
# behoud alleen dat deel van de assets dat binnen de periode veranderd is.
assets = lapply(assets,function(x)
{x[!(x$DateAdded == cfg$firstdate_BAR & x$Status_ID=="Active")]})
  
assets$MSkabels=assets$MSkabels[(!is.na(DateAdded)& DateAdded > cfg$firstdate_NOR & DateAdded < cfg$lastdate) | 
                                 (!is.na(DateRemoved)& DateRemoved > cfg$firstdate_NOR & DateRemoved < cfg$lastdate )]
assets$LSkabels=assets$LSkabels[(!is.na(DateAdded)& DateAdded > cfg$firstdate_NOR & DateAdded < cfg$lastdate ) | 
                                 (!is.na(DateRemoved)& DateRemoved > cfg$firstdate_NOR & DateRemoved < cfg$lastdate )]

assets$MSmoffen=assets$MSmoffen[(!is.na(DateAdded)& DateAdded > cfg$firstdate_NOR & DateAdded < cfg$lastdate) | 
                                 (!is.na(DateRemoved)& DateRemoved > cfg$firstdate_NOR & DateRemoved < cfg$lastdate)]
assets$LSmoffen=assets$LSmoffen[(!is.na(DateAdded)& DateAdded > cfg$firstdate_NOR & DateAdded < cfg$lastdate) | 
                                 (!is.na(DateRemoved)& DateRemoved > cfg$firstdate_NOR & DateRemoved < cfg$lastdate)]


# Opsplitsen in MS en LS, zo zit het in de BARlog ook
setpbarwrapper(cfg$pb, label = "Saving BAR data");
save(assets,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_BAR.Rda"))
remove("assets")

}
AHA_assetsNOR = function(cfg){        

# NOR Data ----------------------------------------------------------------
setpbarwrapper(cfg$pb, label = "Loading NOR data");

# Set what dates unexplainable bumps in the data occured, NOR
cfg$BadDates = list()
cfg$BadDates$kabels = list(
  DateAdded   = as.Date(c("2007-01-06","2010-07-03",'2011-02-05', "2011-06-04","2012-05-05")),
  DateRemoved = as.Date(c("1970-01-01","2007-01-06","2010-07-03",'2011-02-05', "2011-06-04","2012-05-05")),
  Date_Status_ch = as.Date(c("1970-01-01","2012-12-08")),
  DateLength_ch = as.Date("1970-01-01"))

cfg$BadDates$moffen = list(
  DateAdded   = as.Date(c("2007-01-06","2010-07-03",'2011-02-05', "2011-06-04","2012-05-05")),
  DateRemoved = as.Date(c("1970-01-01","2007-01-06","2010-07-03",'2011-02-05', "2011-06-04","2012-05-05")))

cfg$BadDates$Verbindingen = as.Date(ymd(c("2007-01-06","2011-08-01","2011-04-06","2012-05-05")))

# Laad de assets en converteer de datums als deze verkeerd staan 
load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))
try(setnames(assets$kabels,"PC_6_naar.y","PC_6_naar"))
setpbarwrapper(cfg$pb, label = "Calculating NOR data");

# Bereken postcode 4
assets$moffen$PC_4     = substr(assets$moffen$PC_6,1,4)
assets$kabels$PC_4_van = substr(assets$kabels$PC_6_van,1,4)
assets$kabels$PC_4_naar= substr(assets$kabels$PC_6_naar,1,4)  

# Gooi alle grote verandering weg

minassets = list()
minassets$kabels=assets$kabels[(!is.na(DateAdded)& DateAdded > cfg$firstdate_NOR & DateAdded < cfg$lastdate & !(DateAdded %in% cfg$BadDates$kabels$DateAdded)) | 
                                 (!is.na(DateRemoved)& DateRemoved > cfg$firstdate_NOR & DateRemoved < cfg$lastdate & !(DateRemoved %in% cfg$BadDates$kabels$DateRemoved))|
                                 (!is.na(Date_Status_ch)& Date_Status_ch > cfg$firstdate_NOR & Date_Status_ch < cfg$lastdate & !(Date_Status_ch %in% cfg$BadDates$kabels$Date_Status_ch))|
                                 (!is.na(DateLength_ch)& DateLength_ch > cfg$firstdate_NOR & DateLength_ch < cfg$lastdate & !(DateLength_ch %in% cfg$BadDates$kabels$DateLength_ch))]

minassets$moffen=assets$moffen[(!is.na(DateAdded)& DateAdded > cfg$firstdate_NOR & DateAdded < cfg$lastdate & !(DateAdded %in% cfg$BadDates$moffen$DateAdded)) | 
                                 (!is.na(DateRemoved)& DateRemoved > cfg$firstdate_NOR & DateRemoved < cfg$lastdate & !(DateRemoved %in% cfg$BadDates$moffen$DateRemoved))]

assets$LSkabels = minassets$kabels[Brontabel == "ls_kabels"]
assets$MSkabels = minassets$kabels[Brontabel == "ms_kabels"]
assets$LSmoffen = minassets$moffen[Brontabel == "ls_moffen"]
assets$MSmoffen = minassets$moffen[Brontabel == "ms_moffen"]

l_ply(names(assets), function(x) eval(parse(text=paste0("assets$", x,"[,System:=\"NOR\"]"))))

assets$kabels = NULL
assets$moffen = NULL

setpbarwrapper(cfg$pb, label = "Saving NOR data");
save(assets,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_NOR.Rda"),compress=F)
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
storingen$LS= mindataset[(mindataset$Datum > cfg$firstdate_NOR & mindataset$Datum < cfg$lastdate)]
storingen$LS[,Brontabel := "LS storingen"]

load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_MS.Rda"))
mindataset[,Datum:=as.Date(mindataset$Datum)]
storingen$MS= mindataset[(mindataset$Datum > cfg$firstdate_NOR & mindataset$Datum < cfg$lastdate)]
storingen$MS[,Brontabel := "MS storingen"]

load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_KOPPEL_MELDING_GROEP.Rda"))
storingen$KLAKMelders = (mindataset);
storingen$KLAKMelders[,Brontabel := "Melder"]
                                            
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
