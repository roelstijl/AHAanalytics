AHA_Data_KA_Proxy_Preprocessing = function(datasets=c("assetsBAR","assetsNOR","nettopo","storingen","validatieset")){
# Merges the Delta data into small datasets for analysis
# Datasets selects only a certain set to process (default = all)
# cfg$firstdate_NOR is the startdate of the data subset
# last date is the last date to take
#
# Settings --------------------------------
cfg = list()
cfg$pb  = pbarwrapper(title = paste0("AHA_Data_KA_Proxy_Preprocessing: ",as.character(Sys.time())),label = "Start", min = 0, max = 3*length(datasets)+1);

# Specify the dates from to
cfg$firstdate_NOR = as.Date("2007-02-01")
cfg$firstdate_BAR = as.Date("2014-02-14")
cfg$lastdate      = as.Date("2015-02-01")

# Choose the correct functions --------------------
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

AHA_assetsBAR = function(cfg){   
# BAR Data -------------------------------
setpbarwrapper(cfg$pb, label = "Loading BAR data");
# Laad de assets en converteer de datums als deze verkeerd staan 
load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_BAR_assets.Rda"))
assets$MSHLDROUTE = NULL

setpbarwrapper(cfg$pb, label = "Calculating BAR data");
# behoud alleen dat deel van de assets dat binnen de periode veranderd is.

minassets = list();

minassets$LSkabels=assets$LSkabels[(!is.na(DateAdded)& DateAdded > cfg$firstdate_BAR & DateAdded < cfg$lastdate) | 
                                 (!is.na(DateRemoved)& DateRemoved > cfg$firstdate_BAR & DateRemoved < cfg$lastdate)|
                                 (!is.na(DateStatus_ch)& DateStatus_ch > cfg$firstdate_BAR & DateStatus_ch < cfg$lastdate & substrRight(Status_ch,11)=="Uit Bedrijf")|
                                 (!is.na(DateLength_ch)& DateLength_ch > cfg$firstdate_BAR & DateLength_ch < cfg$lastdate)|
                                   Date_Last_Modified!=as.Date("2014-05-18")]

minassets$MSkabels=assets$MSkabels[(!is.na(DateAdded)& DateAdded > cfg$firstdate_BAR & DateAdded < cfg$lastdate) | 
                                     (!is.na(DateRemoved)& DateRemoved > cfg$firstdate_BAR & DateRemoved < cfg$lastdate)|
                                     (!is.na(DateStatus_ch)& DateStatus_ch > cfg$firstdate_BAR & DateStatus_ch < cfg$lastdate & substrRight(Status_ch,11)=="Uit Bedrijf")|
                                     (!is.na(DateLength_ch)& DateLength_ch > cfg$firstdate_BAR & DateLength_ch < cfg$lastdate)|
                                     Date_Last_Modified!=as.Date("2014-05-18")]

minassets$MSmoffen=assets$MSmoffen[(!is.na(DateAdded)& DateAdded > cfg$firstdate_BAR & DateAdded < cfg$lastdate) | 
                                 (!is.na(DateRemoved)& DateRemoved > cfg$firstdate_BAR & DateRemoved < cfg$lastdate)|
                                   Date_Last_Modified!=as.Date("2014-05-18")]

minassets$LSmoffen=assets$LSmoffen[((!is.na(DateAdded)& DateAdded > cfg$firstdate_BAR & DateAdded < cfg$lastdate) | 
                                 (!is.na(DateRemoved)& DateRemoved > cfg$firstdate_BAR & DateRemoved < cfg$lastdate))|
                                   Date_Last_Modified!=as.Date("2014-05-18")]

assets = minassets

# Opsplitsen in MS en LS, zo zit het in de BARlog ook
setpbarwrapper(cfg$pb, label = "Saving BAR data");
save(assets,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_BAR.Rda"),compress=F)
}

AHA_assetsNOR = function(cfg){        

# NOR Data
setpbarwrapper(cfg$pb, label = "Loading NOR data");

# Set what dates unexplainable bumps in the data occured, NOR
cfg$BadDates = list()
cfg$BadDates$kabels = list(
  DateAdded   = as.Date(c("2007-01-06","2010-07-03",'2011-02-05', "2011-06-04","2012-05-05","2010-04-03","2012-12-08")),
  DateRemoved = as.Date(c("2007-01-06","2010-07-03",'2011-02-05', "2011-06-04","2012-05-05","2010-07-03")),
  Date_Status_ch = as.Date(c("2012-12-08")),
  DateLength_ch = as.Date(c("2015-01-03","2012-12-08","2007-11-03","2011-02-05","2007-09-08"))
)

cfg$BadDates$moffen = list(
  DateAdded   = as.Date(c("2007-01-06","2011-06-04","2011-02-05","2010-07-03",'2011-02-05', "2011-06-04","2012-05-05","2012-12-08","2010-04-03"),
  DateRemoved = as.Date(c("2007-01-06","2010-07-03",'2011-02-05', "2011-06-04","2012-05-05","2011-08-06","2010-07-03","2010-04-03")))
)

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
                                 (!is.na(Date_Status_ch)& Date_Status_ch > cfg$firstdate_NOR & Date_Status_ch < cfg$lastdate & !(Date_Status_ch %in% cfg$BadDates$kabels$Date_Status_ch) & substrRight(Status_ch, 14)=="Buiten Bedrijf")|
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
# EAN-Hoofdleiding-XY-PC data
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

# Load BAR data for Verbindingen of Kabels
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_KABELS_XY.Rda"))
BAR_LS_Kabels = mindataset[,list(ID_Verbinding, ID_NAN,ID_Hoofdleiding,ID_Kabel)]

rm("mindataset")

# Start calculating the nettopology
# Add the ID_Verbinding
setkey(EAN_Aansluitingen,ID_Kabel)
setkey(BAR_LS_Kabels,ID_Kabel)
EAN_Aansluitingen[,ID_Verbinding:=unique(BAR_LS_Kabels)[EAN_Aansluitingen,ID_Verbinding]]

# Combine the sets
setkey(Hoofdleiding_Station,ID_EAN)
setkey(Hoofdleiding_Station2,ID_EAN)
setkey(EAN_Aansluitingen,ID_EAN)

setpbarwrapper(cfg$pb, label = "Calculating Nettopo data");
nettopo = unique(Hoofdleiding_Station)[unique(EAN_Aansluitingen),
                                       list(ID_EAN,ID_Hoofdleiding,ID_Station,ID_Kabel,ID_Verbinding,Naam_Ruimte,ID_NR_Ruimte,
                                            ID_Stationbehuizing,Coo_X,Coo_Y,Adres,SJV,SJV_Laag,Profiel_Type)]

set(nettopo,which(is.na(nettopo$ID_Hoofdleiding)),j=1:ncol(nettopo),
    unique(Hoofdleiding_Station2)[unique(
      EAN_Aansluitingen)[is.na(nettopo$ID_Hoofdleiding)],
      list(ID_EAN,ID_Hoofdleiding,ID_Station,ID_Kabel,Naam_Ruimte,ID_Verbinding,ID_NR_Ruimte,
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
save(nettopo,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo_EAN.Rda"),compress=F)
}

AHA_storingen = function(cfg){
# Storingsdata uit KLAK

storingen=list()  
setpbarwrapper(cfg$pb, label = "Loading KLAK data");

# Load the date required and do some basic operations ---------------------
# Load the LS KLAK
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_LS.Rda"))
mindataset[,Datum:=as.Date(mindataset$Datum)]
storingen$LS= unique(mindataset[(mindataset$Datum > cfg$firstdate_NOR & mindataset$Datum < cfg$lastdate)],by="ID_KLAK_Melding")
storingen$LS[,Brontabel := "LS storingen"]

# Add PC6 based coordinates
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_PC_6_met_Coo.Rda"))
PC_6_Coo = mindataset

# Load the MS KLAK
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_MS.Rda"))
mindataset[,Datum:=as.Date(mindataset$Datum)]
storingen$MS= mindataset[(mindataset$Datum > cfg$firstdate_NOR & mindataset$Datum < cfg$lastdate)]
storingen$MS[,Brontabel := "MS storingen"]

# Add XY based on the lon lat in the systems (converted to XY elsewhere)
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK MS en klant melding_XY.Rda"))
try(setnames(mindataset,"KLAK_MELDING","ID_KLAK_Melding"))
setkey(mindataset,ID_KLAK_Melding)
setkey(storingen$MS,ID_KLAK_Melding)
storingen$MS = mindataset[,list(ID_KLAK_Melding,Coo_X,Coo_Y)][storingen$MS]

# Load Meldingen
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_KOPPEL_MELDING_GROEP.Rda"))
storingen$KLAKMelders = (mindataset);
storingen$KLAKMelders[,Brontabel := "Melder"]
                
# Load GISmutaties
load(paste0(settings$Ruwe_Datasets,"/21. GIS-mutaties/GISMUTATIE.Rda"))
setkey(mindataset,ID_KLAK_Melding)
gismutaties = mindataset

# Load nettopology data
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo_EAN.Rda"))

load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_Stations.Rda")); 
MS_Stations = mindataset

load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_hoofdleidingen.Rda")); 
MS_hoofdleidingen = mindataset

# Correct the data ---------------------------------
setpbarwrapper(cfg$pb, label = "Process KLAK data");
storingen$LS[,Huisnr:=(onlynum(Adres))]

# Correct the PC with spaces and add PC4
l_ply(names(storingen),function(x) storingen[[x]][,PC_6 :=gsub(" ","",PC_6)])
l_ply(names(storingen),function(x) storingen[[x]][,PC_4 :=substr(PC_6,1,4)])

# Add coordinates based on center of PC_6
setkey(PC_6_Coo,PC_6)
setkey(storingen$LS,PC_6)
storingen$LS = PC_6_Coo[,list(PC_6,Coo_X_PC_6,Coo_Y_PC_6)][storingen$LS]

# Add the ms routenamen based on sections
MS_Stations[,Naam_Onderstation:=gsub("150/20KV |50KV |SH |SS | 10KV| \\(Normaal\\)|RS |OS ", "",ID_Voedend_Station)]
toremove = " |,|-|\\.|\\'"
MS_Stations[,Sectie:=gsub(toremove,"",tolower(Lokale_naam))]

storingen$Nettopologie = data.table(Naam_Onderstation=storingen$MS$Naam_Onderstation,
                                  ID_KLAK_Melding = storingen$MS$ID_KLAK_Melding,
                                  Naam_Station_Van = gsub(toremove,"",tolower(c(storingen$MS$Sectie_van))),
                                  Naam_Station_Naar = gsub(toremove,"",tolower(c(storingen$MS$Sectie_naar))),
                                  Sectie=gsub(toremove,"",tolower(c(storingen$MS$Sectie_naar,storingen$MS$Sectie_van))))

storingen$Nettopologie[Sectie==""| Sectie=="unset",Sectie:=NA]

setkey(MS_Stations,Sectie)
setkey(storingen$Nettopologie,Sectie)
storingen$Nettopologie = unique(MS_Stations[,list(Sectie,Routenaam)])[storingen$Nettopologie]
setkey(storingen$Nettopologie,ID_KLAK_Melding,Routenaam)

# Add the hoofdleidingen based on sections
station_namen_naar = laply(MS_hoofdleidingen$Naar_Station_B,
      function(x){
        (matrix(switch(ifelse(length(unlist(strsplit(x,"/")))==2,"two","nope"),
                       two = unlist(strsplit(x,"/")),
                       nope=c(NA,NA)
        )        )        )      })

station_namen_van = laply(MS_hoofdleidingen$Van_Station_A,
                           function(x){
                             (matrix(switch(ifelse(length(unlist(strsplit(x,"/")))==2,"two","nope"),
                                            two = unlist(strsplit(x,"/")),
                                            nope=c(NA,NA)
        )        )        )      })

MS_hoofdleidingen[,Naam_Station_Van := gsub(toremove,"",tolower(station_namen_van[,2]))]
MS_hoofdleidingen[,Naam_Station_Naar := gsub(toremove,"",tolower(station_namen_naar[,2]))]
MS_hoofdleidingen[,Nummer_Station_Van := gsub(toremove,"",tolower(station_namen_van[,1]))]
MS_hoofdleidingen[,Nummer_Station_Naar := gsub(toremove,"",tolower(station_namen_naar[,1]))]

setkey(MS_hoofdleidingen,Naam_Station_Van,Naam_Station_Naar)
setkey(storingen$Nettopologie,Naam_Station_Van,Naam_Station_Naar)

storingen$Nettopologie[, ID_Hoofdleiding := MS_hoofdleidingen[storingen$Nettopologie,ID_Hoofdleiding]]
storingen$Nettopologie[, ID_Verbinding   := MS_hoofdleidingen[storingen$Nettopologie,ID_Verbinding]]


# Correct the KLAK meldingen, want alleen eerste melders tellen
setnames(storingen$KLAKMelders,"ID_KLAK_Melding","ID_KLAK_Melding_oud")
setkey(storingen$KLAKMelders,ID_Groep)
temp = unique(storingen$KLAKMelders[ID_Groep!="" & !is.na(ID_Groep) & ST_Groep_eerste=="Ja",
                                    list(ID_Groep,ID_KLAK_Melding_oud)])
setnames(temp,"ID_KLAK_Melding_oud","ID_KLAK_Melding")
setkey(temp,ID_Groep)
storingen$KLAKMelders=temp[storingen$KLAKMelders]

# Pak alleen relevante meldingen
setkey(storingen$LS,ID_KLAK_Melding)
setkey(storingen$KLAKMelders,ID_KLAK_Melding)
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

save(storingen,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"),compress=F)
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
