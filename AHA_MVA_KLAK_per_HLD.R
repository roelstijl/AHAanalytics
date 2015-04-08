AHA_MVA_KLAK_per_HLD = function()
{
# Created by Roel Stijl (Bearingpoint) 2015
# Script is used to calculate the number of KLAK reports on a given cable of Ms Route
# No input required
storingen = list()
storingen$KLAKMelders = LoadWrap(paste0(settings$Ruwe_Datasets,"/25. KLAK-OUD/KLAK_MELDING_XY.Rda"))
storingen$LS = LoadWrap(paste0(settings$Ruwe_Datasets,"/25. KLAK-OUD/KLAK_LS.Rda"))
storingen$MS = LoadWrap(paste0(settings$Ruwe_Datasets,"/25. KLAK-OUD/KLAK_MS.Rda"))

# Load nettopology data
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo_EAN.Rda"))
MS_Stations = LoadWrap(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_Stations.Rda")); 
MS_hoofdleidingen = LoadWrap(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_hoofdleidingen.Rda")); 

l_ply(names(storingen),function(x) storingen[[x]][,PC_6 :=gsub(" ","",PC_6)])

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
                             )))})

station_namen_van = laply(MS_hoofdleidingen$Van_Station_A,
                          function(x){
                            (matrix(switch(ifelse(length(unlist(strsplit(x,"/")))==2,"two","nope"),
                                           two = unlist(strsplit(x,"/")),
                                           nope=c(NA,NA)
                            )))})

MS_hoofdleidingen[,Naam_Station_Van := gsub(toremove,"",tolower(station_namen_van[,2]))]
MS_hoofdleidingen[,Naam_Station_Naar := gsub(toremove,"",tolower(station_namen_naar[,2]))]
MS_hoofdleidingen[,Nummer_Station_Van := gsub(toremove,"",tolower(station_namen_van[,1]))]
MS_hoofdleidingen[,Nummer_Station_Naar := gsub(toremove,"",tolower(station_namen_naar[,1]))]

setkey(MS_hoofdleidingen,Naam_Station_Van,Naam_Station_Naar)
setkey(storingen$Nettopologie,Naam_Station_Van,Naam_Station_Naar)

MS_hoofdleidingen=unique(MS_hoofdleidingen)
storingen$Nettopologie[,ID_Hoofdleiding := MS_hoofdleidingen[storingen$Nettopologie,ID_Hoofdleiding]]
storingen$Nettopologie[,ID_Verbinding   := MS_hoofdleidingen[storingen$Nettopologie,ID_Verbinding]]

setkey(storingen$MS,ID_KLAK_Melding)
setkey(storingen$Nettopologie,ID_KLAK_Melding)

storingen$MS = storingen$Nettopologie[storingen$MS,allow.cartesian=T]

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

# Add the nettopology
setkey(storingen$KLAKMelders,PC_6,Huisnr); 
setkey(nettopo,PC_6,Huisnr)
storingen$KLAKMelders = unique(nettopo[,list(ID_Hoofdleiding,PC_6,Huisnr,ID_Verbinding)])[storingen$KLAKMelders]

setkey(storingen$KLAKMelders,ID_KLAK_Melding)
setkey(storingen$LS,ID_KLAK_Melding)
setkey(storingen$MS,ID_KLAK_Melding)

storingen$KLAKMelders[,Datum:=unique(storingen$LS)[storingen$KLAKMelders,Datum]]
storingen$KLAKMelders[is.na(storingen$KLAKMelders$Datum),Datum:=unique(storingen$MS)[storingen$KLAKMelders[is.na(Datum)],Datum]]

storingen$KLAKMelders[,KLAKLS:=unique(storingen$LS)[storingen$KLAKMelders,ID_KLAK_Melding]]


setkey(KLAKMelders,ID_Verbinding,ID_Hoofdleiding,)

storingen$MS = storingen$MS[,list(ID_Hoofdleiding,Routenaam,ID_KLAK_Melding,Datum)]



save(storingen,file=paste0(settings$Input_Datasets,"/25. KLAK-OUD/KLAK_Meldingen_per_HLD.Rda"),compress=F)
}