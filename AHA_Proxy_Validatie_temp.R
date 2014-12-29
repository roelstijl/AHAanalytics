load("C:/Datasets/AHAdata/2. Input Datasets/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda")
load("C:/Datasets/AHAdata/2. Input Datasets/1. AID KID proxy/AHA_Proxy_partial_data_assets.Rda")

load("C:/Datasets/AHAdata/1. Ruwe Datasets/23. Validatie_data/Koppeling KLAK-NRG Dick Grollers..Rda")
mindataset[,Regio:="Zuid_oost"]
validatie = mindataset

load("C:/Datasets/AHAdata/1. Ruwe Datasets/23. Validatie_data/Koppeling KLAK-NRG Robert Aikema..Rda")
mindataset[,Regio:="Amsterdam"]
validatie = (rbind(mindataset,validatie))
validatie = unique(validatie[!is.na(validatie$ID_NAN)])
validatie$ID_KLAK_Melding = as.character(validatie$ID_KLAK_Melding)

load("C:/Datasets/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_LS.Rda")
KLAK = mindataset
load("C:/Datasets/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_MS.Rda")
KLAK = rbind(KLAK,mindataset,fill=TRUE)

validatie = validatie[ID_KLAK_Melding %in% KLAK$ID_KLAK_Melding]
validatie[ID_NAN %in% a$ID_NAN]

a = rbind(assets$LSkabels,assets$MSkabels,assets$LSmoffen,assets$MSmoffen,fill=TRUE)
b = rbind(storingen$LS,storingen$MS,fill=TRUE)

c=validatie[ID_NAN %in% a$ID_NAN]
d=validatie[ID_KLAK_Melding %in% b$ID_KLAK_Melding]
d=validatie[ID_KLAK_Melding %in% mindataset$ID_KLAK_Melding]

setkey(validatie,ID_KLAK_Melding)
setkey(b,ID_KLAK_Melding)

d=b[validatie]
d[!is.naID_KLAK_Melding]

nrow(c[Regio=="Amsterdam"]) / nrow(validatie[Regio=="Amsterdam"])
nrow(c[Regio=="Zuid_oost"]) / nrow(validatie[Regio=="Zuid_oost"])

nrow(d[Regio=="Amsterdam"]) / nrow(validatie[Regio=="Amsterdam"])
nrow(d[Regio=="Zuid_oost"]) / nrow(validatie[Regio=="Zuid_oost"])
