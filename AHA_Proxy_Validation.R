AHA_Proxy_Validation =function()
{
# Load some files ----------------------------
load(paste0(settings$Ruwe_Datasets,"/23. Validatie_data/Koppeling KLAK-NRG Dick Grollers..Rda"))
ValidatieSet = mindataset[,Regio := "Amsterdam"]
load(paste0(settings$Ruwe_Datasets,"/23. Validatie_data/Koppeling KLAK-NRG Robert Aikema..Rda"))
ValidatieSet = rbind(ValidatieSet, mindataset[,Regio := "Zuid-Oost"])
ValidatieSet = ValidatieSet[!is.na(ValidatieSet$ID_KLAK_Melding)]
ValidatieSet$ID_KLAK_Melding = as.character(ValidatieSet$ID_KLAK_Melding)

load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))
assets$moffen=unique(assets$moffen,by="ID_NAN")
assets$kabels=unique(assets$kabels,by="ID_NAN")

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


# Merge the data for all assets --------------------------------
setkey(ValidatieSet ,ID_NAN)
setkey(assets$kabels,ID_NAN)
setkey(assets$moffen,ID_NAN)

headers =c ("Coo_X","Coo_Y","Spanningsniveau","BRONSYSTEEM","Isolatiemedium","file","DateAdded",
            "DateRemoved","Status_ID","ID_NAN","Voltage","Netvlak","Date_Length_ch","Length_ch","Coo_X_van","Coo_Y_van")

Validated.assets = rbind(
      merge (assets$kabels,ValidatieSet)[,headers[1:14],with=FALSE],
      merge (assets$moffen,ValidatieSet)[,headers[1:12],with=FALSE],fill=TRUE)

# Merge for all KLAK -------------------------
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_LS.Rda"))
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_MS.Rda"))

Validated.changed.storingen = rbind(
  merge (storingen$LS,ValidatieSet,by="ID_KLAK_Melding")[,headers,with=FALSE],
  merge (storingen$MS,ValidatieSet,by="ID_KLAK_Melding")[,headers,with=FALSE],fill=TRUE)

# Load the proxy data for assets and merge --------------------
load(paste0(settings$Input_Datasets,"/2. Input Datasets/1. AID KID proxy/AHA_Proxy_partial_data_assets.Rda")
          
Validated.changed.assets = rbind(
     merge (assets$LSkabels,ValidatieSet,by="ID_NAN")[,headers[3:16],with=FALSE],
     merge (assets$MSkabels,ValidatieSet,by="ID_NAN")[,headers[3:16],with=FALSE],
     merge (assets$LSmoffen,ValidatieSet,by="ID_NAN")[,headers[1:12],with=FALSE],
     merge (assets$MSmoffen,ValidatieSet,by="ID_NAN")[,headers[1:12],with=FALSE],fill=TRUE)

# Load the proxy for KLAK and merge -----------------------------
headers = c("ID_KLAK_Melding","Netsoort",
"Tijdstip_begin_storing","Datum","Coo_X","Coo_Y")
load(paste0(settings$Input_Datasets,
            "/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))
Validated.changed.storingen = rbind(
       merge (storingen$LS,ValidatieSet,by="ID_KLAK_Melding")[,headers,with=FALSE],
       merge (storingen$MS,ValidatieSet,by="ID_KLAK_Melding")[,headers,with=FALSE],fill=TRUE)
     
}
