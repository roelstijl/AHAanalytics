AHA_Proxi_KA_Postprocessing = function(PC_4) 
{
# This function will plot the data files used in project AHA onto google maps
# Input is a series of PC4 areas or a single PC4 area PC_4 = 6810:6823 for Arnhem
# Method include proxy 
# Load the required data files -------------------------------------
load(paste0(settings$Input_Datasets,"/23. Validatie_data/Validatie koppelingen.Rda"))

load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets.Rda"))
# Coo_X van missing due to bug
assets$kabels = rbind(assets$LSkabels,assets$MSSkabels,fill=TRUE)[,list(Status_ID,DateLength_ch,ID_NAN,Voltage,Length_ch,DateRemoved,DateAdded)]#,Coo_X_van,Coo_Y_van,PC_6_van,Coo_X_naar,Coo_Y_naar,PC_6_naar)]
assets$moffen = rbind(assets$LSmoffen,assets$MSmoffen,fill=TRUE)[,list(Status_ID,ID_NAN,Voltage,DateRemoved,DateAdded,Coo_X,Coo_Y,PC_4,PC_6)]
assets$all    = rbind(assets$moffen, assets$kabels,fill=TRUE)

load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))
storingen$all = rbind(storingen$LS,storingen$MS,fill=TRUE)[,list(Datum,Status,ID_KLAK_Melding,Getroffen_klanten_totaal,Duur_onderbreking,Datum_Eerste_Ontwerp,Klacht,Netcomponent,Mof,Veroorzaker,Tijdstip_definitief_einde)]

# Start the merging
ValidatieSet[,inNOR:=(ID_NAN %in% assets$all$ID_NAN)]
ValidatieSet[,inKLAK:=(ID_KLAK_Melding %in% storingen$all$ID_KLAK_Melding)]
ValidatieSet[,inKLAKMelders:=(ID_KLAK_Melding %in% storingen$KLAKMELDERS$ID_KLAK_Melding)]
ValidatieSet[,inAllSystems := inNOR&inKLAK&inKLAKMelders]

molten = melt(ValidatieSet, id.vars = c("Regio","Datum","ID_KLAK_Melding","ID_NAN"),measure.vars=c("inNOR","inKLAK","inKLAKMelders","inAllSystems"))

ggplot(molten,aes(variable,fill=paste(Regio,value))) + geom_bar(position="dodge",colour="black")



# Put it on a map


}

NORanalytics = function()
  
{
  load("C:/Datasets/AHAdata/2. Input Datasets/6. NOR/masterdataset_ELCVERBINDINGSDELEN.Rda")
  a = ValidatieSet$ID_NAN %in% masterdataset$ID_NAN
  barplot(table(a))  
  
  setkey(masterdataset,ID_NAN)
  setkey(ValidatieSet,ID_NAN)
  ValidatieSet=unique(ValidatieSet)
  masterdataset = unique(masterdataset)
  masterdataset[masterdataset$ID_NAN %in% ValidatieSet$ID_NAN]
  
  
  load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo.Rda"))
  
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_6_Spatial.Rda"))
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_4_Spatial.Rda"))
  
}

