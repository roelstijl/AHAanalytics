AHA_Data_Batch_Processing = function(range)
{
# BARlog processing ---------------
mode ="shiny"
AHA_Data_Import("BARlog","MH_NRG_LS_HLD","MH_NRG_LS_HLD",mode)
AHA_Data_Import("BARlog","MH_NRG_LS_KABELS","MH_NRG_LS_KABELS",mode)
AHA_Data_Import("BARlog","MH_NRG_LS_MOFFEN","MH_NRG_LS_MOFFEN",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_HLD","MH_NRG_MS_HLD",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_KABELS","MH_NRG_MS_KABELS",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_MOFFEN","MH_NRG_MS_MOFFEN",mode)
  
# NOR processing ------------------
  folder="NOR"
  setfolder     = list.files(settings$Bron_Datasets,pattern=folder)[1]; 
  dir.create(paste0(settings$Ruwe_Datasets,"/",setfolder), showWarnings = FALSE)
  a=list.files(paste0(settings$Bron_Datasets,"/",setfolder))
  all = substr(a[1:95], 17, 20)
  all = substr(a[range], 17, 20)

  for (n in all) {
  AHA_Data_Import("NOR",paste0("ELCVERBINDINGEN_",n),"ELCVERBINDINGEN",mode)
   AHA_Data_Import("NOR",paste0("ELCVERBINDINGSDELEN_",n),"ELCVERBINDINGSDELEN",mode)
   AHA_Data_Import("NOR",paste0("ELCVERBINDINGSKNOOPPUNTEN_",n),"ELCVERBINDINGSKNOOPPUNTEN",mode
  }


source('C:/Dropbox/1. BearingPoint/1. Billable projects/2. Alliander/3. Asset health/AHAanalytics/AHA_Data_NOR_Log.R')
AHA_Data_NOR_Log("ELCVERBINDINGEN")
AHA_Data_NOR_Log("ELCVERBINDINGSDELEN")
AHA_Data_NOR_Log("ELCVERBINDINGSKNOOPPUNTEN")
}