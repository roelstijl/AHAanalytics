AHA_Data_Batch_Processing = function(range)
{
# Settings ---------------------
mode ="save"

# KLAK/GISm processing ----------------
AHA_Data_Import("KLAK","KLAK_KOPPEL_MELDING_GROEP","KLAK_KOPPEL_MELDING_GROEP",mode)
AHA_Data_Import("KLAK","KLAK_LS","KLAK_LS",mode)
AHA_Data_Import("KLAK","KLAK_MS","KLAK_MS",mode)
AHA_Data_Import("GIS-mutaties","GISMUTATIE","GISMUTATIE",mode)
  
# BARlog processing ---------------
AHA_Data_Import("BARlog","MH_NRG_LS_HLD","MH_NRG_LS_HLD",mode)
AHA_Data_Import("BARlog","MH_NRG_LS_KABELS","MH_NRG_LS_KABELS",mode)
AHA_Data_Import("BARlog","MH_NRG_LS_MOFFEN","MH_NRG_LS_MOFFEN",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_HLD","MH_NRG_MS_HLD",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_KABELS","MH_NRG_MS_KABELS",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_MOFFEN","MH_NRG_MS_MOFFEN",mode)

processXY("MH_NRG_MS_KABELS","beginend")
# processXY("MH_NRG_LS_KABELS","beginend")
processXY("MH_NRG_MS_MOFFEN","position")
processXY("MH_NRG_LS_MOFFEN","position")
  
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
   AHA_Data_Import("NOR",paste0("ELCVERBINDINGSKNOOPPUNTEN_",n),"ELCVERBINDINGSKNOOPPUNTEN",mode)
  }


source('C:/Dropbox/1. BearingPoint/1. Billable projects/2. Alliander/3. Asset health/AHAanalytics/AHA_Data_NOR_Log.R')
AHA_Data_NOR_Log("ELCVERBINDINGEN")
AHA_Data_NOR_Log("ELCVERBINDINGSDELEN")
AHA_Data_NOR_Log("ELCVERBINDINGSKNOOPPUNTEN")
}

processXY = function(file,mode) 
{load(paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,".Rda"));cat("starting\n")
 mindataset = cbind(mindataset,AHA_Data_BAR_GEOMETRY(mindataset$Ligging,mode))
 mindataset[,Ligging:=NULL]; cat("saving\n")
 save(mindataset,file=paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_XY.Rda"))
}