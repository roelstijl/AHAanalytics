AHA_Data_Batch_Processing = function(range)
{
# Settings ---------------------

# process the nettopo -----------------------
AHA_DATA_Correct_NRG_Corruption() # Corrects corruptions in the NRG data
AHA_Data_Import("Nettopologie","aansluitingengeotrace")
AHA_Data_Import("Nettopologie","EAN_LS_Aansluitingen")
AHA_Data_Import("Nettopologie","aansluitingen_stationinclbehuizing")
AHA_Data_Import("Nettopologie","MS_hoofdleidingen")
AHA_Data_Import("Nettopologie","MS_Stations")
processXY("EAN_LS_Aansluitingen","position",veld="Lokatie",folder="11. Nettopologie")
processPC6("EAN_LS_Aansluitingen_XY","punt",folder="11. Nettopologie")

# Processing of registered NAN-KLAK---------------
AHA_Data_Import("Validatie_data","Koppeling KLAK-NRG","Koppeling KLAK-NRG","save")

# KLAK/GISmutaties processing ----------------
AHA_Data_Import("KLAK","KLAK_KOPPEL_MELDING_GROEP","KLAK_KOPPEL_MELDING_GROEP","save")
AHA_Data_Import("KLAK","KLAK_LS","KLAK_LS","save","yes")
AHA_Data_Import("KLAK","KLAK_MS","KLAK_MS","save","yes")
AHA_Data_Import("GIS-mutaties","GISMUTATIE","GISMUTATIE","save")
  
# BARlog processing ---------------
AHA_Data_Import("BARlog","MH_NRG_LS_HLD","MH_NRG_LS_HLD","save")
AHA_Data_Import("BARlog","MH_NRG_LS_KABELS","MH_NRG_LS_KABELS","save")
AHA_Data_Import("BARlog","MH_NRG_LS_MOFFEN","MH_NRG_LS_MOFFEN","save")
AHA_Data_Import("BARlog","MH_NRG_MS_HLD","MH_NRG_MS_HLD","save")
AHA_Data_Import("BARlog","MH_NRG_MS_KABELS","MH_NRG_MS_KABELS","save")
AHA_Data_Import("BARlog","MH_NRG_MS_MOFFEN","MH_NRG_MS_MOFFEN","save")

# Add the XY coordinates in the same way NOR has them
processXY("MH_NRG_MS_KABELS","beginend",veld="Ligging")
processXY("MH_NRG_LS_KABELS","beginend",veld="Ligging")
processXY("MH_NRG_MS_MOFFEN","position",veld="Lokatie")
processXY("MH_NRG_LS_MOFFEN","position",veld="Lokatie")

# Add the PC_6 locations of the assets
processPC6("MH_NRG_LS_KABELS","van_naar")
processPC6("MH_NRG_MS_KABELS","van_naar")
processPC6("MH_NRG_MS_MOFFEN","punt")
processPC6("MH_NRG_LS_MOFFEN","punt")

# Create a set for the NOR coupling later

# Add the dates etc
AHA_Data_BAR_Log()
  
# NOR processing ------------------
NORdate = "1501"
AHA_Data_Import("NOR",paste0("ELCVERBINDINGEN_",NORdate),"ELCVERBINDINGEN","save")
AHA_Data_Import("NOR",paste0("ELCVERBINDINGSDELEN_",NORdate),"ELCVERBINDINGSDELEN","save")
AHA_Data_Import("NOR",paste0("ELCVERBINDINGSKNOOPPUNTEN_",NORdate),"ELCVERBINDINGSKNOOPPUNTEN","save")

# Convert into log
AHA_Data_NOR_Log("ELCVERBINDINGEN",backups=F)
AHA_Data_NOR_Log("ELCVERBINDINGSDELEN",backups=F)
AHA_Data_NOR_Log("ELCVERBINDINGSKNOOPPUNTEN",backups=F)

# Correct missing PC Naar
processPC6("masterdataset_ELCVERBINDINGSDELEN","naar",paste0(settings$Input_Datasets,"/6. NOR"))
processPC6("changes_ELCVERBINDINGSDELEN","naar",paste0(settings$Input_Datasets,"/6. NOR"))

# Post processing
AHA_Data_NOR_Log_Postprocessing()

# Preprocessing for Proxi
AHA_Data_KA_Proxy_Preprocessing("assetsBAR","assetsNOR")

# Tableau output ---------------------------
# Add the XY coordinates in a spatial file
processXY("MH_NRG_LS_KABELS","lines",,veld="Ligging")
processXY("MH_NRG_MS_KABELS","lines",,veld="Ligging")

# Create the tableau output for the visuals
Tableau_Create_Polygons(fileout="MH_NRG_MS_KABELS_Geospatial_Tableau",sources="spd",combine = FALSE)
Tableau_Create_Polygons(fileout="MH_NRG_LS_KABELS_Geospatial_Tableau",sources="spd",combine = FALSE)
}

BAR_HLD_Subset= function(){
  load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_KABELS_XY_PC6.Rda"))
  try(setnames(mindataset,"Ls_Verbinding","ID_Verbinding"))
  BAR_LS_HLD = mindataset[,list(ID_NAN,ID_Hoofdleiding,ID_Verbinding)]
  
  load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_MS_KABELS_XY_PC6.Rda"))
  try(setnames(mindataset,"ID_MS_HLD","ID_Verbinding"))
  BAR_MS_HLD = mindataset[,list(ID_NAN,ID_Hoofdleiding,ID_Verbinding)]
  
  save(BAR_LS_HLD,BAR_MS_HLD,file=paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_KABELS_HLD_Sample.Rda"))
}