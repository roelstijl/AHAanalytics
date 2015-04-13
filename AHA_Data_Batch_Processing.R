# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Function in this file perform batch processing of the data

AHA_Data_Batch_Processing = function(){
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Function used to batch process all the data processing functions
# Input:
# none

# Other data ---------------------------
manualinput <- readline("Convert source files to raw files manually y/n?\n\n")

if (manualinput == "y")
AHA_Data_Import() # load additional files
  
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

# KLAK processing ----------------
KLAK_MS_fix_CooXY()

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

# Add the dates etc
AHA_Data_BAR_Log()

# Tableau output
AHA_Data_KA_Proxy_Preprocessing("assetsBAR")
  
# NOR processing ------------------
NORdate = ""
AHA_Data_Import("NOR",paste0("ELCVERBINDINGEN_",NORdate),"ELCVERBINDINGEN","save")
AHA_Data_Import("NOR",paste0("ELCVERBINDINGSDELEN_",NORdate),"ELCVERBINDINGSDELEN","save")
AHA_Data_Import("NOR",paste0("ELCVERBINDINGSKNOOPPUNTEN_",NORdate),"ELCVERBINDINGSKNOOPPUNTEN","save")

# Convert into log
AHA_Data_NOR_Log("ELCVERBINDINGEN",backups=F)
AHA_Data_NOR_Log("ELCVERBINDINGSDELEN",backups=F)
AHA_Data_NOR_Log("ELCVERBINDINGSKNOOPPUNTEN",backups=F)

# Correct missing PC Naar
processPC6("masterdataset_ELCVERBINDINGSDELEN","van_naar_NOR",paste0(settings$Input_Datasets,"/6. NOR"))
processPC6("masterdataset_ELCVERBINDINGSKNOOPPUNTEN","punt_NOR",paste0(settings$Input_Datasets,"/6. NOR"))

# Post processing
AHA_Data_NOR_Log_Postprocessing()

# Preprocessing for Proxi
AHA_Data_KA_Proxy_Preprocessing("assetsNOR")

# Tableau output ---------------------------

Save_Tableau_assets("BAR","BAR_Tableau")
Save_Tableau_assets("NOR","NOR_Tableau")

# Add the XY coordinates in a spatial file
processXY("MH_NRG_LS_KABELS","lines",,veld="Ligging")
processXY("MH_NRG_MS_KABELS","lines",,veld="Ligging")

# Create the tableau output for the visuals
Tableau_Create_Polygons(fileout="MH_NRG_MS_KABELS_Geospatial_Tableau",sources="spd",combine = FALSE)
Tableau_Create_Polygons(fileout="MH_NRG_LS_KABELS_Geospatial_Tableau",sources="spd",combine = FALSE)
}

BAR_HLD_Subset= function(){
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Function creates a sample of the BAR data
# Input:
# none
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_KABELS_XY_PC6.Rda"))
try(setnames(mindataset,"Ls_Verbinding","ID_Verbinding"))
BAR_LS_HLD = mindataset[,list(ID_NAN,ID_Hoofdleiding,ID_Verbinding)]

load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_MS_KABELS_XY_PC6.Rda"))
try(setnames(mindataset,"ID_MS_HLD","ID_Verbinding"))
BAR_MS_HLD = mindataset[,list(ID_NAN,ID_Hoofdleiding,ID_Verbinding)]

save(BAR_LS_HLD,BAR_MS_HLD,file=paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_KABELS_HLD_Sample.Rda"))
}

KLAK_MS_fix_CooXY = function(){
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Function converts the coordinates from lon lat to RDS
# Input:
# none
load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK MS en klant melding.Rda"))
Convert_Coordinate_System(mindataset)
save(mindataset,file=paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK MS en klant melding_XY.Rda"))
}
