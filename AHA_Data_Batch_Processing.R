AHA_Data_Batch_Processing = function(range)
{
# Settings ---------------------
mode ="save"

# Processing of registered NAN-KLAK---------------
AHA_Data_Import("Validatie_data","Koppeling KLAK-NRG","Koppeling KLAK-NRG",mode)

# KLAK/GISmutaties processing ----------------
AHA_Data_Import("KLAK","KLAK_KOPPEL_MELDING_GROEP","KLAK_KOPPEL_MELDING_GROEP",mode)
AHA_Data_Import("KLAK","KLAK_LS","KLAK_LS",mode,"yes")
AHA_Data_Import("KLAK","KLAK_MS","KLAK_MS",mode,"yes")
AHA_Data_Import("GIS-mutaties","GISMUTATIE","GISMUTATIE",mode)
  
# BARlog processing ---------------
AHA_Data_Import("BARlog","MH_NRG_LS_HLD","MH_NRG_LS_HLD",mode)
AHA_Data_Import("BARlog","MH_NRG_LS_KABELS","MH_NRG_LS_KABELS",mode)
AHA_Data_Import("BARlog","MH_NRG_LS_MOFFEN","MH_NRG_LS_MOFFEN",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_HLD","MH_NRG_MS_HLD",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_KABELS","MH_NRG_MS_KABELS",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_MOFFEN","MH_NRG_MS_MOFFEN",mode)

# Add the XY coordinates in the same way NOR has them
processXY("MH_NRG_MS_KABELS","beginend",atype="kabels")
processXY("MH_NRG_LS_KABELS","beginend",atype="kabels")
processXY("MH_NRG_MS_MOFFEN","position",atype="moffen")
processXY("MH_NRG_LS_MOFFEN","position",atype="moffen")

# Add the XY coordinates in a spatial file
processXY("MH_NRG_LS_KABELS","lines",atype="kabels")
processXY("MH_NRG_MS_KABELS","lines",atype="kabels")
processXY("MH_NRG_MS_MOFFEN","points",atype="moffen")
processXY("MH_NRG_LS_MOFFEN","points",atype="moffen")

# Add the PC_6 locations of the assets
processPC6("MH_NRG_LS_KABELS","van_naar")
processPC6("MH_NRG_MS_KABELS","van_naar")
processPC6("MH_NRG_MS_MOFFEN","punt")
processPC6("MH_NRG_LS_MOFFEN","punt")

# Add the dates etc
AHA_Data_BAR_Log_Postprocessing()
  
# NOR processing ------------------
mode = "save"
AHA_Data_Import("NOR",paste0("ELCVERBINDINGEN_"),"ELCVERBINDINGEN",mode)
AHA_Data_Import("NOR",paste0("ELCVERBINDINGSDELEN_"),"ELCVERBINDINGSDELEN",mode)
AHA_Data_Import("NOR",paste0("ELCVERBINDINGSKNOOPPUNTEN_"),"ELCVERBINDINGSKNOOPPUNTEN",mode)

# Convert into log
AHA_Data_NOR_Log("ELCVERBINDINGEN")
AHA_Data_NOR_Log("ELCVERBINDINGSDELEN")
AHA_Data_NOR_Log("ELCVERBINDINGSKNOOPPUNTEN")

# Post processing
AHA_Data_NOR_Log_Postprocessing()

# Preprocessing for Proxi
AHA_Data_KA_Proxy_Preprocessing_NOR()
}

