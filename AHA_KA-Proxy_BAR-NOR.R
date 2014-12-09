AHA_Proxy_KA_BAR_NOR = 
  function(method,assettypes=c("LSkabels","MSkabels","LSmoffen","MSmoffen")) 
  {
    # This function calculates the asset id - klak id proxy for the asset health analytics project
    # Data should be loaded using the AHA_Proxy_Dataset function (global environment)
    #
    # Input:
    # Method refers to the proxy method used, GEO, PC, HLD or OLD
    # Asset refers to the asset type, e.g. moffen, kabels or both
    # Voltage refers to the voltage to use, can be MS, LS or both

# Load data if not available
if (!exists("assets")) {
  cat("Importing data file \n");tic()
  load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets.Rda"),envir = .GlobalEnv)
  load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo.Rda"),envir = .GlobalEnv)
  load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"),envir = .GlobalEnv);toc()
}

# Omzetten assetset -------------------------------------------------------

#assetstemp    <- assets
#assets        <- list()
#assets$moffen <- rbind(assetstemp$mof_LS,assetstemp$mof_MS)
#assets$kabels <- rbind(assetstemp$kbl_LS,assetstemp$kbl_MS)

#set keys for different methods
switch(method,
       "PC"={
         setkey(assets$LSmoffen,PC_6)
         setkey(assets$MSmoffen,PC_4)
         setkey(assets$LSkabels,PC_6_van,PC_6_naar)
         setkey(assets$MSkabels,PC_4_van,PC_4_naar)
       },
       "TOPO"={
         setkey(assets$LSmoffen,ID_Hoofdleiding)
         #setkey(assets$MSmoffen,MS_Routenaam)
         setkey(assets$LSkabels,ID_Hoofdleiding)
         setkey(assets$MSkabels,Routenaam_MS)
       },
       "XY"={
         setkey(assets$LSmoffen,Coo_X,Coo_Y)
         setkey(assets$MSmoffen,Coo_X,Coo_Y)
         setkey(assets$LSkabels,Coo_X_naar,Coo_Y_naar,Coo_X_van,Coo_Y_van)
         setkey(assets$MSkabels,Coo_X_naar,Coo_Y_naar,Coo_X_van,Coo_Y_van)
       })

# Loop over assettype --------------------------------------------------
for (assettype in assettypes) {
      voltage = substr(paste(assettype),1,2)
      assetklak <- assets[[assettype]][0,] #aanmaken koppeltabel
      assetklak[,ID_KLAK_Melding:=NA]         ; assetklak[,ID_KLAK_Melding:=as.integer(ID_KLAK_Melding)]
      assetklak[,Netcomponent:=NA]            ; assetklak[,Netcomponent:=as.character(Netcomponent)]
      assetklak[,Tijdstip_begin_storing:=NA]  ; assetklak[,Tijdstip_begin_storing:=as.character(Tijdstip_begin_storing)]
      assetklak[,Gmu_Verwerking_Gereed:=NA]   ; assetklak[,Gmu_Verwerking_Gereed:=as.character(Gmu_Verwerking_Gereed)]
      assetklak[,PC_6:=NA]                    ; assetklak[,PC_6:=as.character(PC_6)]
      assetklak[,ID_Groep:=NA]                ; assetklak[,ID_Groep:=as.double(ID_Groep)]
      
      print(paste(assettype,voltage,nrow(storingen[[voltage]])))
      for(i in 1:nrow(storingen[[voltage]])){
              }
} #einde for-loop over assettypes

}  #einde functie
    
    
    

