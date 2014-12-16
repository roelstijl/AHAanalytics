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
  
  # Configuration parameters and settings ---------------------------
  config = list()
  config$timediff$min = -30 # Aantal dagen tussen storing en verwijdering assets
  config$timediff$max = 70  # Aantal dagen tussen storing en verwijdering assets
  config <<- config
  
  # Load data if not available -----------------------------
  if (!exists("assets")) {
    cat("Importing data file \n"); tic()
    load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets.Rda"),envir = .GlobalEnv)
    load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo.Rda"),envir = .GlobalEnv)
    load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"),envir = .GlobalEnv)
    toc();
  }; 
    
  # Quick Fixes -------------------------------------
  try(setnames(storingen$LS,"PC_6.x","PC_6"),silent=T)
  storingen$LS[,PC_6:=gsub(" ","",storingen$LS$PC_6, fixed=TRUE)]
  storingen$MS[,PC_6:=gsub(" ","",storingen$MS$PC_6, fixed=TRUE)]
  storingen$LS$Tijdstip_begin_storing <- as.Date(storingen$LS$Tijdstip_begin_storing)
  storingen$MS$Tijdstip_begin_storing <- as.Date(storingen$MS$Tijdstip_begin_storing)
  
  #storingen$LS                                          <- data.frame(storingen$LS )   #Quick fix
  #storingen$MS                                          <- data.frame(storingen$MS )   #Quick fix
  
  # Omzetten assetset -------------------------------------------------------
  
  #assetstemp    <- assets
  #assets        <- list()
  #assets$moffen <- rbind(assetstemp$mof_LS,assetstemp$mof_MS)
  #assets$kabels <- rbind(assetstemp$kbl_LS,assetstemp$kbl_MS)
  
  #set keys for different methods
  switch(method,
         PC={
           setkey(assets$LSmoffen,PC_6)
           setkey(assets$MSmoffen,PC_4)
           setkey(assets$LSkabels,PC_6_van,PC_6_naar)
           setkey(assets$MSkabels,PC_4_van,PC_4_naar)
         },
         TOPO={
           setkey(assets$LSmoffen,ID_Hoofdleiding)
           #setkey(assets$MSmoffen,MS_Routenaam)
           setkey(assets$LSkabels,ID_Hoofdleiding)
           setkey(assets$MSkabels,Routenaam_MS)
         },
         XY={
           setkey(assets$LSmoffen,Coo_X,Coo_Y)
           setkey(assets$MSmoffen,Coo_X,Coo_Y)
           setkey(assets$LSkabels,Coo_X_naar,Coo_Y_naar,Coo_X_van,Coo_Y_van)
           setkey(assets$MSkabels,Coo_X_naar,Coo_Y_naar,Coo_X_van,Coo_Y_van)
         })
  setkey(storingen$KLAKMELDERS,ID_Groep)
  
# Loop over assettype -------------------------------------------------------
#   cat("")
#   voltage = substr(paste(assettype),1,2)
# #     assetklak= data.table(
# #       ID_KLAK_Melding        =character(), 
# #       Netcomponent           =character(), 
# #       Tijdstip_begin_storing = as.Date(character()),
# #       Gmu_Verwerking_Gereed  = as.Date(character()),
# #       PC_6                   = character(),
# #       ID_Groep               = character())
#     
#     cat(paste(assettype,voltage,nrow(storingen[[voltage]]),"\n"))

#aanmaken klakgegevenstabel, aanroepen proxyfunctie --------------------------
  for(voltage in c("LS","MS")){ 
    klaktabel    <- storingen[[voltage]]   #aanmaken tabel met klakmeldingen
    
    for(klaknr in klaktabel$ID_KLAK_Melding){
      klak          = klaktabel[ID_KLAK_Melding==klaknr]
      klakmeldingen = storingen$KLAKMELDERS[klak$ID_Groep]
      #sqitch functie invoegen voor overige methodes
      Proxy_PC_6(klak,klakmeldingen,voltage,assets) 
    
    }
  }
}

#  Postcode 6 proxy ---------------------------------    
Proxy_PC_6 = function(klakl,klakmelders,voltage,assets)
{
  # Kies de assets die voldoen aan de PC6
  # aanmaken koppeltabel
  if (!exists("assetsl")) { assetsl <<- list()}
  switch(voltage,
  LS={
  assetsl$LSkabels = assets$LSkabels[klakl$PC_6] # Verschil kabel/mof
  assetsl$LSmoffen = assets$LSmoffen[klakl$PC_6] # Verschil kabel/mof
  },
  MS={
  assetsl$MSkabels = assets$MSkabels[klakl$PC_6] # Verschil kabel/mof
  assetsl$MSmoffen = assets$MSmoffen[klakl$PC_6] # Verschil kabel/mof
  }) 
  #Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
   if(sum(assetl$Status_ID=="Removed"|assetl$Status_ID=="Lengthch") ==0 ){ 
   } else  
   {
     # Bereken de datum verschillen 
     assetsl$kabel = calc.date.diff(assetl,klakl,"kabels") # is een funtie die hieronder staat
     #idem voor moffen
     
     # Maak dataframe met mogelijk gevonden assets
     assets = 
     
     return(assetl)
   }
}

  
calc.date.diff = function(assetl,klakl,assettype){ # je moet deze even fixen voor moffen + kabels
    diff = 
      switch(moffen = data.table(
        a = assetl$DateAdded   - klakl$Tijdstip_begin_storing,
        b = assetl$DateRemoved - klakl$Tijdstip_begin_storing),
      kabels = data.table(
          a = assetl$DateAdded   - klakl$Tijdstip_begin_storing,
          b = assetl$DateRemoved - klakl$Tijdstip_begin_storing, # Voor kabels ook lengte
          c = assetl$DateLength_ch - klakl$Tijdstip_begin_storing) # Voor kabels ook lengte
      )
    #kopieren voor assets en moffen
    
    cbind(diff[1], mycol = na.omit(unlist(diff[-1])))
    
    assetl$in.timediff((assetl$Adiff > config$timediff$min) & (assetl$Adiff < config$timediff$max) | 
                         (assetl$Rdiff > config$timediff$min) & (assetl$Rdiff < config$timediff$max) |
                         (assetl$Ldiff > config$timediff$min) & (assetl$Ldiff < config$timediff$max) )
    return(assetl)
  
  
}

Proxy_filter = function()
{
  #Selecteer alleen LS kabels
  kabelsklak        <- kabelsklak[which(kabelsklak$Voltage %in% c("400V","Onbekend")),]
  
  #Selecteer verwijderde moffen die in de rondom de storing vervangen zijn
  LSkabelsklakhld <- kabelsklak[which((kabelsklak$Rdiffc==1 & kabelsklak$vervc ==1)|kabelsklak$Ldiffc ==1),]
  LSkabelsklakhld <- transform(LSkabelsklakhld, freq.KLAKmelding=ave(seq(nrow(LSkabelsklakhld)),ID_KLAK_Melding,FUN=length))
  LSkabelsklakhld <- LSkabelsklakhld[which(LSkabelsklakhld$freq.KLAKmelding<6),]
  table(table(LSkabelsklakhld$ID_KLAK_Melding))
  table(LSkabelsklakhld$Netcomponent)
}
