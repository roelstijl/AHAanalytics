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
  config$timediff$max =  70 # Aantal dagen tussen storing en verwijdering assets
  config$vervdiff$min = -45 # Aantal dagen tussen storing en verwijdering assets
  config$vervdiff$max =  45 # Aantal dagen tussen storing en verwijdering assets
  
  config <<- config
  
  #developer parameters
  develop               <-  list()
  develop$countremoved  <-  0
  develop               <<- develop
  

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

# Set keys for different methods-----------------------
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
  

# Aanmaken klakgegevenstabel, for-loop over klakmeldingen, aanroepen proxyfunctie --------------------------
  for(voltage in c("LS","MS")){ 
    klaktabel    <- storingen[[voltage]]            #aanmaken tabel met klakmeldingen
    if (!exists("assetsl")) { assetsl <- list()}    #aanmaken tabel met gekoppelde assets
    for(klaknr in klaktabel$ID_KLAK_Melding[c(1:4)]){
      klak          = klaktabel[ID_KLAK_Melding==klaknr]
      klakmeldingen = storingen$KLAKMELDERS[as.list(klak$ID_Groep)]
      #switch functie invoegen voor overige methodes
      assetsl       = Proxy_PC_6(klak,klakmeldingen,voltage,assets,assetsl) 
      
      
      
      }
  } 
  develop <<- develop                                                        # wegschrijven developers parameters
  filename=paste0(settings$Analyse_Datasets,"/assetslPC",gsub(":",".",paste0(Sys.time())),".Rda")  # definiëren file van weg te schrijven assets
  save(assetsl,file=filename)
}

# Postcode 6 proxy ---------------------------------    
Proxy_PC_6 = function(klakl,klakmelders,voltage,assets,assetsl)
{
  # Kies de assets die voldoen aan de PC6
  # aanmaken koppeltabel
  switch(voltage,
  LS={
  assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = assets$LSkabels[klakl$PC_6]                                              # Zoeken op Postcode 6
  assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels")  # Aanroepen functie om tijdsverschillen e.d. te berekenen
  
  assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = assets$LSmoffen[klakl$PC_6] # Zoeken op Postcode 6
  assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen")
  },
  MS={
  assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = assets$MSkabels[klakl$PC_4] # Zoeken op Postcode 4
  assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = assets$MSmoffen[klakl$PC_4] # Zoeken op postcode 4
  }) 
  #Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
  
  
  return(assetsl)
}

# Hoofdleidingen proxy -------------------------------------

# Uitrekenen tijdsverschillen------------------------------------------------  
process.table = function(assetstb,klakl,assettype){ # je moet deze even fixen voor moffen + kabels
    if(sum(assetstb$Status_ID=="Removed"|assetstb$Status_ID=="Lengthch") ==0){ 
    assetstb <- "Geen verwijderde/veranderde assets gevonden"
    } else  
    {
    diff = 
      switch(assettype, 
      moffen = data.table(
        Adiff = assetstb$DateAdded   - klakl$Tijdstip_begin_storing,
        Rdiff = assetstb$DateRemoved - klakl$Tijdstip_begin_storing),
      kabels = data.table(
        Adiff = assetstb$DateAdded   - klakl$Tijdstip_begin_storing,
        Rdiff = assetstb$DateRemoved - klakl$Tijdstip_begin_storing, # Voor kabels ook lengte
        Ldiff = assetstb$DateLength_ch - klakl$Tijdstip_begin_storing) # Voor kabels ook lengte
      )
    #kopieren voor assets en moffen
    
    assetstb             <- cbind(assetstb,diff)
    switch(assettype, 
           moffen = {assetstb$in.timediff <- ((assetstb$Adiff > config$timediff$min) & (assetstb$Adiff < config$timediff$max) | 
                                              (assetstb$Rdiff > config$timediff$min) & (assetstb$Rdiff < config$timediff$max) )},
           kabels = {assetstb$in.timediff <- ((assetstb$Adiff > config$timediff$min) & (assetstb$Adiff < config$timediff$max) | 
                                              (assetstb$Rdiff > config$timediff$min) & (assetstb$Rdiff < config$timediff$max) |
                                              (assetstb$Ldiff > config$timediff$min) & (assetstb$Ldiff < config$timediff$max))}
          )
    }
    return(assetstb)
  
  
}

# Filteren resultaten ----------------------------------------------
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
