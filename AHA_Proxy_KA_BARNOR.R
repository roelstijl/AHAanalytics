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
  config$sdiff$max    =   2 # Aantal dagen tussen storing en verwijdering assets

  config <<- config
  
  #developer parameters
  develop                        <<-  list()
  develop$countremoved$LSkabels  <<-  0
  develop$countremoved$LSmoffen  <<-  0
  develop$countremoved$MSkabels  <<-  0
  develop$countremoved$MSmoffen  <<-  0

  
  
  #progress bar
  pb  <- txtProgressBar()


# Load data if not available -----------------------------
  if (!exists("assets")) {
    cat("Importing data file \n"); tic()
    load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets.Rda"),envir = .GlobalEnv)
    load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo.Rda"),envir = .GlobalEnv)
    load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"),envir = .GlobalEnv)
    toc();
  }; 

# Quick Fixes -------------------------------------
  storingen$LS[,PC_6:=gsub(" ","",storingen$LS$PC_6, fixed=TRUE)]  #verwijderen spaties uit postcodes
  storingen$MS[,PC_6:=gsub(" ","",storingen$MS$PC_6, fixed=TRUE)]  #verwijderen spaties uit postcodes
  storingen$LS$Tijdstip_begin_storing  <- as.Date(storingen$LS$Tijdstip_begin_storing) #converteren tijdstippen naar datumnotatie
  storingen$MS$Tijdstip_begin_storing  <- as.Date(storingen$MS$Tijdstip_begin_storing) #converteren tijdstippen naar datumnotatie
  storingen$LS$Datum_Verwerking_Gereed <- as.Date(storingen$LS$Datum_Verwerking_Gereed) #converteren tijdstippen naar datumnotatie
  storingen$MS$Datum_Verwerking_Gereed <- as.Date(storingen$MS$Datum_Verwerking_Gereed) #converteren tijdstippen naar datumnotatie
  names(assets$LSkabels)[3:4]          <- c("Coo_X_van","Coo_Y_van")
  names(assets$MSkabels)[3:4]          <- c("Coo_X_van","Coo_Y_van")

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
  

assetsltb <- list()

# Aanmaken klakgegevenstabel, for-loop over klakmeldingen, aanroepen proxyfunctie --------------------------
for(voltage in c("LS","MS")){ 
  cat(voltage)
  titlepb <- paste("Proxy",method,voltage)

  klaktabel    <- storingen[[voltage]]                  # aanmaken tabel met klakmeldingen
    if (!exists("assetsltb")) { assetsltb <- list()}    # aanmaken tabel met gekoppelde assets
    counter    <- 0
    for(klaknr in klaktabel$ID_KLAK_Melding[1:50]){
      klak          <- klaktabel[ID_KLAK_Melding==klaknr]
      klakmeldingen <- storingen$KLAKMELDERS[as.list(klak$ID_Groep)]
      #switch functie invoegen voor overige methodes
      assetsltb     <- Proxy_PC_6(klak,klakmeldingen,voltage,assets,assetsltb) 
      counter       <- counter + 1; setTxtProgressBar(pb, counter/nrow(klaktabel),title=titlepb)
     }
  develop$storingen[voltage] <<- counter
  } 
  filename=paste0(settings$Analyse_Datasets,"/assetslPC",gsub(":",".",paste0(Sys.time())),".Rda")  # definiëren file van weg te schrijven assets
  save(assetsltb,file=filename)
  close(pb);
}

# Postcode 6 proxy ---------------------------------    
Proxy_PC_6 = function(klakl,klakmelders,voltage,assets,assetsl)
{
  # Kies de assets die voldoen aan de PC6
  # teruggeven koppeltabel
  switch(voltage,
  LS={
  assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = assets$LSkabels[klakl$PC_6]                                              # Zoeken op Postcode 6
  assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels")  # Aanroepen functie om tijdsverschillen e.d. te berekenen
  
  assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = assets$LSmoffen[klakl$PC_6]                                              # Zoeken op Postcode 6
  assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen")  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
  },
  MS={
  assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = assets$MSkabels[klakl$PC_4] # Zoeken op Postcode 4
  assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels")  # Aanroepen functie om tijdsverschillen e.d. te berekenen
  
  assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = assets$MSmoffen[klakl$PC_4] # Zoeken op postcode 4
  assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen")  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
  }) 
  
  
  
  return(assetsl)
}

# Hoofdleidingen proxy -------------------------------------

# Uitrekenen tijdsverschillen, wel of niet verwijderd------------------------------------------------  
process.table = function(assetstb,klakl,assettype){
      #bugfixing regels:
      #try(print(paste(klakl$ID_KLAK_Melding,length(assetstb),nrow(assetstb),class(assetstb),assettype)))
      #try(print(paste(sum(complete.cases(assetstb$ID_unique)), nrow(assetstb),class(assetstb),assettype)))
      #if(sum(class(assetstb)              ==   "character")){print(head(assetstb))}
    try({ 
    if (sum(complete.cases(assetstb$ID_unique)) ==   0          ){}else{            # Verwijder rijen met enkel NA's
      
    if (sum(assetstb$Status_ID=="Removed"|assetstb$Status_ID=="Lengthch") ==0){assetstb <- assetstb[0,]} #Indien alleen toegevoegde assets -> verwijder alle rijen
    else  {
    #uitrekenen tijdsverschillen met storing
    if (is.na(klakl$Datum_Verwerking_Gereed)){     #calculate differences between the changes in the NOR/BARlog and the time of start interruption, if time of processing is NA
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
    }else{                                         #calculate differences between the moment between the changes in the NOR/BARlog and the time that the interruption was processed in NRG
      
      diff = 
        switch(assettype, 
               moffen = data.table(
                 Adiff = assetstb$DateAdded   - klakl$Datum_Verwerking_Gereed,
                 Rdiff = assetstb$DateRemoved - klakl$Datum_Verwerking_Gereed),
               kabels = data.table(
                 Adiff = assetstb$DateAdded   - klakl$Datum_Verwerking_Gereed,
                 Rdiff = assetstb$DateRemoved - klakl$Datum_Verwerking_Gereed, 
                 Ldiff = assetstb$DateLength_ch - klakl$Datum_Verwerking_Gereed) # Voor kabels ook lengte
        )
    }
    #Dicht genoeg op storing?
    assetstb             <- cbind(assetstb,diff)
    switch(assettype, 
           moffen = {if ("Netspanning_tpv_storing" %in% names(klakl)){develop$countremoved$MSmoffen <<- develop$countremoved$MSmoffen + 1} else {develop$countremoved$LSmoffen <<- develop$countremoved$LSmoffen +1}},
           kabels = {if ("Netspanning_tpv_storing" %in% names(klakl)){develop$countremoved$MSkabels <<- develop$countremoved$MSkabels + 1} else {develop$countremoved$LSkabels <<- develop$countremoved$LSkabels +1}}
    )
    switch(assettype, 
           moffen = {assetstb$in.timediff <- ((assetstb$Adiff > config$timediff$min) & (assetstb$Adiff < config$timediff$max) | 
                                              (assetstb$Rdiff > config$timediff$min) & (assetstb$Rdiff < config$timediff$max) )},
           kabels = {assetstb$in.timediff <- ((assetstb$Adiff > config$timediff$min) & (assetstb$Adiff < config$timediff$max) | 
                                              (assetstb$Rdiff > config$timediff$min) & (assetstb$Rdiff < config$timediff$max) |
                                              (assetstb$Ldiff > config$timediff$min) & (assetstb$Ldiff < config$timediff$max))}
          )
    switch(assettype, 
           moffen = {
             tdiff       <- sapply(assetstb$DateAdded,function(x){x-assetstb$DateRemoved})  # Uitrekenen tijdsverschillen tussen tijd van verwijdering en toevoeging
             
             xdiff       <- sapply(assetstb$Coo_X,function(x){x-assetstb$Coo_X})            # Uitrekenen afstandsverschillen in horizontale richting
             
             ydiff       <- sapply(assetstb$Coo_Y,function(x){x-assetstb$Coo_Y})            # Uitrekenen afstandsverschillen in verticale richting
             
             sdiff       <- xdiff^2+ydiff^2                                                 # Uitrekenen afstandsverschillen totaal
             
             matrix      <- matrix((sdiff < config$sdiff$max) & (tdiff >  config$vervdiff$min & tdiff < config$vervdiff$max),ncol=max(nrow(tdiff),1))
             diag(matrix)<- FALSE                                                           #Zorg ervoor dat assets niet door zichzelf vervangen kunnen worden
             
             in.verv <- rowSums()
    
             assetstb             <- cbind(assetstb, in.verv)
             assetstb$koppelc     <- assetstb$in.timediff & assetstb$in.verv
           },
           kabels={
             tdiff <- sapply(assetstb$DateAdded,function(x){x-assetstb$DateRemoved})
             
             xdiffvv <- sapply(assetstb$Coo_X_van, function(x){x-assetstb$Coo_X_van})
             xdiffvn <- sapply(assetstb$Coo_X_van, function(x){x-assetstb$Coo_X_naar})
             xdiffnv <- sapply(assetstb$Coo_X_naar,function(x){x-assetstb$Coo_X_van})
             xdiffnn <- sapply(assetstb$Coo_X_naar,function(x){x-assetstb$Coo_X_naar})
             
             ydiffvv <- sapply(assetstb$Coo_Y_van,function(x){x-assetstb$Coo_Y_van})
             ydiffvn <- sapply(assetstb$Coo_Y_van,function(x){x-assetstb$Coo_Y_naar})
             ydiffnv <- sapply(assetstb$Coo_Y_naar,function(x){x-assetstb$Coo_Y_van})
             ydiffnn <- sapply(assetstb$Coo_Y_naar,function(x){x-assetstb$Coo_Y_naar})
             
             sdiffvv <- xdiffvv^2+ydiffvv^2
             sdiffvn <- xdiffvn^2+ydiffvn^2
             sdiffnv <- xdiffnv^2+ydiffnv^2
             sdiffnn <- xdiffnn^2+ydiffnn^2
             
             in.verv <-  ifelse(assetstb$Length_ch < 0 ,1,
                         ifelse(assetstb$Length_ch >= 0,0,
                         ifelse(assetstb$Status_ID == "Active",0,1
                               
                             
                             )))
             
           }
    )
    }}},silent=F)
    
   
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
