AHA_Proxy_KA_BAR_NOR = 
function(method,assettypes=c("LSkabels","MSkabels","LSmoffen","MSmoffen")) 
{ # This function calculates the asset id - klak id proxy for the asset health analytics project
  # Data should be loaded using the AHA_Proxy_Dataset function (global environment)
  #
  # Input:
  # Method refers to the proxy method used, GEO, PC, HLD or OLD
  # Asset refers to the asset type, e.g. moffen, kabels or both
  # Voltage refers to the voltage to use, can be MS, LS or both
  
# Configuration parameters and settings ---------------------------
  config = list()
  config$timediff$min = -30 # Aantal dagen tussen storing en verwijdering/toevoeging assets
  config$timediff$max =  70 # Aantal dagen tussen storing en verwijdering/toevoeging assets
  config$vervdiff$min = -45 # Aantal dagen tussen verwijderde en toegevoegde asset
  config$vervdiff$max =  45 # Aantal dagen tussen verwijderde en toegevoegde asset
  config$sdiff$max    =   2 # Afstand tussen verwijderde en toegevoegde asset
  config$szoek$LS     = 200 # Afstand waarover assets gezocht worden bij XY-proxy
  config$szoek$MS     =2000 # Afstand waarover assets gezocht worden bij XY-proxy


  try(config <<- config)
  
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
  if(names(assets$LSkabels)[1]=="ID_Hoofdleiding"){setnames(assets$LSkabels,c("ID_Verbinding","ID_Hoofdleiding"),c("ID_Hoofdleiding","ID_Verbinding"))}
  names(assets$LSkabels)[3:4]          <- c("Coo_X_van","Coo_Y_van")
  names(assets$MSkabels)[3:4]          <- c("Coo_X_van","Coo_Y_van")
  nettopo$EAN_koppel$ID_Hoofdleiding_LS<- as.character(nettopo$EAN_koppel$ID_Hoofdleiding_LS) #zorgen dat hoofdleidingen characters zijn
  storingen$LS$Coo_X                   <- as.numeric(gsub(",",".",storingen$LS$Coo_X))
  storingen$LS$Coo_Y                   <- as.numeric(gsub(",",".",storingen$LS$Coo_Y))
  storingen$MS$Coo_X                   <- as.numeric(gsub(",",".",storingen$MS$Coo_X))
  storingen$MS$Coo_Y                   <- as.numeric(gsub(",",".",storingen$MS$Coo_Y))

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
           setkey(assets$MSmoffen,Routenaam_MS)
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
    for(klaknr in klaktabel$ID_KLAK_Melding){
      klak          <- klaktabel[ID_KLAK_Melding==klaknr]
      klakmeldingen <- storingen$KLAKMELDERS[as.list(klak$ID_Groep)]
      assetsltb <- switch(method,
             PC   = Proxy_PC_6(klak,klakmeldingen,voltage,assets,assetsltb),
             TOPO = Proxy_TOPO(klak,klakmeldingen,voltage,assets,assetsltb,nettopo),
             XY   = Proxy_XY  (klak,klakmeldingen,voltage,assets,assetsltb)
        )
       
      counter       <- counter + 1; setTxtProgressBar(pb, counter/nrow(klaktabel),title=titlepb)
     }
  develop$storingen[voltage] <<- counter
  } 
  filename=paste0("C:/Data/AHAdata/3. Analyse Datasets","/assetsl",method,gsub(":",".",paste0(Sys.time())),".Rda")  # definiëren file van weg te schrijven assets
  #filename=paste0(settings$Analyse_Datasets,"/assetsl",method,gsub(":",".",paste0(Sys.time())),".Rda")  # definiëren file van weg te schrijven assets
  save(assetsltb,file=filename)
  close(pb);
}

# Postcode 6 proxy ---------------------------------    
Proxy_PC_6 = function(klakl,klakmelders,voltage,assets,assetsl)
{
  # Postcodelijst samenstellen
    # Kies de assets die voldoen aan de PC6
  # teruggeven koppeltabel
  switch(voltage,
  LS={
  PClijst = unique(c(klakl$PC_6,klakmelders$PC_6))
  PCdt    = data.table(PC_6_naar=PClijst)
  
  # Zoeken op Postcode 6 van, voeg andere assets die op PC6_naar matchen ook toe
  assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = assets$LSkabels[which(assets$LSkabels$PC_6_van %in% PClijst | assets$LSkabels$PC_6_naar %in% PClijst)]                                                 
  assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels")  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
  
  
  assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = assets$LSmoffen[J(PClijst)]                                                 # Zoeken op Postcode 6
  assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen")  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
  },
  MS={
  PClijst = list(unique(c(klakl$PC_4,substr(klakmelders$PC_6,1,4))))
    
  assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = assets$MSkabels[PClijst] # Zoeken op Postcode 4
  assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels")  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
  
  assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = assets$MSmoffen[PClijst] # Zoeken op postcode 4
  assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen")  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
  }) 
  
  
  
  return(assetsl)
}

# Hoofdleidingen proxy -------------------------------------
Proxy_TOPO = function(klakl,klakmelders,voltage,assets,assetsl,nettopo)
{
  # Vind HLD bij klakmelders
  # Kies de assets die voldoen aan de PC6
  # teruggeven koppeltabel
  switch(voltage,
         LS={
           PCs        = c(klakl$PC_6,klakmelders$PC_6)
           Huisnrs    = c(klakl$Huisnr,klakmelders$Huisnr)
           HLDs       = nettopo$EAN_koppel$ID_Hoofdleiding_LS[which((nettopo$EAN_koppel$Huisnr %in% Huisnrs)  &  (nettopo$EAN_koppel$PC_6  %in% PCs))]
           HLDs       = list(unique(HLDs))

           assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = assets$LSkabels[HLDs]                                              # Zoeken op Postcode 6
           assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels")  # Aanroepen functie om tijdsverschillen e.d. te berekenen
           
           assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = assets$LSmoffen[HLDs]                                              # Zoeken op Postcode 6
           assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen")  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
         },
         MS={
           PCs        = c(klakl$PC_6,klakmelders$PC_6)
           Huisnrs    = c(klakl$Huisnr,klakmelders$Huisnr)
           Routes     = nettopo$EAN_koppel$Routenaam_MS[which((nettopo$EAN_koppel$Huisnr %in% Huisnrs)  &  (nettopo$EAN_koppel$PC_6  %in% PCs))]
           Routes     = list(unique(Routes))
           
           assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = assets$MSkabels[Routes]                                              # Zoeken op Postcode 6
           assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels")  # Aanroepen functie om tijdsverschillen e.d. te berekenen
           
           
           assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = assets$MSmoffen[Routes]                                              # Zoeken op Postcode 6
           assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen")  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
           
             }) 
  
  
  
  return(assetsl)
}

# XY of afstanden-proxy----------------------------------------------------------------------
Proxy_XY = function(klakl,klakmelders,voltage,assets,assetsl)
{
  # Vind HLD bij klakmelders
  # Kies de assets die voldoen aan de PC6
  # teruggeven koppeltabel
  switch(voltage,
         LS={
           #Bepaal ranges van X en Y
           Xmin   = floor(klakl$Coo_X - config$szoek$LS)
           Xmax   = ceiling(klakl$Coo_X + config$szoek$LS)
           Ymin   = floor(klakl$Coo_Y - config$szoek$LS)
           Ymax   = ceiling(klakl$Coo_Y + config$szoek$LS)
           if(!is.na(Xmin+Xmax+Ymin+Ymax)){
             XRange = c(Xmin:Xmax)
             YRange = c(Ymin:Ymax)
           }
           else {
             XRange = c(0)
             YRange = c(0)
           }
  
           assetsl$LSkabels[[klakl$ID_KLAK_Melding]] <- assets$LSkabels[((Coo_X_van %in% XRange) & (Coo_Y_van %in% YRange))|((Coo_X_naar %in% XRange)&(Coo_Y_naar %in% YRange))]                # Zoeken op XY
           assetsl$LSkabels[[klakl$ID_KLAK_Melding]] <- process.table(assetsl$LSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels")                                                               # Aanroepen functie om tijdsverschillen e.d. te berekenen
           
           assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] <- assets$LSmoffen[(Coo_X %in% XRange) & (Coo_Y %in% YRange)]      # Zoeken op XY
           assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] <- process.table(assetsl$LSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen")                         # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
         },
         MS={
           #Bepaal ranges van X en Y
           Xmin   = floor(klakl$Coo_X   - config$szoek$MS)
           Xmax   = ceiling(klakl$Coo_X + config$szoek$MS)
           Ymin   = floor(klakl$Coo_Y   - config$szoek$MS)
           Ymax   = ceiling(klakl$Coo_Y + config$szoek$MS)
           
           if(!is.na(Xmin+Xmax+Ymin+Ymax)){
             XRange = c(Xmin:Xmax)
             YRange = c(Ymin:Ymax)
           }
           else {
             XRange = c(0)
             YRange = c(0)
           }
            
           
           assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = assets$MSkabels[((Coo_X_van %in% XRange) & (Coo_Y_van %in% YRange))|((Coo_X_naar %in% XRange)&(Coo_Y_naar %in% YRange))]                # Zoeken op XY
           assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels")  # Aanroepen functie om tijdsverschillen e.d. te berekenen
           
           
           assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = assets$MSmoffen[(Coo_X %in% XRange) & (Coo_Y %in% YRange)]                                        # Zoeken op XY
           assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen")  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
           
         }) 
  return(assetsl)
}


# Uitrekenen tijdsverschillen, wel of niet verwijderd------------------------------------------------  
process.table = function(assetstb,klakl,assettype){
      #bugfixing regels:
      #try(print(paste(klakl$ID_KLAK_Melding,length(assetstb),nrow(assetstb),class(assetstb),assettype)))
      #try(print(paste(sum(complete.cases(assetstb$ID_unique)), nrow(assetstb),class(assetstb),assettype)))
    if(length(assetstb)              ==   0){print(head(assetstb))}else{
    try({ 
    assetstb  <-    assetstb[!is.na(assetstb$ID_unique)]                                                 # Verwijder rijen met enkel NA's
    if (sum(complete.cases(assetstb$ID_unique)) ==   0   ){assetstb <- assetstb[0,]}else{         # Verwijder sets met enkel NA's
      
    if (sum(assetstb$Status_ID=="Removed"|assetstb$Status_ID=="Lengthch") ==0){assetstb <- assetstb[0,]} #Indien alleen toegevoegde assets -> verwijder alle rijen
    else  {
    # uitrekenen tijdsverschillen met storing
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
                 Adiff = assetstb$DateAdded   -   klakl$Datum_Verwerking_Gereed,
                 Rdiff = assetstb$DateRemoved -   klakl$Datum_Verwerking_Gereed),
               kabels = data.table(
                 Adiff = assetstb$DateAdded   -   klakl$Datum_Verwerking_Gereed,
                 Rdiff = assetstb$DateRemoved -   klakl$Datum_Verwerking_Gereed, 
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
             tdiff         <- sapply(assetstb$DateAdded,function(x){x-assetstb$DateRemoved})  # Uitrekenen tijdsverschillen tussen tijd van verwijdering en toevoeging
             
             xdiff         <- sapply(assetstb$Coo_X,function(x){x-assetstb$Coo_X})            # Uitrekenen afstandsverschillen in horizontale richting
             
             ydiff         <- sapply(assetstb$Coo_Y,function(x){x-assetstb$Coo_Y})            # Uitrekenen afstandsverschillen in verticale richting
             
             sdiff         <- xdiff^2+ydiff^2                                                 # Uitrekenen afstandsverschillen totaal
             
             matrixvv      <- matrix((sdiff < config$sdiff$max) & (tdiff >  config$vervdiff$min & tdiff < config$vervdiff$max),ncol=max(nrow(tdiff),1))
             diag(matrixvv)<- FALSE                                                           #Zorg ervoor dat assets niet door zichzelf vervangen kunnen worden
             
             in.verv       <- rowSums(matrixvv)
    
             assetstb             <- cbind(assetstb, in.verv)
             assetstb$koppelc     <- assetstb$in.timediff & assetstb$in.verv
           },
           kabels={
             tdiff   <- sapply(assetstb$DateAdded,function(x){x-assetstb$DateRemoved})

             xdiffvv <- sapply(assetstb$Coo_X_van, function(x){x-assetstb$Coo_X_van})
             xdiffvn <- sapply(assetstb$Coo_X_van, function(x){x-assetstb$Coo_X_naar})
             xdiffnv <- sapply(assetstb$Coo_X_naar,function(x){x-assetstb$Coo_X_van})
             xdiffnn <- sapply(assetstb$Coo_X_naar,function(x){x-assetstb$Coo_X_naar})
             
             ydiffvv <- sapply(assetstb$Coo_Y_van, function(x){x-assetstb$Coo_Y_van})
             ydiffvn <- sapply(assetstb$Coo_Y_van, function(x){x-assetstb$Coo_Y_naar})
             ydiffnv <- sapply(assetstb$Coo_Y_naar,function(x){x-assetstb$Coo_Y_van})
             ydiffnn <- sapply(assetstb$Coo_Y_naar,function(x){x-assetstb$Coo_Y_naar})
             
             sdiffar <- array(c(xdiffvv^2+ydiffvv^2,xdiffvn^2+ydiffvn^2,xdiffnv^2+ydiffnv^2,xdiffnn^2+ydiffnn^2),dim=c(max(1,ncol(tdiff)),max(1,nrow(tdiff)),4))
             sdiff   <- aaply(sdiffar,1:2,min)
             
             matrixvv      <- matrix((sdiff < config$sdiff$max) & (tdiff >  config$vervdiff$min & tdiff < config$vervdiff$max),ncol=max(nrow(tdiff),1))
             diag(matrixvv)<- FALSE
             
             in.verv <-  ifelse(is.na(assetstb$Length_ch),
                         ifelse(assetstb$Status_ID == "Active", 0,rowSums(matrixvv)),
                         ifelse(assetstb$Length_ch < 0, 1, 0)
                                )
             
             assetstb             <- cbind(assetstb, in.verv)
             assetstb$koppelc     <- assetstb$in.timediff & assetstb$in.verv
             
           }
    )
    }}},silent=F)}
    
   
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

 
#  install.packages("rbenchmark")
#  library(rbenchmark)
#  
#  benchmark(replications = 10, order = "elapsed",
#            vector_search1 = {
#              assets$LSkabels[which(assets$LSkabels$PC_6_van == "1011AA" & assets$LSkabels$PC_6_naar== "1011AB")]
#            },
#            vector_search2 = {
#              assets$LSkabels[assets$LSkabels$PC_6_van == "1011AA" & assets$LSkabels$PC_6_naar== "1011AB"]
#            },
#            binary_search1 = {
#              assets$LSkabels[.("1011AA","1011AB")]
#            },
#            binary_search2 = {
#              assets$LSkabels[PC_6_van == "1011AA" & PC_6_naar== "1011AB"]
#            }
#  )


runall = function(){


  for(method in c("PC","XY","TOPO"))  {
    fprof=paste0("C:/Data/AHAdata/Rprof",method,".out")
    print(method)
    Rprof(filename = fprof)
    AHA_Proxy_KA_BAR_NOR(method)
    Rprof()
    summaryRprof(fprof)
  }
}