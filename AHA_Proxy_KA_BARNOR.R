AHA_Proxy_KA_BAR_NOR = 
  function(method,set,nr1=1,nr2=nrow(storingen$LS),assettypes=c("LSkabels","MSkabels","LSmoffen","MSmoffen")) 
  { # This function calculates the asset id - klak id proxy for the asset health analytics project
    # Data should be loaded using the AHA_Proxy_Dataset function (global environment)
    # 
    # Input:
    # Method refers to the proxy method used, XY, PC, TOPO or TOPOplus (yet to be implemented)
    # set refers to the asset-data source that has to be used (BAR or NOR)
    # nr1 refers to the first row of the storingen$LS and storingen$MS set to be used, nr2 to the last line
    # parallel = T will process outages in parallel (to be implemented)    
    
# Configuration parameters and settings -------------------------------------------------------------
  config = list()
  config$timediff$min = -30 # Aantal dagen tussen storing en verwijdering/toevoeging assets
  config$timediff$max =  70 # Aantal dagen tussen storing en verwijdering/toevoeging assets
  config$vervdiff$min = -45 # Aantal dagen tussen verwijderde en toegevoegde asset
  config$vervdiff$max =  45 # Aantal dagen tussen verwijderde en toegevoegde asset
  config$sdiff$max    =   2 # Afstand tussen verwijderde en toegevoegde asset
  config$szoek$LS     = 200 # Afstand waarover assets gezocht worden bij XY-proxy
  config$szoek$MS     =2000 # Afstand waarover assets gezocht worden bij XY-proxy
  config$set          = set
  switch(set,
         NOR={config$kabelscol    = c("ID_unique","ID_NAN","Status_ID","is.verv","DateAdded","DateRemoved","DateLength_ch","Status_ch","Date_Status_ch") #kolommen die meegenomen worden in de output voor kabels
              config$moffencol    = c("ID_unique","ID_NAN","Status_ID","Coo_X","Coo_Y","DateAdded","DateRemoved")},                                      #kolommen die meegenomen worden in de output voor moffen
         BAR={config$kabelscol    = c("ID_BAR","ID_NAN", "Status_ID","is.verv","DateAdded","DateRemoved","DateLength_ch")                                #kolommen die meegenomen worden in de output voor kabels
              config$moffencol    = c("ID_BAR","ID_NAN", "Status_ID","Coo_X","Coo_Y","DateAdded","DateRemoved")})                                        #kolommen die meegenomen worden in de output voor moffen
  config$Status_ch    = c("In Bedrijf->Buiten Bedrijf","LS->BL","MS->BM", "LS_TNA->BL","LS->NL","1536->BB","3->PG",
                          "LS_TN->BL","12->BB","MS->Buiten Bedrijf","MS->NM","LS->BL_TN","LM->BM","8->BB","513->BB",
                          "1025->BB" ,"2048->BB") # De status changes waarbij kabels die in bedrijf zijn uit bedrijf worden genomen

# Load data if not available -----------------------------
    if (!exists("assets")) {
      cat("Importing data file \n"); tic()
      switch(config$set,
             NOR=load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_NOR.Rda"),envir = .GlobalEnv),
             BAR=load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_BAR.Rda"),envir = .GlobalEnv))
      #load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo.Rda"),envir = .GlobalEnv)
      load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"),envir = .GlobalEnv)
      toc();
    }; 
    
    #Laad juiste assetset
    switch(config$set,
           NOR={if(!("file" %in% names(assets$LSmoffen))){load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_NOR.Rda"),envir = .GlobalEnv)}},
           BAR={if( ("file" %in% names(assets$LSmoffen))){load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_BAR.Rda"),envir = .GlobalEnv)}}
    )

# Quick Fixes ------------------------------------------------------------------------------------------
    storingen$LS[,PC_6:=gsub(" ","",storingen$LS$PC_6, fixed=TRUE)]  #verwijderen spaties uit postcodes
    storingen$MS[,PC_6:=gsub(" ","",storingen$MS$PC_6, fixed=TRUE)]  #verwijderen spaties uit postcodes
    storingen$LS$Tijdstip_begin_storing  <- as.Date(storingen$LS$Tijdstip_begin_storing) #converteren tijdstippen naar datumnotatie
    storingen$MS$Tijdstip_begin_storing  <- as.Date(storingen$MS$Tijdstip_begin_storing) #converteren tijdstippen naar datumnotatie
    storingen$LS$Datum_Verwerking_Gereed <- as.Date(storingen$LS$Datum_Verwerking_Gereed) #converteren tijdstippen naar datumnotatie
    storingen$MS$Datum_Verwerking_Gereed <- as.Date(storingen$MS$Datum_Verwerking_Gereed) #converteren tijdstippen naar datumnotatie
    setkey(storingen$KLAKMelders, ID_KLAK_Melding)
    storingen$LS$ID_Groep = storingen$KLAKMelders[list(storingen$LS$ID_KLAK_Melding),mult="first"][,c("ID_Groep"),with=F] #Koppel groepsnummer storing aan Klakmelding
    storingen$MS$ID_Groep = storingen$KLAKMelders[list(storingen$MS$ID_KLAK_Melding),mult="first"][,c("ID_Groep"),with=F] #Koppel groepsnummer storing aan Klakmelding
    storingen$MS          = storingen$MS[!duplicated(storingen$MS$ID_KLAK_Melding),]
    storingen$LS          = storingen$LS[!duplicated(storingen$LS$ID_KLAK_Melding),]
    try(setnames(storingen$KLAKMelders, "Routenaam", "Routenaam_MS"))
    #PC_4 Toevoegen
    assets$MSmoffen$PC_4 = substr(assets$MSmoffen$PC_6,1,4)
    assets$LSmoffen$PC_4 = substr(assets$LSmoffen$PC_6,1,4)
  
    assets$LSkabels$PC_4_van  = substr(assets$LSkabels$PC_6_van,1,4)
    assets$LSkabels$PC_4_naar = substr(assets$LSkabels$PC_6_naar,1,4)  
    assets$MSkabels$PC_4_van  = substr(assets$MSkabels$PC_6_van,1,4)
    assets$MSkabels$PC_4_naar = substr(assets$MSkabels$PC_6_naar,1,4)
    #Hoofdleiding hernoemen
    try(setnames(assets$LSmoffen,"ID_Hoofdleiding_present","ID_Hoofdleiding"))
    try(setnames(assets$LSkabels,"ID_Hoofdleiding_present","ID_Hoofdleiding"))
    try(setnames(assets$MSmoffen,"Routenaam_Present",      "Routenaam_MS"))
    try(setnames(assets$MSkabels,"Routenaam_Present",      "Routenaam_MS"))

    switch(set,
           NOR={tic()
                assets$LSkabels$Index=rownames(assets$LSkabels)
                assets$MSkabels$Index=rownames(assets$MSkabels)
                assets$LSmoffen$Index=rownames(assets$LSmoffen)
                assets$MSmoffen$Index=rownames(assets$MSmoffen) #Toevoegen unieke index aan assets
                toc()},
           BAR={l_ply(assets[1:4],function(x) try(setnames(x,"Status_R","Status_ID"),silent=T))
                l_ply(assets[1:4],function(x) try(setnames(x,"Hoofdleiding","ID_Hoofdleiding"),silent=T))
                l_ply(assets[1:4],function(x) try(setnames(x,"ID_LS_Hoofdleiding","ID_Hoofdleiding_2"),silent=T))
                l_ply(assets[1:4],function(x) try(setnames(x,"ID_MS_Hoofdleiding","ID_Hoofdleiding_2"),silent=T))
                l_ply(assets[1:4],function(x) try(setnames(x,"Bouwejaar","Bouwjaar"),silent=T))
                assets$MSkabels$PC_4_van  <- substr(assets$MSkabels$PC_6_van,1,4)
                assets$MSkabels$PC_4_naar <- substr(assets$MSkabels$PC_6_naar,1,4)}
    )
    
    #nettopo$EAN_koppel$ID_Hoofdleiding_LS <- as.character(nettopo$EAN_koppel$ID_Hoofdleiding_LS) #zorgen dat hoofdleidingen characters zijn
    

# Bepalen of kabels wel of niet vervangen is, aanmaken lijst met weg te schrijven data  ---------------------------
    #tic()
    #assets$MSkabels$is.verv <- kabel_verv(assets$MSkabels,config)
    #assets$LSkabels$is.verv <- kabel_verv(assets$LSkabels,config)
    #toc()

    assetsltb <- list()    # aanmaken tabel met gekoppelde assets
    assetsltb$LSkabels        = as.list(storingen$LS$ID_KLAK_Melding[which(complete.cases(storingen$LS$ID_KLAK_Melding[nr1:nr2]))])
    names(assetsltb$LSkabels) = as.list(storingen$LS$ID_KLAK_Melding[which(complete.cases(storingen$LS$ID_KLAK_Melding[nr1:nr2]))])
    
    assetsltb$MSkabels        = as.list(storingen$MS$ID_KLAK_Melding[which(complete.cases(storingen$MS$ID_KLAK_Melding[nr1:nr2]))])
    names(assetsltb$MSkabels) = as.list(storingen$MS$ID_KLAK_Melding[which(complete.cases(storingen$MS$ID_KLAK_Melding[nr1:nr2]))])
    
    assetsltb$LSmoffen        = as.list(storingen$LS$ID_KLAK_Melding[which(complete.cases(storingen$LS$ID_KLAK_Melding[nr1:nr2]))])
    names(assetsltb$LSmoffen) = as.list(storingen$LS$ID_KLAK_Melding[which(complete.cases(storingen$LS$ID_KLAK_Melding[nr1:nr2]))])
    
    assetsltb$MSmoffen        = as.list(storingen$MS$ID_KLAK_Melding[which(complete.cases(storingen$MS$ID_KLAK_Melding[nr1:nr2]))])
    names(assetsltb$MSmoffen) = as.list(storingen$MS$ID_KLAK_Melding[which(complete.cases(storingen$MS$ID_KLAK_Melding[nr1:nr2]))])
    
    

# Set keys for different methods-----------------------
    switch(method,
           PC={
             dummy=list()
             setkey(assets$LSmoffen,PC_6)
             setkey(assets$MSmoffen,PC_4)
             setkey(assets$LSkabels,Index);
             dummy$LSkabels_van  = assets$LSkabels[,c("Index","PC_6_van"),with=F];setkey(dummy$LSkabels_van,PC_6_van)
             dummy$LSkabels_naar = assets$LSkabels[,c("Index","PC_6_naar"),with=F];setkey(dummy$LSkabels_naar,PC_6_naar)
             setkey(assets$MSkabels,Index);
             dummy$MSkabels_van  = assets$MSkabels[,c("Index","PC_4_van"),with=F];setkey(dummy$MSkabels_van,PC_4_van)
             dummy$MSkabels_naar = assets$MSkabels[,c("Index","PC_4_naar"),with=F];setkey(dummy$MSkabels_naar,PC_4_naar)
           },
           TOPO={
             setkey(assets$LSmoffen,ID_Hoofdleiding)
             setkey(assets$MSmoffen,Routenaam_MS)
             setkey(assets$LSkabels,ID_Hoofdleiding)
             setkey(assets$MSkabels,Routenaam_MS)
           },
           XY={
             dummy=list()
             setkey(assets$LSmoffen,Index);dummy$LSmoffen = assets$LSmoffen[,c("Index","Coo_X","Coo_Y"),with=F];setkey(dummy$LSmoffen,Coo_X)
             setkey(assets$MSmoffen,Index);dummy$MSmoffen = assets$MSmoffen[,c("Index","Coo_X","Coo_Y"),with=F];setkey(dummy$MSmoffen,Coo_X)
             setkey(assets$LSkabels,Index);dummy$LSkabels = assets$LSkabels[,c("Index","Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar"),with=F];setkey(dummy$LSkabels,Coo_X_van)
             setkey(assets$MSkabels,Index);dummy$MSkabels = assets$MSkabels[,c("Index","Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar"),with=F];setkey(dummy$MSkabels,Coo_X_van)
           })
    setkey(storingen$KLAKMelders,ID_Groep)
    
    

# For-loop over klakmeldingen, aanroepen proxyfunctie,wegschrijven data -----------------------------
    for(voltage in c("LS","MS")){ 
      cat(paste(voltage,nr1))
      titlepb <- paste("Proxy",method,voltage)
      klaktabel    <- storingen[[voltage]]                # aanmaken tabel met klakmeldingen
      if (nr2 > nrow(klaktabel)) { nr2 = nrow(klaktabel)}
      if (nr1 <= nr2){
        
        counter    <- 0
        #progress bar
        setTkProgressBar(pb, nr1,label = paste("Proxy", voltage,"min=",nr1,",max=",nr2,"nr = ", counter)) ;        
          
        for(klaknr in klaktabel$ID_KLAK_Melding[nr1:nr2]){
          klak          <- klaktabel[ID_KLAK_Melding==klaknr]
          klakmeldingen <- storingen$KLAKMelders[as.list(klak$ID_Groep)]
          
          assetsltb <- switch(method,
                              PC   = Proxy_PC_6(klak,klakmeldingen,voltage,assets,assetsltb,config,dummy),
                              TOPO = Proxy_TOPO(klak,klakmeldingen,voltage,assets,assetsltb,config),
                              XY   = Proxy_XY  (klak,klakmeldingen,voltage,assets,assetsltb,config,dummy)
          )
          counter       <- counter + 1; setTkProgressBar(pb, nr1 + counter,label = paste("Proxy", voltage,"min=",nr1,",max=",nr2,"nr=", counter)) ;
          
        }}
    } 
    #filename=paste0("C:/Data/AHAdata/3. Analyse Datasets","/assetsl_NOR_",method,"_",gsub(":",".",paste0(Sys.time())),".Rda")  # definiëren file van weg te schrijven assets
    filename=paste0(settings$Analyse_Datasets,"/1. KA Proxy/assetsl_",set,"_",method,gsub(":",".",paste0(Sys.time())),".Rda")  # definiëren file van weg te schrijven assets
    save(assetsltb,file=filename)
    print(format(object.size(assetsltb),units="Mb"))
    rm(assetsltb)
    close(pb);
  }

# Postcode 6 proxy ---------------------------------    
Proxy_PC_6 = function(klakl,klakmelders,voltage,assets,assetsl,config,dummy)
{
  # Postcodelijst samenstellen
  # Kies de assets die voldoen aan de PC6
  # teruggeven koppeltabel
  switch(voltage,
         LS={
           PClijst = unique(c(klakl$PC_6,klakmelders$PC_6))
           PClijst = PClijst[!is.na(PClijst)]
           PCdt    = data.table(PC_6_naar=PClijst)
           
           
           # Zoeken op Postcode 6 van, voeg andere assets die op PC6_naar matchen ook toe
           Indices = rbind(dummy$LSkabels_van[J(PClijst)][,c("Index"),with=F],dummy$LSkabels_naar[J(PClijst)][,c("Index"),with=F])
           Indices = unique(Indices$Index[!is.na(Indices$Index)])
           assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = assets$LSkabels[J(Indices),config$kabelscol,with=F]
           if( nrow(assetsl$LSkabels[[klakl$ID_KLAK_Melding]])>80000){ 
             Sys.sleep(1)
             paste(paste(PClijst,Indices))
             Sys.sleep(1)
           }
           rm(Indices)
           #if(nrow(assetsl$LSkabels[[klakl$ID_KLAK_Melding]])>200){assetsl$LSkabels[[klakl$ID_KLAK_Melding]]=assetsl$LSkabels[[klakl$ID_KLAK_Melding]][,1:100]}
           assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels",config)  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
           
           assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = assets$LSmoffen[J(PClijst),config$moffencol,with=F]                                                     # Zoeken op Postcode 6
           assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen",config)  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
         },
         MS={
           PClijst = list(unique(c(klakl$PC_4,substr(klakmelders$PC_4,1,4))))
           
           Indices = rbind(dummy$MSkabels_van[PClijst][,c("Index"),with=F],dummy$MSkabels_naar[PClijst][,c("Index"),with=F]) #Zoeken op PC4
           Indices = unique(Indices$Index[!is.na(Indices$Index)])
           assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = assets$MSkabels[J(Indices),config$kabelscol,with=F]
           #if(nrow(assetsl$MSkabels[[klakl$ID_KLAK_Melding]])>200){assetsl$MSkabels[[klakl$ID_KLAK_Melding]]=assetsl$MSkabels[[klakl$ID_KLAK_Melding]][,1:100]}
           assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels",config)  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
           
           assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = assets$MSmoffen[PClijst,config$moffencol,with=F] # Zoeken op postcode 4
           assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen",config)  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
         }) 
  
  
  
  return(assetsl)
}

# Hoofdleidingen proxy -------------------------------------
Proxy_TOPO = function(klakl,klakmelders,voltage,assets,assetsl,config)
{
  # Vind HLD bij klakmelders
  # Kies de assets die voldoen aan de PC6
  # teruggeven koppeltabel
  switch(voltage,
         LS={
           #PCs        = c(klakl$PC_6,klakmelders$PC_6)
           #Huisnrs    = c(klakl$Huisnr,klakmelders$Huisnr)
           #HLDs       = nettopo$EAN_koppel$ID_Hoofdleiding_LS[which((nettopo$EAN_koppel$Huisnr %in% Huisnrs)  &  (nettopo$EAN_koppel$PC_6  %in% PCs))]
           HLDs       = c(klakl$ID_Hoofdleiding,klakmelders$ID_Hoofdleiding)
           HLDs       = list(unique(HLDs))
           HLDs       = lapply(HLDs, function (x) x[!is.na(x)])   # NA's verwijderen
           if(sum(!is.na(HLDs))==0|nrow(data.frame(HLDs))>10){
             assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = assets$LSkabels[0,]
             assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = assets$LSmoffen[0,]
           }else{
             assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = assets$LSkabels[HLDs,config$kabelscol,with=F]                                              # Zoeken op Postcode 6
             if(nrow(assetsl$LSkabels[[klakl$ID_KLAK_Melding]] )>500){(paste(klakl$ID_KLAK_Melding, nrow(assetsl$LSkabels[[klakl$ID_KLAK_Melding]] )))}
             assetsl$LSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels",config)  # Aanroepen functie om tijdsverschillen e.d. te berekenen
             
             assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = assets$LSmoffen[HLDs,config$moffencol,with=F]                                               # Zoeken op Postcode 6
             if(nrow(assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] )>500){print(paste(klakl$ID_KLAK_Melding, nrow(assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] )))}
             assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$LSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen",config)  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
           }
         },
         MS={
           #PCs        = c(klakl$PC_6,klakmelders$PC_6)
           #Huisnrs    = c(klakl$Huisnr,klakmelders$Huisnr)
           #Routes     = nettopo$EAN_koppel$Routenaam_MS[which((nettopo$EAN_koppel$Huisnr %in% Huisnrs)  &  (nettopo$EAN_koppel$PC_6  %in% PCs))]
           Routes     = klakmelders$Routenaam_MS
           Routes     = list(unique(Routes))                        # Dubbelingen verwijderen
           Routes     = lapply(Routes, function (x) x[!is.na(x)])   # NA's verwijderen
           if(sum(!is.na(Routes))==0|nrow(data.frame(Routes))>10){
             assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = assets$MSkabels[0,]
             assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = assets$MSmoffen[0,]
           }else{
             assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = assets$MSkabels[Routes,config$kabelscol,with=F]                                              # Zoeken op Postcode 6
             if(nrow(assetsl$MSkabels[[klakl$ID_KLAK_Melding]] )>500){print(paste(klakl$ID_KLAK_Melding, nrow(assetsl$MSkabels[[klakl$ID_KLAK_Melding]] )))}
             assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels",config)  # Aanroepen functie om tijdsverschillen e.d. te berekenen
             
             
             assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = assets$MSmoffen[Routes,config$moffencol,with=F]                                              # Zoeken op Postcode 6
             if(nrow(assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] )>500){print(paste(klakl$ID_KLAK_Melding, nrow(assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] )))}
             assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen",config)  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
           }
         }) 
  
  
  
  return(assetsl)
}

# XY of afstanden-proxy----------------------------------------------------------------------
Proxy_XY = function(klakl,klakmelders,voltage,assets,assetsl,config,dummy)
{
  # Vind HLD bij klakmelders
  # Kies de assets die voldoen aan de PC6
  # teruggeven koppeltabel
  switch(voltage,
         LS={
           if(sum(!is.na(klakl$Coo_X),!is.na(klakmelders$Coo_X))>0){
           #Bepaal ranges van X en Y
           Xmin   = floor(  min(klakl$Coo_X,klakmelders$Coo_X,na.rm=T) - config$szoek$LS)
           Xmax   = ceiling(max(klakl$Coo_X,klakmelders$Coo_X,na.rm=T) + config$szoek$LS)
           Ymin   = floor(  min(klakl$Coo_Y,klakmelders$Coo_Y,na.rm=T) - config$szoek$LS)
           Ymax   = ceiling(max(klakl$Coo_Y,klakmelders$Coo_Y,na.rm=T) + config$szoek$LS)
           
             XRange = c(Xmin:Xmax)
             YRange = c(Ymin:Ymax)
             
             switch(config$set,
                    NOR = {#kabels
                           dummy2=list() 
                           dummy2$X_van                               <- dummy$LSkabels[J(XRange)];setkey(dummy2$X_van,Coo_Y_van) #Zoeken op Xvan
                           try(dummy2$Indices_van                     <- dummy2$X_van[J(YRange)])                                  #Zoeken op Yvan
                           setkey(dummy$LSkabels,Coo_X_naar)
                           dummy2$X_naar                              <- dummy$LSkabels[J(XRange)];setkey(dummy2$X_naar,Coo_Y_naar)#Zoeken op Xnaar
                           try(dummy2$Indices                             <- rbind(dummy2$Indices_van,dummy2$X_naar[J(YRange)]))         #Zoeken op Ynaar, combineren
                           dummy2$Indices                             <- unique(dummy2$Indices[!is.na(dummy2$Indices$Index),])      #Uniek en NA's verwijderen
                           assetsl$LSkabels[[klakl$ID_KLAK_Melding]]  <- assets$LSkabels[J(dummy2$Indices$Index)][,config$kabelscol,with=F]
                           
                           #Moffen
                           dummy2$X                                   <- dummy$LSmoffen[J(XRange)];setkey(dummy2$X,Coo_Y)
                           try(dummy2$Indices                             <- dummy2$X[J(YRange)])
                           dummy2$Indices                             <- dummy2$Indices[!is.na(dummy2$Indices$Index),]
                           assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] <- assets$LSmoffen[J(dummy2$Indices$Index)][,config$moffencol,with=F]
                           rm(dummy2)},
                    BAR = {assetsl$LSkabels[[klakl$ID_KLAK_Melding]] <- assets$LSkabels[((Coo_X_van < Xmax & Coo_X_van > Xmin) & (Coo_Y_van < Ymax & Coo_Y_van > Ymin))|((Coo_X_naar < Xmax ) & (Coo_X_naar > Xmin ) &(Coo_Y_naar < Ymax) &(Coo_Y_naar > Ymin))][,config$kabelscol,with=F]               
                           assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] <- assets$LSmoffen[((Coo_X < Xmax & Coo_X > Xmin) & (Coo_Y < Ymax & Coo_Y > Ymin))][,config$moffencol,with=F]               
                    })           }
           else { #Indien geen XY bekend
             assetsl$LSkabels[[klakl$ID_KLAK_Melding]] <- assets$LSkabels[0,][,config$kabelscol,with=F]                # Lege tabel
             assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] <- assets$LSkabels[0,][,config$kabelscol,with=F]                # Lege tabel
           }

           
           
           # Aanroepen functie om tijdsverschillen e.d. te berekenen
           assetsl$LSkabels[[as.character(klakl$ID_KLAK_Melding)]] <- process.table(assetsl$LSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels",config) 
           assetsl$LSmoffen[[klakl$ID_KLAK_Melding]] <- process.table(assetsl$LSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen",config)                         # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
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
           #Zoeken op XY
           switch(config$set,
                  NOR = {assetsl$MSkabels[[klakl$ID_KLAK_Melding]] <- assets$MSkabels[((Coo_X_van %in% XRange) & (Coo_Y_van %in% YRange))|((Coo_X_naar %in% XRange)&(Coo_Y_naar %in% YRange)),config$kabelscol,with=F]                # Zoeken op XY
                         assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] <- assets$MSmoffen[(Coo_X %in% XRange) & (Coo_Y %in% YRange),config$moffencol,with=F]},                                                                             # Zoeken op XY
                  BAR = {assetsl$MSkabels[[klakl$ID_KLAK_Melding]] <- assets$MSkabels[((Coo_X_van < Xmax & Coo_X_van > Xmin) & (Coo_Y_van < Ymax & Coo_Y_van > Ymin))|((Coo_X_naar < Xmax ) & (Coo_X_naar > Xmin ) &(Coo_Y_naar < Ymax) &(Coo_Y_naar > Ymin)),config$kabelscol,with=F]               
                         assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] <- assets$MSmoffen[((Coo_X < Xmax & Coo_X > Xmin) & (Coo_Y < Ymax & Coo_Y > Ymin)),config$moffencol,with=F]               
                  })
           
           
           assetsl$MSkabels[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSkabels[[klakl$ID_KLAK_Melding]],klakl,"kabels",config)  # Aanroepen functie om tijdsverschillen e.d. te berekenen
           assetsl$MSmoffen[[klakl$ID_KLAK_Melding]] = process.table(assetsl$MSmoffen[[klakl$ID_KLAK_Melding]],klakl,"moffen",config)  # Bereken, als er verwijderde of veranderde assets zijn, de datumverschillen
           
         }) 
  return(assetsl)
}

# Uitrekenen tijdsverschillen, wel of niet verwijderd------------------------------------------------  
process.table = function(assetstb,klakl,assettype,config){
  #bugfixing regels:
  #try(print(paste(klakl$ID_KLAK_Melding,length(assetstb),nrow(assetstb),class(assetstb),assettype)))
  #try(print(paste(sum(complete.cases(assetstb$ID_unique)), nrow(assetstb),class(assetstb),assettype)))
  if(length(assetstb)              ==   0){print(head(assetstb))}else{
    try({switch(config$set,
                NOR={assetstb <- assetstb[!is.na(assetstb$ID_unique)]; countnna <- sum(complete.cases(assetstb$ID_unique))},                                         # Verwijder rijen met enkel NA's
                BAR={assetstb <- assetstb[!is.na(assetstb$ID_BAR)];    countnna <- sum(complete.cases(assetstb$ID_BAR))})
         if (countnna == 0 ){assetstb <- assetstb[0,]}else{                                                   # Verwijder sets met enkel NA's
           if (sum(assetstb$Status_ID=="Removed"|assetstb$Status_ID=="Lengthch") ==0){assetstb <- assetstb[0,]} # Indien alleen toegevoegde assets -> verwijder alle rijen
           else  {
             assetstb <- cbind(assetstb,ID_KLAK_Melding=klakl$ID_KLAK_Melding)                             # Toevoegen KLAkmeldingnummer    
             # uitrekenen tijdsverschillen met storing
             if (is.na(klakl$Datum_Verwerking_Gereed)){     #calculate differences between the changes in the NOR/BARlog and the time of start interruption, if time of processing is NA
               diff = 
                 switch(assettype, 
                        moffen = data.table(
                          Adiff = assetstb$DateAdded   - klakl$Tijdstip_begin_storing,
                          Rdiff = assetstb$DateRemoved - klakl$Tijdstip_begin_storing),
                        kabels = data.table(
                          Adiff = assetstb$DateAdded   - klakl$Tijdstip_begin_storing,
                          Rdiff = assetstb$DateRemoved - klakl$Tijdstip_begin_storing, 
                          Sdiff = assetstb$DateRemoved - klakl$Tijdstip_begin_storing,   # Voor kabels ook status
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
                          Sdiff = assetstb$DateRemoved - klakl$Tijdstip_begin_storing,    # Voor kabels ook status
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
                    moffen = {assetstb$in.timediff <- ((assetstb$Rdiff > config$timediff$min) & (assetstb$Rdiff < config$timediff$max) );
                              assetstb$in.timediff <- ifelse(is.na(assetstb$in.timediff),FALSE,assetstb$in.timediff)},
                    kabels = {assetstb$in.timediff <- ((assetstb$Sdiff > config$timediff$min) & (assetstb$Sdiff < config$timediff$max) | 
                                                         (assetstb$Rdiff > config$timediff$min) & (assetstb$Rdiff < config$timediff$max) |
                                                         (assetstb$Ldiff > config$timediff$min) & (assetstb$Ldiff < config$timediff$max));
                              assetstb$in.timediff <- ifelse(is.na(assetstb$in.timediff),FALSE,assetstb$in.timediff)}
             )
             switch(assettype, 
                    moffen = {
                      ncr<- nrow(assetstb)
                      m1   = t(matrix(assetstb$DateAdded,ncol=ncr,nrow=ncr))
                      m2   = matrix(assetstb$DateRemoved,ncol=ncr,nrow=ncr) 
                      tdiff = m1-m2                                     # Uitrekenen tijdsverschillen tussen tijd van verwijdering en toevoeging

                      xdiff         <- sapply(assetstb$Coo_X,function(x){x-assetstb$Coo_X})            # Uitrekenen afstandsverschillen in horizontale richting
                      
                      ydiff         <- sapply(assetstb$Coo_Y,function(x){x-assetstb$Coo_Y})            # Uitrekenen afstandsverschillen in verticale richting
                      
                      sdiff         <- xdiff^2+ydiff^2                                                 # Uitrekenen afstandsverschillen totaal
                      
                      matrixvv      <- matrix((sdiff < config$sdiff$max) & (tdiff >  config$vervdiff$min & tdiff < config$vervdiff$max),ncol=max(nrow(tdiff),1))
                      diag(matrixvv)<- FALSE                                                           #Zorg ervoor dat assets niet door zichzelf vervangen kunnen worden
                      
                      is.verv       <- (rowSums(matrixvv,na.rm=T)>0)
                      
                      assetstb             <- cbind(assetstb, is.verv)
                      assetstb$koppelc     <- assetstb$in.timediff & assetstb$is.verv                  #Koppel asset T/F?
                    },
                    kabels={
                      
                      assetstb$koppelc     <- assetstb$in.timediff & assetstb$is.verv                  #Koppel asset T/F?
                      
                    }
             )
                        }}},silent=F)}
  
  
  return(assetstb)
  rm(klakl)
  
  
}

# Functie om te bepalen of kabel wel of niet vervangen is--------------------------------------------------------
kabel_verv =  function(kabelset,config){
  switch(config$set,
         NOR={
           nearestnb        <-  cbind(kabelset[!is.na(Coo_X_van),list(Index)],
                                      Rownr=1:nrow(kabelset[!is.na(Coo_X_van),]),
                                      nn2(kabelset[!is.na(Coo_X_van),list(Coo_X_van,Coo_Y_van)] , query = kabelset[!is.na(Coo_X_van),list(Coo_X_van,Coo_Y_van)],searchtype="radius",radius=config$sdiff$max,k=20)$nn.idx,
                                      nn2(kabelset[!is.na(Coo_X_van),list(Coo_X_naar,Coo_Y_naar)] , query = kabelset[!is.na(Coo_X_van),list(Coo_X_van,Coo_Y_van)],searchtype="radius",radius=config$sdiff$max,k=20)$nn.idx,
                                      nn2(kabelset[!is.na(Coo_X_van),list(Coo_X_van,Coo_Y_van)] , query = kabelset[!is.na(Coo_X_van),list(Coo_X_naar,Coo_Y_naar)],searchtype="radius",radius=config$sdiff$max,k=20)$nn.idx,
                                      nn2(kabelset[!is.na(Coo_X_van),list(Coo_X_naar,Coo_Y_naar)] , query = kabelset[!is.na(Coo_X_van),list(Coo_X_naar,Coo_Y_naar)],searchtype="radius",radius=config$sdiff$max,k=20)$nn.idx)
           names(nearestnb) <-  c("Index","Rownr",sapply(c(1:80),function(x){paste0("V",x)}))
           setkey(nearestnb, Index)
           setkey(kabelset,Index)
         },
         BAR={
           nearestnb        <-  cbind(kabelset[!is.na(Coo_X_van),list(ID_BAR)],
                                      Rownr=1:nrow(kabelset[!is.na(Coo_X_van),]),
                                      nn2(kabelset[!is.na(Coo_X_van),list(Coo_X_van,Coo_Y_van)] , query = kabelset[!is.na(Coo_X_van),list(Coo_X_van,Coo_Y_van)],searchtype="radius",radius=config$sdiff$max,k=20)$nn.idx,
                                      nn2(kabelset[!is.na(Coo_X_van),list(Coo_X_naar,Coo_Y_naar)] , query = kabelset[!is.na(Coo_X_van),list(Coo_X_van,Coo_Y_van)],searchtype="radius",radius=config$sdiff$max,k=20)$nn.idx,
                                      nn2(kabelset[!is.na(Coo_X_van),list(Coo_X_van,Coo_Y_van)] , query = kabelset[!is.na(Coo_X_van),list(Coo_X_naar,Coo_Y_naar)],searchtype="radius",radius=config$sdiff$max,k=20)$nn.idx,
                                      nn2(kabelset[!is.na(Coo_X_van),list(Coo_X_naar,Coo_Y_naar)] , query = kabelset[!is.na(Coo_X_van),list(Coo_X_naar,Coo_Y_naar)],searchtype="radius",radius=config$sdiff$max,k=20)$nn.idx)
           names(nearestnb) <-  c("ID_BAR","Rownr",sapply(c(1:80),function(x){paste0("V",x)}))
           setkey(nearestnb, ID_BAR)
           setkey(kabelset, ID_BAR)
           
         })
  nearestnb[nearestnb == 0] <- NA
  tdiff            <- nearestnb
  
  for(kolom in (names(nearestnb)[-c(1:2)])){
    nearestnb[nearestnb[,eval(parse(text=kolom))] == nearestnb$Rownr,eval(parse(text=kolom)):=NA] 
    tdiff[,eval(parse(text=kolom)) := (kabelset[!is.na(Coo_X_van)][nearestnb$Rownr,DateRemoved] - kabelset[!is.na(Coo_X_van)][nearestnb[,eval(parse(text=kolom))],DateAdded])]
  }
  
  vervTF <- data.frame(Log=(rowSums((ifelse(is.na(nearestnb),F,T) & ifelse(is.na(tdiff),F,tdiff >  config$vervdiff$min & tdiff < config$vervdiff$max))[,-c(1:2)])>0))
  switch(config$set,
         NOR={vervTF$Index   <-  nearestnb$Index
              setDT(vervTF); setkey(vervTF,Index)
              vervTF <- merge(kabelset,vervTF,all=T)[,list(Index,Log)]
         },
         BAR={vervTF$ID_BAR   <-  nearestnb$ID_BAR
              setDT(vervTF); setkey(vervTF,ID_BAR)
              vervTF <- merge(kabelset,vervTF,all=T)[,list(ID_BAR,Log)]
         })
  
  switch(config$set,
         NOR={
           kabelset$Status_ID[is.na(kabelset$Status_ID)] <- 0
           
           is.verv  <- ((kabelset$Status_ID ==  "Length_changed") | (kabelset$Status_ch %in%  config$Status_ch)
                         | ((!is.na(kabelset$Coo_X_van)) & (kabelset$Status_ID=="Removed") & vervTF$Log))
           return(is.verv)  
         },
         BAR={
           
           is.verv  <- ((kabelset$Status_ID ==  "Length_changed") | (kabelset$Status_ch %in%  config$Status_ch)
                        | ((!is.na(kabelset$Coo_X_van)) & (kabelset$Status_ID=="Removed") & vervTF$Log))
           return(is.verv)
         })
  
  
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

# Functie om alle proxymethoden te runnen -------------------------------------------------------
runall = function(){
  
  for(method in c("PC","XY","TOPO"))  {
    for(set in c("NOR","BAR")){
      print(paste(method,set))
      AHA_Proxy_KA_BAR_NOR(method,set)
    }
  }
}