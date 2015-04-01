proxy_samenv  <- function(global=F,set){
  #Deze functie gebruikt drie proxy-uitkomsten (van PC, XY en TOPO) 
  #en voegt deze samen tot één lange lijst. Er  worden punten gegeven
  #aan elke koppeling op basis van de gebruikte koppelmethode, wel/geen
  #Overeenstem,ming met de beschrijving in KLAK en het wel of niet beschikbaar
  #zijn van een GIS-mutatiedatum. Ook wordt er een frequentietabel
  #gemaakt van het aantal gevonden en gekoppelde assets
  if(global){
    if(!(exists("proxy_res"))){
      setwd(paste0(settings$Analyse_Datasets,"/1. KA Proxy"))
      proxy_res      <- list() 
      myFile <- file.choose(); load(myFile);
      proxy_res$PC   <- assetsltb
      myFile <- file.choose(); load(myFile);
      proxy_res$XY   <- assetsltb
      myFile <- file.choose(); load(myFile);
      proxy_res$TOPO <- assetsltb
      proxy_res <<- proxy_res
      rm(assetsltb)
    }
  }else{
    setwd(paste0(settings$Analyse_Datasets,"/1. KA Proxy"))
    proxy_res      <- list() 
    myFile <- file.choose(caption = "Selecteer Postcode file"); load(myFile);
    proxy_res$PC   <- assetsltb
    myFile <- file.choose(caption = "Selecteer XY file"); load(myFile);
    proxy_res$XY   <- assetsltb
    myFile <- file.choose(caption = "Selecteer Topologie file"); load(myFile);
    proxy_res$TOPO <- assetsltb
    rm(assetsltb)
  }

  
# Inladen storingsdata -------------------------------------
  if(!exists("storingen")){load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))}
  namechange(storingen$MS,"Omschrijving_Oorzaak2","Oorzaak2")
  storingen <- lapply(storingen, unique)
  setkey(storingen$MS,ID_KLAK_Melding);setkey(storingen$LS,ID_KLAK_Melding);
  storingen$MS <- unique(storingen$MS)
  setkey(storingen$LS,ID_KLAK_Melding);setkey(storingen$LS,ID_KLAK_Melding);  
  storingen$LS <- unique(storingen$LS)
  
  koppellijst = list()
  freqtabel   = data.frame(method=c("PC","TOPO","XY"))
  
# Configuratie-instellingen--------------------------------------------------
config <- list()
config$LSkabels$comp <- c("Netkabel (GPLK)","Netkabel (kunststof)")
config$LSkabels$onbk <- c("","Anders, toelichten bij opm.")
config$LSmoffen$comp <- c("Mof (kunststof)","Mof (massa)")
config$LSmoffen$onbk <- c("","Anders, toelichten bij opm.")

config$MSkabels$comp <- c("Kabel (kunststof)","Kabel (papier-lood)")
config$MSkabels$onbk <- c("","Anders, toelichten bij opm.","Geen","Nog meten")
config$MSmoffen$comp <- c("Mof (kunststof)","Mof (massa)"," Mof (olie)","Overgangsmof (GPLK-XLPE)")
config$MSmoffen$onbk <- c("","Anders, toelichten bij opm.","Geen","Nog meten")

config$graafschade   <- c("Graafwerkzaamheden oud","Graafwerkzaamheden vers")

#Quick-fix------------------------------------
#proxy_res$XY$MSkabels = proxy_res$TOPO$MSkabels[10071] #moffen LS nog niet goed gekoppeld in proxy-methode
#proxy_res$XY$MSmoffen = proxy_res$TOPO$MSmoffen[9963] #moffen LS nog niet goed gekoppeld in proxy-methode
#proxy_res$PC$MSkabels = proxy_res$TOPO$MSkabels[10071] #moffen LS nog niet goed gekoppeld in proxy-methode

koppellijst =list()  # Aanmaken koppellijst
  
# for loop over assetklasses

  for(klasse in c("LSkabels","LSmoffen","MSkabels","MSmoffen")){
    cat(klasse)
    voltage = substr(klasse,1,2)
    type    = substr(klasse,3,8)
    try(koppellijst[[klasse]] <- unique(rbind(
                                        cbind(rbindlist(proxy_res$PC[[klasse]][which(ldply(proxy_res$PC[[klasse]],nrow)$V1>0)],fill=T),method="PC"),
                                        cbind(rbindlist(proxy_res$XY[[klasse]][which(ldply(proxy_res$XY[[klasse]],nrow)$V1>0)],fill=T),method="XY"),
                                        cbind(rbindlist(proxy_res$TOPO[[klasse]][which(ldply(proxy_res$TOPO[[klasse]],nrow)$V1>0)],fill=T),method="TOPO"),fill=T)));
    assetsgevonden <- nrow(koppellijst[[klasse]])
    colname = paste(klasse,"gevonden");    freqtabel = cbind(freqtabel,Freq=data.frame(table(koppellijst[[klasse]]$method))$Freq);setnames(freqtabel,"Freq",colname)
    setkey(koppellijst[[klasse]],ID_KLAK_Melding)
    
     if(type=="moffen"){
       kabelklasse = paste0(voltage,"kabels")
       koppellijst[[klasse]]
       koppellijst[[klasse]]$is.verv2 <- pbapply(koppellijst[[klasse]],1,function(x) vervangingmoffen(x,koppellijst[[kabelklasse]])) #Check op moffen die vervangen zijn door een nieuw stuk kabel
       koppellijst[[klasse]]$koppelc  <- koppellijst[[klasse]]$in.timediff & (koppellijst[[klasse]]$is.verv | koppellijst[[klasse]]$is.verv2)
     }}
for(klasse in c("LSkabels","LSmoffen","MSkabels","MSmoffen")){
    try(koppellijst[[klasse]] <- koppellijst[[klasse]][which(koppellijst[[klasse]]$koppelc)]) #Neem alleen gekoppelde assets mee, die vervangen zijn en voldoen
    colname = paste(klasse,"gekoppeld");    freqtabel = cbind(freqtabel,Freq=data.frame(table(koppellijst[[klasse]]$method))$Freq);setnames(freqtabel,"Freq",colname)
    assetsgekoppeld <- nrow(koppellijst[[klasse]])
    
    formpaste                 <- paste0(paste(names(koppellijst[[klasse]])[!c(names(koppellijst[[klasse]])=="method")],collapse=" + ")," ~ method")
    koppellijst[[klasse]]     <- dcast.data.table(koppellijst[[klasse]],formula = formpaste,value.var="method")  #Converteer koppelmethode van long naar wide
    setkey(koppellijst[[klasse]],ID_KLAK_Melding)
    
    koppellijst[[klasse]]     <- storingen[[voltage]][koppellijst[[klasse]],c("ID_KLAK_Melding","Datum_Verwerking_Gereed","Netcomponent",
                                                                      names(koppellijst[[klasse]])),with=F]
    koppellijst[[klasse]]$GIS_datum            <- ifelse(is.na(koppellijst[[klasse]]$Datum_Verwerking_Gereed),0.5,1.5)
    
    if(type=="kabels"){
      koppellijst[[klasse]]$Storingsdatum        <- ifelse((koppellijst[[klasse]]$Rdiff > 0 & koppellijst[[klasse]]$Rdiff < 32)|
                                                           (koppellijst[[klasse]]$Ldiff > 0 & koppellijst[[klasse]]$Ldiff < 32)|
                                                           (koppellijst[[klasse]]$Sdiff > 0 & koppellijst[[klasse]]$Sdiff < 32),
                                                           2,1)}else{
      koppellijst[[klasse]]$Storingsdatum        <- ifelse((koppellijst[[klasse]]$Rdiff > 0 & koppellijst[[klasse]]$Rdiff < 32),2,1)
    }
    koppellijst[[klasse]]$Storingsdatum[is.na(koppellijst[[klasse]]$Storingsdatum)]  = 1

    koppellijst[[klasse]]$PC  <- ifelse(koppellijst[[klasse]]$PC=="PC",3,0) #Omzetten naar punten
    koppellijst[[klasse]]$XY  <- ifelse(koppellijst[[klasse]]$XY=="XY",1,0) #Omzetten naar punten
    koppellijst[[klasse]]$TOPO<- ifelse(koppellijst[[klasse]]$TOPO=="TOPO",5,0) #Omzetten naar punten
    
    #koppellijst$klasse = rbind(proxy_res$PC$klasse,proxy_res$XY$klasse,proxy_res$TOPO$klasse)
    
    koppellijst[[klasse]]$Component            <- ifelse(koppellijst[[klasse]]$Netcomponent %in% config[[klasse]]$comp,1.5,
                                                         ifelse(koppellijst[[klasse]]$Netcomponent %in% config[[klasse]]$onbk,1,0.5))
    if(set=="NOR"){
      koppellijst[[klasse]]                      <- koppellijst[[klasse]][(koppellijst[[klasse]][,list(freq=length(unique(ID_unique))), by=ID_KLAK_Melding])][
                                                             ,c(names(koppellijst[[klasse]]),"freq"),with=F]}
    else{
      koppellijst[[klasse]]                     <- koppellijst[[klasse]][(koppellijst[[klasse]][,list(freq=length(unique(ID_BAR))), by=ID_KLAK_Melding])][
                                                             ,c(names(koppellijst[[klasse]]),"freq"),with=F]}
    koppellijst[[klasse]]$punten               <- rowSums(koppellijst[[klasse]][,c("XY","PC","TOPO"),with=F],na.rm=T)*
                                                  koppellijst[[klasse]]$GIS_datum*
                                                  koppellijst[[klasse]]$Component*
                                                  koppellijst[[klasse]]$Storingsdatum/
                                                  koppellijst[[klasse]]$freq

    
    koppellijst[[klasse]]$Oorzaak              <- storingen[[voltage]][J(koppellijst[[klasse]]$ID_KLAK_Melding)]$Oorzaak2

    koppellijst[[klasse]]$Graafschade          <- (storingen[[voltage]][J(koppellijst[[klasse]]$ID_KLAK_Melding)]$Oorzaak2 %in% config$graafschade)
  
    print(paste("Klaar, aantal gevonden assets is",assetsgevonden,"aantal gekoppelde assets is",assetsgekoppeld))
  }
  View(freqtabel)
  filename  = paste0(settings$Analyse_Datasets,"/4. KA Proxy samengevoegd/Proxy_koppellijst_",gsub(":",".",paste0(Sys.time())),".Rda")
  filename2 = paste0(settings$Analyse_Datasets,"/4. KA Proxy samengevoegd/Proxy_frequentietabel_",gsub(":",".",paste0(Sys.time())),".xlsx")
  write.xlsx(freqtabel,file=filename2)
  save(koppellijst,file=filename)
  print("klaar")
}

vervangingmoffen = function(moffenlijst,kabellijst){
  if(moffenlijst["ID_KLAK_Melding"]=="3740842"){
    print("blah")
    print(paste(kabellijst[J(moffenlijst["ID_KLAK_Melding"]),]$DateAdded))
    print(moffenlijst["DateRemoved"])
  }
  test <-(sum(kabellijst[J(moffenlijst["ID_KLAK_Melding"]),]$DateAdded==moffenlijst["DateRemoved"])>0)
  return(test)}
