<<<<<<< HEAD
proxy_samenv  <- function(){
  setwd(settings$Analyse_Datasets)
  proxy_res      <- list() 
  myFile <- file.choose(); load(myFile);
  proxy_res$PC   <- assetsltb
  myFile <- file.choose(); load(myFile);
  proxy_res$XY   <- assetsltb
  myFile <- file.choose(); load(myFile);
  proxy_res$TOPO <- assetsltb
  rm(assetsltb)
=======
proxy_samenv  <- function(global=F){
  if(global){
    if(!(exists("proxy_res"))){
      setwd(settings$Analyse_Datasets)
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
    setwd(settings$Analyse_Datasets)
    proxy_res      <- list() 
    myFile <- file.choose(); load(myFile);
    proxy_res$PC   <- assetsltb
    myFile <- file.choose(); load(myFile);
    proxy_res$XY   <- assetsltb
    myFile <- file.choose(); load(myFile);
    proxy_res$TOPO <- assetsltb
    rm(assetsltb)
  }
>>>>>>> 2918abf6cbffe4809c6cb78bdba00b079f679124
  
# Inladen storingsdata -------
  if(!exists("storingen")){load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))}
  setkey(storingen$MS,ID_KLAK_Melding)
  
<<<<<<< HEAD
  koppellijst =list()
  
=======
# Configuratie-instellingen
config <- list()
config$LSkabels$comp <- c("Netkabel (GPLK)","Netkabel (kunststof)")
config$LSkabels$onbk <- c("","Anders, toelichten bij opm.")
config$LSmoffen$comp <- c("Mof (kunststof)","Mof (massa)")
config$LSmoffen$onbk <- c("","Anders, toelichten bij opm.")

config$MSkabels$comp <- c("Kabel (kunststof)","Kabel (papier-lood)")
config$MSkabels$onbk <- c("","Anders, toelichten bij opm.","Geen","Nog meten")
config$MSmoffen$comp <- c("Mof (kunststof)","Mof (massa)"," Mof (olie)","Overgangsmof (GPLK-XLPE)")
config$MSmoffen$onbk <- c("","Anders, toelichten bij opm.","Geen","Nog meten")

#Quick-fix------------------------------------
proxy_res$TOPO$LSmoffen = proxy_res$PC$LSmoffen[38] #moffen LS nog niet goed gekoppeld in proxy-methode
proxy_res$TOPO$MSmoffen = proxy_res$PC$MSmoffen[2] #moffen LS nog niet goed gekoppeld in proxy-methode

  koppellijst =list()  # Aanmaken koppellijst
  
# for loop over assetklasses
>>>>>>> 2918abf6cbffe4809c6cb78bdba00b079f679124
  for(klasse in c("LSkabels","LSmoffen","MSmoffen","MSkabels")){
    print(klasse)
    try(koppellijst[[klasse]] <- rbind(cbind(rbindlist(proxy_res$PC[[klasse]][which(ldply(proxy_res$PC[[klasse]],nrow)$V1>0)]),method="PC"),
                                       cbind(rbindlist(proxy_res$XY[[klasse]][which(ldply(proxy_res$XY[[klasse]],nrow)$V1>0)]),method="XY"),
                                       cbind(rbindlist(proxy_res$TOPO[[klasse]][which(ldply(proxy_res$TOPO[[klasse]],nrow)$V1>0)]),method="TOPO")));
    try(koppellijst[[klasse]] <- koppellijst[[klasse]][which(koppellijst[[klasse]]$koppelc)])
        
    formpaste                 <- paste0(paste(names(koppellijst[[klasse]])[!c(names(koppellijst[[klasse]])=="method")],collapse=" + ")," ~ method")
    koppellijst[[klasse]]     <- dcast.data.table(koppellijst[[klasse]],formula = formpaste,value.var="method")
    setkey(koppellijst[[klasse]],ID_KLAK_Melding)
    
    koppellijst[[klasse]]     <- storingen$MS[koppellijst[[klasse]],c("ID_KLAK_Melding","Datum_Verwerking_Gereed","Netcomponent",
                                                                      names(koppellijst[[klasse]])),with=F]
    koppellijst[[klasse]]$GIS_datum            <- ifelse(is.na(koppellijst[[klasse]]$Datum_Verwerking_Gereed),0,1)

    koppellijst[[klasse]]$PC  <- ifelse(koppellijst[[klasse]]$PC=="PC",3,0) #Omzetten naar punten
    koppellijst[[klasse]]$XY  <- ifelse(koppellijst[[klasse]]$XY=="XY",1,0) #Omzetten naar punten
    koppellijst[[klasse]]$TOPO<- ifelse(koppellijst[[klasse]]$TOPO=="TOPO",5,0) #Omzetten naar punten
    
<<<<<<< HEAD
    koppellijst$klasse = rbind(proxy_res$PC$klasse,proxy_res$XY$klasse,proxy_res$TOPO$klasse)
    
    print("klaar")
=======
    koppellijst[[klasse]]$Component            <- ifelse(koppellijst[[klasse]]$Netcomponent %in% config[[klasse]]$comp,2,
                                                         ifelse(koppellijst[[klasse]]$Netcomponent %in% config[[klasse]]$onbk,1,0))
    koppellijst[[klasse]]                      <- koppellijst[[klasse]][(koppellijst[[klasse]][,list(freq=length(unique(ID_unique))), by=ID_KLAK_Melding])][
                                                             ,c(names(koppellijst[[klasse]]),"freq"),with=F]
    koppellijst[[klasse]]$punten               <- rowSums(koppellijst[[klasse]][,c("XY","PC","TOPO","GIS_datum"),with=F],na.rm=T)/
                                                  koppellijst[[klasse]]$freq
>>>>>>> 2918abf6cbffe4809c6cb78bdba00b079f679124
  }
  
  return(koppellijst)
  print("klaar")
}


# koppellijst <- lapply(koppellijstall,function(x){x<-x[,c("ID_KLAK_Melding","ID_unique","ID_NAN","punten"),with=F]})
# save(koppellijst, file=paste0(settings$Input_Datasets,"/3. Doelvariabele/Koppellijst_NOR.Rda"))
