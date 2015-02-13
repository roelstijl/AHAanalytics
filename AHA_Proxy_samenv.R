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
  
  if(!exists("storingen")){load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))}
  
  koppellijst =list()
  
  for(klasse in c("LSkabels","LSmoffen","MSmoffen","MSkabels")){
    
    koppellijst[klasse] <- rbind(cbind(rbindlist(proxy_res$PC[[klasse]][which(ldply(proxy_res$PC[[klasse]],nrow)$V1>0)]),method="PC"),
                                 cbind(rbindlist(proxy_res$XY[[klasse]][which(ldply(proxy_res$XY[[klasse]],nrow)$V1>0)]),method="XY"),
                                 cbind(rbindlist(proxy_res$TOPO[[klasse]][which(ldply(proxy_res$TOPO[[klasse]],nrow)$V1>0)]),method="TOPO"))
      
    print("klaar")
  }
  
  
}


