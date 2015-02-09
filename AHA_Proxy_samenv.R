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
  
  if(!exists("storingen")){load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))}
  
  koppellijst =list()
  
  for(klasse in c("LSkabels","LSmoffen","MSmoffen","MSkabels")){
    
    koppellijst$klasse = rbind(proxy_res$PC$klasse,proxy_res$XY$klasse,proxy_res$TOPO$klasse)
    
    print("klaar")
  }
  
  
}
