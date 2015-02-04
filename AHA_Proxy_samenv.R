proxy_samenv  <- function(){
  setwd(settings$Analyse_Datasets)
  proxy_res      <- list() 
  myFile <- file.choose(); load(myFile);
  proxy_res$PC   <- assetsltb
  myFile <- file.choose(); load(myFile);
  proxy_res$XY   <- assetsltb
  myFile <- file.choose(); load(myFile);
  proxy_res$TOPO <- assetsltb

  
}

