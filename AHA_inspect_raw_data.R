AHA_inspect_raw_data = function(dataset,header,headerfile){
  shinyfolder  = "x. Shiny"

  save(dataset,file=paste0(shinyfolder,"/dataset.Rda"))
  save(header,file=paste0(shinyfolder,"/header.Rda"))
  
  cat("Starting shiny .\n")  
  header = runApp(shinyfolder)
  
  cat("Closing shiny .\n")
  file.rename(paste0(shinyfolder ,"/header.xlsx"),paste0(shinyfolder,"/",headerfile));
  
}

