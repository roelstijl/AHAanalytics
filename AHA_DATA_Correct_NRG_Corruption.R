AHA_DATA_Correct_NRG_Corruption= function(){
  require(data.table)
  MS_stations <- fread(paste0(settings$Bron_Datasets,"/11. Nettopologie/MS_Stations.txt"),sep="\t") 
  MS_hld <- read.csv(paste0(settings$Bron_Datasets,"/11. Nettopologie/MS_hoofdleidingen.txt"),sep="\t",colClasses="character") 
  
  setnames(MS_stations,as.character(MS_stations[1,])) #set names first row (headers) to characters 
  MS_stations <- MS_stations[-1,] #delete first row 
  
  write.csv(MS_stations, file =paste0(settings$Bron_Datasets,"/11. Nettopologie/MS_Stations.csv"))
  write.csv(MS_hld, file =paste0(settings$Bron_Datasets,"/11. Nettopologie/MS_hoofdleidingen.csv"))
  
}