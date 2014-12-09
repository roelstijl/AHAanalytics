nettopo_hld= function(){
  require(data.table)
  load(file = "AHA_Proxy_partial_data_2014-11-17.Rda")
  MS_stations <- fread("MS Stations.txt") 
  MS_hld <- fread("MS hoofdleidingen.csv", sep = ",") 
  
  setnames(MS_hld,as.character(MS_hld[1,])) #set names first row (headers) to characters 
  setnames(MS_stations,as.character(MS_stations[1,])) #set names first row (headers) to characters 
  setnames(MS_stations,"Nummer behuizing", "Stations_behuizing") #change header name "Nummer_behuizing" to "Stations_behuizing"
  
  nettopo_new <- merge(nettopo$EAN_to_HLD, MS_stations[,c("Stations_behuizing","Routenaam", "Id"), with = FALSE], by = "Stations_behuizing", all.x = TRUE) #merge set "EAN_to_HLD" with "MS_stations" with names columns 
  setnames(MS_hld,"Nummer", "ID_Hoofdleiding") #change header name "Nummer" naar "ID_hoofdleiding"
  setnames(MS_hld, as.character(MS_hld[1])) #copy first row names to column headers 
  MS_hld1 <- MS_hld[-1,] #delete first row 
  View(MS_hld1)
  MS_hld2 <- MS_hld1[-c(52292:55574),] #delete corrupt rows (3282)
  fix(MS_hld2)
  barplot(table(table(MS_hld2$Id))) #check corruption
  
  View(MS_hld2[duplicated(MS_hld2$Id),])
  MS_hld2 = data.table(MS_hld2); setkey(MS_hld2, Id); MS_hld3 = unique(MS_hld2)
  
  write.csv(nettopo_MSRing_hld, file = "Nettopo_MSRing_hld.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
  
}