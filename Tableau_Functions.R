Save_Tableau_assets = function(dataset,folder){
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_dataset_",dataset,".Rda"))
# Loop over all components of dataset
for (n in names(dataset))
{
  # Transform the RDS collumns to something more usable
  CooNames = cbind(names(dataset[[n]])[grepl("Coo_X",names(dataset[[n]]))],names(dataset[[n]])[grepl("Coo_Y",names(dataset[[n]]))])
  for (Co in 1:nrow(CooNames)){
    dataset[[n]] = Convert_Coordinate_System(dataset[[n]],from = "RDS", to = "lonlat",
                                       xcol = CooNames[Co,1],ycol = CooNames[Co,2],
                                       xcolout = strrep(CooNames[Co,1],"Coo_X","Lon"), ycolout = strrep(CooNames[Co,2],"Coo_Y","Lat"))
  }
  if (any(n %in% c("MSkabels","LSkabels"))) {
    dataset[[n]] = rbind(dataset[[n]][,Lon:=c(Lon_van)],dataset[[n]][,Lon:=c(Lon_van)])
    dataset[[n]][,Lat:=c(Lat_van[1:(nrow(dataset[[n]])/2)],Lat_naar[1:(nrow(dataset[[n]])/2)])]
  }
}  

SaveWrap(rbind(dataset$LSkabels,dataset$MSkabels,fill=T),paste0(settings$Analyse_Datasets,"/4. Tableau sets/",folder,"/Kabels_",folder,".csv"))
SaveWrap(rbind(dataset$LSmoffen,dataset$MSmoffen,fill=T),paste0(settings$Analyse_Datasets,"/4. Tableau sets/",folder,"/Moffen_",folder,".csv"))

}

Save_Tableau_storingen = function(folder){
  dataset = LoadWrap(filechoose=T)
  
  # Loop over all components of dataset
  for (n in names(dataset))
  {
    # Transform the RDS collumns to something more usable
    CooNames = cbind(names(dataset[[n]])[grepl("Coo_X",names(dataset[[n]]))],names(dataset[[n]])[grepl("Coo_Y",names(dataset[[n]]))])
    for (Co in 1:nrow(CooNames)){
      if (!is.na(CooNames[1])){
      
      dataset[[n]] = Convert_Coordinate_System(dataset[[n]],from = "RDS", to = "lonlat",
                                               xcol = CooNames[Co,1],ycol = CooNames[Co,2],
                                               xcolout = strrep(CooNames[Co,1],"Coo_X","Lon"), ycolout = strrep(CooNames[Co,2],"Coo_Y","Lat"))
    }}
    
    SaveWrap(dataset[[n]],paste0(settings$Analyse_Datasets,"/4. Tableau sets/",folder,"/",folder,n,".csv"))
  }   
  SaveWrap(rbind(dataset$LS,dataset$MS,fill=T),paste0(settings$Analyse_Datasets,"/4. Tableau sets/",folder,"/",folder,"MS_LS.csv"))
}

Save_Tableau_dataset = function(){
  dataset = LoadWrap()
  
  NANset = LoadWrap("E:/1. Alliander/3. Asset Health Analytics/3. Analyse Datasets/4. Tableau sets/ELCVERBINDINGSKNOOPPUNTEN_1501_Tableau.Rda")
  
  setkey(NANset,ID_NAN)
  setkey(dataset,ID_NAN)
  
  dataset = dataset[,In_Recent_NOR := ID_NAN %in% NANset$ID_NAN]
  
    # Transform the RDS collumns to something more usable
    CooNames = cbind(names(dataset)[grepl("Coo_X",names(dataset))],names(dataset)[grepl("Coo_Y",names(dataset))])
    for (Co in 1:nrow(CooNames)){
      if (!is.na(CooNames[1])){
        
        dataset = Convert_Coordinate_System(dataset,from = "RDS", to = "lonlat",
                                                 xcol = CooNames[Co,1],ycol = CooNames[Co,2],
                                                 xcolout = strrep(CooNames[Co,1],"Coo_X","Lon"), ycolout = strrep(CooNames[Co,2],"Coo_Y","Lat"))
      }}
    
    SaveWrap(dataset,paste0(settings$Analyse_Datasets,"/4. Tableau sets/",file_path_sans_ext(basename(settings$Last_Load)),"_Tableau.csv"))
}