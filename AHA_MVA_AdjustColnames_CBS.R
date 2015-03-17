#Written by Michiel Musterd - 13-09-2015
#This function adjusts the column names for the coupled buurt,wijk,gem file of CBS such
#that the column names can be easily recognized as coming from the buurt or wijk set

adjustColNames_CBS=function(){
  
  #Load the data table
  SetName=paste0(settings$Ruwe_Datasets,"/15. CBS/buurt_wijk_gem_shp.Rda")
  SetNameOut=paste0(settings$Ruwe_Datasets,"/15. CBS/buurt_wijk_gem_shp_adjustedNames.Rda")
  SetNameCheck=load(SetName)
  Set=get(SetNameCheck)
  
  #grep the desired column names based on their start and end
  buurt_Names=names(Set)[grepl("^i.", names(Set)) & grepl(".1$", names(Set))]
  wijk_Names=names(Set)[grepl("^i.", names(Set)) & !grepl(".1$", names(Set))]


  #adjust the names by cutting of the beginning and end 
  buurt_Names_Adjusted=paste0(substr(buurt_Names,3,nchar(buurt_Names)-2),"_BU")
  wijk_Names_Adjusted=paste0(substr(wijk_Names,3,nchar(wijk_Names)),"_WK")


  #replace the old names with the new
  setnames(Set,buurt_Names,buurt_Names_Adjusted)
  setnames(Set,wijk_Names,wijk_Names_Adjusted)
  
  save(Set,file=SetNameOut)
  
  return(names(Set))
  
  
  
}