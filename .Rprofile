.First = function (install.p=F,download.p=F,install.p.zip=F)
{
  # Load user specific things and sources function  
  # Source all the functions
  
  cat("Loading settings for AHA project (Alliander & Bearingpoint), on failure please reload packages\n
      Modify .Rprofile to change these settings\n\n")  
  
  options(stringsAsFactors = FALSE)

  # Determine settings based on computer
  settings = data.frame(1)
  # Laptop Roel Stijl Bearingpoint Folio 1040
  if (Sys.info()["nodename"] =="NLAMS4043734X") {
    settings[,"Bron_Datasets"] = "I:/2. Datasets/1. Alliander/AHAdata/0. Ongebruikte en brondata"
    #   settings[,"Bron_Datasets"] = "C:/Datasets/AHAdata/0. Ongebruikte en brondata"
    settings[,"Ruwe_Datasets"] = "C:/Datasets/AHAdata/1. Ruwe Datasets"
    settings[,"Input_Datasets"] = "C:/Datasets/AHAdata/2. Input Datasets"
    settings[,"Analyse_Datasets"] = "C:/Datasets/AHAdata/3. Analyse Datasets"
    settings[,"Visuals"] = "C:/Datasets/AHAdata/5. Visuals and Tableau workbooks"
    settings[,"Results"] = "C:/Datasets/AHAdata/6. Results"}
  
  if (Sys.info()["nodename"] =="NLAMS4043734Y") {
    settings[,"Bron_Datasets"] = "I:/2. Datasets/1. Alliander/AHAdata/0. Ongebruikte en brondata"
    settings[,"Ruwe_Datasets"] = "C:/Datasets/AHAdata/1. Ruwe Datasets"
    settings[,"Input_Datasets"] = "C:/Datasets/AHAdata/2. Input Datasets"
    settings[,"Analyse_Datasets"] = "C:/Datasets/AHAdata/3. Analyse Datasets"
    settings[,"Visuals"] = "C:/Datasets/AHAdata/5. Visuals and Tableau workbooks"
    settings[,"Results"] = "C:/Datasets/AHAdata/6. Results"}
  
  # Laptop Roel Stijl Alliander
  else if (Sys.info()["nodename"] =="L-AW89JB") {
    settings[,"Bron_Datasets"] = "E:/2. Datasets/1. Alliander/AHAdata/0. Ongebruikte en brondata"
    settings[,"Ruwe_Datasets"] = "E:/AHAdata/1. Ruwe Datasets"
    settings[,"Input_Datasets"] = "E:/AHAdata/2. Input Datasets"
    settings[,"Analyse_Datasets"] = "E:/2. Datasets/1. Alliander/AHAdata/3. Analyse Datasets"
    settings[,"Visuals"] = "E:/2. Datasets/1. Alliander/AHAdata/5. Visuals and Tableau workbooks"
    settings[,"Results"] = "E:/2. Datasets/1. Alliander/AHAdata/6. Results"
    .libPaths ("C:/Data/R")}
  
  # Desktop Jacco
  else if (Sys.info()["nodename"] =="D-AW15BX") {
    settings[,"Bron_Datasets"] = "F:/2. Datasets/1. Alliander/AHAdata/0. Ongebruikte en brondata"
    settings[,"Ruwe_Datasets"] = "F:/2. Datasets/1. Alliander/AHAdata/1. Ruwe Datasets"
    settings[,"Input_Datasets"] = "F:/2. Datasets/1. Alliander/AHAdata/2. Input Datasets"
    settings[,"Analyse_Datasets"] = "F:/2. Datasets/1. Alliander/AHAdata/3. Analyse Datasets"
    settings[,"Visuals"] = "F:/2. Datasets/1. Alliander/AHAdata/5. Visuals and Tableau workbooks"
    settings[,"Results"] = "F:/2. Datasets/1. Alliander/AHAdata/6. Results"}
  
  # Laptop Jacco Heres Alliander
  else if (Sys.info()["nodename"] =="L-AW23JB") {
    settings[,"Bron_Datasets"] = "N:/Multivariate Analyse/AHAdata/0. Ongebruikte en brondata"
    settings[,"Ruwe_Datasets"] = "N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets"
    settings[,"Input_Datasets"] = "N:/Multivariate Analyse/AHAdata/2. Input Datasets"
    settings[,"Analyse_Datasets"] = "N:/Multivariate Analyse/AHAdata/3. Analyse Datasets"}
  
  # Citrix server liander
  else if (Sys.info()["nodename"] =="ST0067") {
    settings[,"Bron_Datasets"] = "N:/Multivariate Analyse/AHAdata/0. Ongebruikte en brondata"
    settings[,"Ruwe_Datasets"] = "N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets"
    settings[,"Input_Datasets"] = "N:/Multivariate Analyse/AHAdata/2. Input Datasets"
    settings[,"Analyse_Datasets"] = "N:/Multivariate Analyse/AHAdata/3. Analyse Datasets"
    memory.limit(32000)}
  
  # Christopher
  else if (Sys.info()["nodename"] =="NLAMS4044343A") {
    settings[,"Bron_Datasets"] = paste0(substr(getwd(),1,nchar(getwd())-13), "/AHAdata/0. Ongebruikte en brondata")
    settings[,"Ruwe_Datasets"] = paste0(substr(getwd(),1,nchar(getwd())-13), "/AHAdata/1. Ruwe Datasets")
    settings[,"Input_Datasets"] = paste0(substr(getwd(),1,nchar(getwd())-13),"/AHAdata/2. Input Datasets")
    settings[,"Analyse_Datasets"] = paste0(substr(getwd(),1,nchar(getwd())-13),"/AHAdata/3. Analyse Datasets")
  }
  
  # Laptop Pieter Stel en overig
  else{
    settings[,"Bron_Datasets"] = "N:/Multivariate Analyse/AHAdata/0. Ongebruikte en brondata"
    settings[,"Ruwe_Datasets"] = "N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets"
    settings[,"Input_Datasets"] = "N:/Multivariate Analyse/AHAdata/2. Input Datasets"
    settings[,"Analyse_Datasets"] = "N:/Multivariate Analyse/AHAdata/3. Analyse Datasets"
    warning("Computer hostname unknown please check\n")}
  
  
  settings<<-settings

# Install required packages if not installed already -------------------------------
  packages = c("xlsxjars", "xlsx", "plyr","Rserve","tcltk2","shiny","foreach","hash","parallel","doParallel",
               "data.table","iterators","pracma","plotGoogleMaps","lubridate","PBSmapping","reshape2","ggplot2")

for (m in 1:length(packages)){
# Install if not present
if(install.p)  install.packages(packages[m])

# Download from ZIP if not present
if(download.p)  download.packages(packages[m],paste0(settings$Ruwe_Datasets,"/0. Packages"))

# Install from ZIP if not present
if(install.p.zip) install.packages(list.files(paste0(settings$Ruwe_Datasets,"/0. Packages"),full.names =TRUE)[m])
}
# require packages
for (m in 1:length(packages)){
suppressMessages(library(packages[m],character.only=TRUE))
}

# Source some functions --------------------------------
source("AHA_Visual_RDS_to_GPS.R")
source("AHA_Data_Import.R")
source("AHA_Data_Batch_Processing.R")
source("AHA_Data_NOR_Log.R")
source('AHA_Data_Geo_Functions.R')
source("AHA_Data_NOR_Log_Postprocessing.R")
source("AHA_Proxy_KA_Preprocessing.R")
source("AHA_Data_BAR_Log_Postprocessing.R")
source("AHA_Proxy_KA_Postprocessing.R")
source("AHA_Extra_Functions.R")

# l_ply(ffiles,source)

# Finnish -------------------------------------
  cat("Loaded settings, built by R Stijl (Bearingpoint), J Heres (Alliander)")  
}