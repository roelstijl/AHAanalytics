.First = function ()
{
  # Load user specific things and sources functions
  # Source all the functions
  
  cat("Loading settings for AHA project (Alliander & Bearingpoint), on failure please reload packages\n
      Modify .Rprofile to change these settings\n\n")  
  
  options(stringsAsFactors = FALSE)
  source("AHA_Visual_RDS_to_GPS.R")
  source("AHA_Data_Import.R")
  source("AHA_Visual_Google_Maps_Plot.R")
  source("AHA_Data_Batch_Processing.R")
  source("AHA_Data_NOR_Log.R")
  source('AHA_Data_Determine_PC.R')
  source("AHA_Data_BAR_GEOMETRY.R")

  # Determine settings based on computer
  settings = data.frame(1)
  # Laptop Roel Stijl Bearingpoint
  if (Sys.info()["nodename"] =="NLAMS4043734X") {
    settings[,"Bron_Datasets"] = "I:/2. Datasets/1. Alliander/AHAdata/0. Ongebruikte en brondata"
    #   settings[,"Bron_Datasets"] = "C:/Datasets/AHAdata/0. Ongebruikte en brondata"
    settings[,"Ruwe_Datasets"] = "C:/Datasets/AHAdata/1. Ruwe Datasets"
    settings[,"Input_Datasets"] = "C:/Datasets/AHAdata/2. Input Datasets"
    settings[,"Analyse_Datasets"] = "C:/Datasets/AHAdata/3. Analyse Datasets"
    settings[,"Visuals"] = "C:/Datasets/AHAdata/5. Visuals and Tableau workbooks"
    settings[,"Results"] = "C:/Datasets/AHAdata/6. Results"}
  
  # Laptop Roel Stijl Alliander
  else if (Sys.info()["nodename"] =="L-AW89JB") {
    settings[,"Bron_Datasets"] = "E:/2. Datasets/1. Alliander/AHAdata/0. Ongebruikte en brondata"
    settings[,"Ruwe_Datasets"] = "E:/2. Datasets/1. Alliander/AHAdata/1. Ruwe Datasets"
    settings[,"Input_Datasets"] = "E:/2. Datasets/1. Alliander/AHAdata/2. Input Datasets"
    settings[,"Analyse_Datasets"] = "E:/2. Datasets/1. Alliander/AHAdata/3. Analyse Datasets"
    settings[,"Visuals"] = "E:/2. Datasets/1. Alliander/AHAdata/5. Visuals and Tableau workbooks"
    settings[,"Results"] = "E:/2. Datasets/1. Alliander/AHAdata/6. Results"}
  
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
    settings[,"Analyse_Datasets"] = "N:/Multivariate Analyse/AHAdata/3. Analyse Datasets"}
  
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
    settings[,"Analyse_Datasets"] = "N:/Multivariate Analyse/AHAdata/3. Analyse Datasets"}
    warning("Computer hostname unknown please check\n")
  }
  
  settings<<-settings


# Install required packages if not installed already
  packages = c("xlsxjars", "xlsx", "plyr","Rserve","tcltk2","shiny","foreach","XML","hash",
               "data.table","pracma","plotGoogleMaps","lubridate","PBSmapping")

# Install if not present
if(FALSE)  sapply(packages,function(x) install.packages(x))

# Download from ZIP if not present
if(FALSE)  sapply(packages,1,function(x) download.packages(x,paste0(settings$Ruwe_Datasets,"/0. Packages")))

# Install from ZIP if not present
if(FALSE) sapply(list.files(paste0(settings$Ruwe_Datasets,"/0. Packages"),full.names =TRUE),1,function(x) install.packages(x))

# require packages
sapply(packages,function(x) suppressMessages(require(x)))
  
  cat("Loaded settings, built by R Stijl (Bearingpoint), J Heres (Alliander)")  
}