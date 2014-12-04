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

# Install required packages if not installed already
if (FALSE){
install.packages(c("xlsxjars", "xlsx"))
install.packages("plyr")
install.packages("Rserve"); 
install.packages("shiny");
install.packages("XML");
install.packages("hash")
install.packages("data.table");
install.packages("pracma") 
install.packages("plotGoogleMaps")
install.packages("lubridate")
install.packages("PBSmapping")
install.packages("foreach")
}
  
# Activate some scripts that might be usefull
suppressMessages(require("foreach"))
suppressMessages(require("parallel"))
suppressMessages(require("xlsx"))
suppressMessages(require("plyr"))
suppressMessages(require("lubridate"))
suppressMessages(require("shiny"))
suppressMessages(require("data.table"))
suppressMessages(library("pracma"))
suppressMessages(library("hash"))
suppressMessages(library("plotGoogleMaps"))
suppressMessages(require("PBSmapping"))
suppressMessages(require("maptools"))
suppressMessages(require("stringr"))
suppressMessages(require("rgeos"))


# Determine settings based on computer
  settings = data.frame(1)
# Laptop Roel Stijl Bearingpoint
if (Sys.info()["nodename"] =="NLAMS4043734X") {
  settings[,"Bron_Datasets"] = "I:/2. Datasets/1. Alliander/AHAdata/0. Ongebruikte en brondata"
  settings[,"Ruwe_Datasets"] = "C:/Datasets/AHAdata/1. Ruwe Datasets"
  settings[,"Input_Datasets"] = "C:/Datasets/AHAdata/2. Input Datasets"
  settings[,"Analyse_Datasets"] = "C:/Datasets/AHAdata/3. Analyse Datasets"
  settings[,"Visuals"] = "C:/Datasets/AHAdata/5. Visuals and Tableau workbooks"
  settings[,"Results"] = "C:/Datasets/AHAdata/6. Results"}

# Laptop Jacco Heres Alliander
else if (Sys.info()["nodename"] =="L-AW23JB") {
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
  settings[,"Bron_Datasets"] = paste0(substr(getwd(),1,nchar(getwd())-13), "/AHAdata/0. Ongebruikte en brondata")
  settings[,"Ruwe_Datasets"] = paste0(substr(getwd(),1,nchar(getwd())-13), "/AHAdata/1. Ruwe Datasets")
  settings[,"Input_Datasets"] = paste0(substr(getwd(),1,nchar(getwd())-13),"/AHAdata/2. Input Datasets")
  settings[,"Analyse_Datasets"] = paste0(substr(getwd(),1,nchar(getwd())-13),"/AHAdata/3. Analyse Datasets")
}

settings<<-settings
  
cat("Loaded settings, built by R Stijl (Bearingpoint), J Heres (Alliander)")  
}