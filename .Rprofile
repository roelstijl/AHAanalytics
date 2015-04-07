.First = function (install.p=F,download.p=F,install.p.zip=F)
{
  # Load user specific things and sources function  
  cat("Loading settings for AHA project (Alliander & Bearingpoint), on failure please reload packages\n
      Modify .Rprofile to change these settings\n\n")  
  options(stringsAsFactors = FALSE)
  
  # Save settings to global variable space for access later
  settings <<- load_settings()
  
  # Install required packages if not .Firinstalled already -------------------------------
  packages = c("xlsxjars", "xlsx", "plyr","Rserve","tcltk2","shiny","foreach","hash","parallel","doParallel","maptools","RANN","proj4","tools",
               "data.table","iterators","pracma","plotGoogleMaps","lubridate","PBSmapping","reshape2","ggplot2","foreign","rgeos","stringr",
               "vcd","heplots")
  
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
  sourcefiles = c("AHA_Data_Import.R","AHA_Data_Batch_Processing.R","Tableau_Functions.R",
                  "AHA_Data_NOR_Log.R", 'AHA_Data_Geo_Functions.R', "AHA_Data_MVA_Preprocessing_Functions.R",
                  "AHA_Data_BAR_Log.R","AHA_Proxy_KA_Postprocessing.R","AHA_Extra_Functions.R",
                  "AHA_MVA_Coupling.R","AHA_MVA_ExtractCableData.R","AHA_MVA_Preprocessing_Functions.R",
                  "AHA_MVA_CorrelationTable.R","AHA_MVA_Analyse.R")

  l_ply(sourcefiles,function(x) try(source(x)))
  
  # l_ply(ffiles,source)
  
  # Finnish -------------------------------------
  cat("Loaded settings, built by R Stijl (Bearingpoint), J Heres (Alliander)\n")
}

load_settings = function(){
  # Determine settings based on computer
  settings = list()
  settings$parallel = F
  
  # Laptop Roel Stijl Bearingpoint Folio 1040
  if (Sys.info()["nodename"] =="NLAMS4043734X") {
    settings$Bron_Datasets = "I:/2. Datasets/1. Alliander/AHAdata/0. Bron Datasets"
    settings$Ruwe_Datasets = "C:/Datasets/AHAdata/1. Ruwe Datasets"
    settings$Input_Datasets = "C:/Datasets/AHAdata/2. Input Datasets"
    settings$Analyse_Datasets = "C:/Datasets/AHAdata/3. Analyse Datasets"
    settings$Visuals = "C:/Datasets/AHAdata/5. Visuals and Tableau workbooks"
    settings$Results = "C:/Datasets/AHAdata/4. Analyse Resultaten"}
  
  # Laptop frank
  else if (Sys.info()["nodename"] =="L-AW09JM" ) {
    settings$Bron_Datasets = "C:/Data/Asset Health Data/0. Bron Datasets"
    settings$Ruwe_Datasets = "C:/Data/Asset Health Data/1. Ruwe Datasets"
    settings$Input_Datasets = "C:/Data/Asset Health Data/2. Input Datasets"
    settings$Analyse_Datasets = "C:/Data/Asset Health Data/3. Analyse Datasets"
    settings$Visuals = "C:/Data/Asset Health Data/5. Visuals and Tableau workbooks"
    settings$Results = "C:/Data/Asset Health Data/4. Analyse Resultaten"}
  
  # Laptop Roel Stijl Bearingpoint Zbook
  else if (Sys.info()["nodename"] =="NLAMS4043734Y") {
    settings$Bron_Datasets = "F:/1. Alliander/3. Asset Health Analytics/0. Bron Datasets"
    settings$Ruwe_Datasets = "F:/1. Alliander/3. Asset Health Analytics/1. Ruwe Datasets"
    settings$Input_Datasets = "F:/1. Alliander/3. Asset Health Analytics/2. Input Datasets"
    settings$Analyse_Datasets = "F:/1. Alliander/3. Asset Health Analytics/3. Analyse Datasets"
    settings$Visuals = "F:/1. Alliander/3. Asset Health Analytics/5. Visuals and Tableau workbooks"
    settings$Results = "F:/1. Alliander/3. Asset Health Analytics/4. Analyse Resultaten"}
  
  else if (Sys.info()["nodename"] =="NLAMS4043734Z") {
    settings$Bron_Datasets = "E:/1. Alliander/3. Asset Health Analytics/0. Bron Datasets"
    settings$Ruwe_Datasets = "E:/1. Alliander/3. Asset Health Analytics/1. Ruwe Datasets"
    settings$Input_Datasets = "E:/1. Alliander/3. Asset Health Analytics/2. Input Datasets"
    settings$Analyse_Datasets = "E:/1. Alliander/3. Asset Health Analytics/3. Analyse Datasets"
    settings$Visuals = "E:/1. Alliander/3. Asset Health Analytics/5. Visuals and Tableau workbooks"
    settings$Results = "E:/1. Alliander/3. Asset Health Analytics/4. Analyse Resultaten"}
  
  # Laptop Roel Stijl Alliander
  else if (Sys.info()["nodename"] =="L-AW89JB") {
    settings$Bron_Datasets = "E:/1. Alliander/3. Asset Health Analytics/0. Bron Datasets"
    settings$Ruwe_Datasets = "E:/1. Alliander/3. Asset Health Analytics/1. Ruwe Datasets"
    settings$Input_Datasets = "E:/1. Alliander/3. Asset Health Analytics/2. Input Datasets"
    settings$Analyse_Datasets = "E:/1. Alliander/3. Asset Health Analytics/3. Analyse Datasets"
    settings$Visuals = "E:/1. Alliander/3. Asset Health Analytics/5. Visuals and Tableau workbooks"
    settings$Results = "E:/1. Alliander/3. Asset Health Analytics/4. Analyse Resultaten"
    .libPaths ("C:/Data/R")}
  
  # Laptop Michiel Musterd BearingPoint Folio 1040
  else if (Sys.info()["nodename"] =="NLAMS4044583A") {
    settings$Bron_Datasets = "E:/1. Alliander Datasets/2. Asset health analytics/0. Bron Datasets"
    settings$Ruwe_Datasets = "E:/1. Alliander Datasets/2. Asset health analytics/1. Ruwe Datasets"
    settings$Input_Datasets = "E:/1. Alliander Datasets/2. Asset health analytics/2. Input Datasets"
    settings$Analyse_Datasets = "-"
    settings$Visuals = "-"
    settings$Results = "-"
    settings$Testcodes ="C:/Users/michiel.musterd/Documents/Alliander project/5. TestCodes"
    .libPaths ("C:/Data/R")}
  
  # Laptop Michiel Musterd BearingPoint zBook
  else if (Sys.info()["nodename"] =="NLAMS4044583X") {
    settings$Bron_Datasets = "D:/1. Alliander Datasets/2. Asset health analytics/0. Bron Datasets"
    settings$Ruwe_Datasets = "D:/1. Alliander Datasets/2. Asset health analytics/1. Ruwe Datasets"
    settings$Input_Datasets = "D:/1. Alliander Datasets/2. Asset health analytics/2. Input Datasets"
    settings$Analyse_Datasets = "D:/1. Alliander Datasets/2. Asset health analytics/3. Analyse Datasets"
    settings$Visuals = "-"
    settings$Results = "-"
    settings$Testcodes ="C:/Users/michiel.musterd/Documents/Alliander project/5. TestCodes"
    .libPaths ("C:/Users/michiel.musterd/Documents/R")}
  
  # R Server
  else if (Sys.info()["nodename"] =="SP0651") {
    settings$Bron_Datasets = "E:/1. Programmeerwerk/Multivariate Analyse/AHAdata/0. Bron Datasets"
    settings$Ruwe_Datasets = "E:/1. Programmeerwerk/Multivariate Analyse/AHAdata/1. Ruwe Datasets"
    settings$Input_Datasets = "E:/1. Programmeerwerk/Multivariate Analyse/AHAdata/2. Input Datasets"
    settings$Analyse_Datasets = "E:/1. Programmeerwerk/Multivariate Analyse/AHAdata/3. Analyse Datasets"
    settings$Visuals = "E:/1. Programmeerwerk/Multivariate Analyse/AHAdata/5. Visuals and Tableau workbooks"
    settings$Results = "E:/1. Programmeerwerk/Multivariate Analyse/AHAdata/4. Analyse Resultaten"
    .libPaths ("E:/1. Programmeerwerk/R packages")}
  
  # Desktop Jacco
  else if (Sys.info()["nodename"] =="D-AW15BX") {
    settings$Bron_Datasets = "C:/Data/AHAdata/0. Bron Datasets"
    settings$Ruwe_Datasets = "C:/Data/AHAdata/1. Ruwe Datasets"
    settings$Input_Datasets = "C:/Data/AHAdata/2. Input Datasets"
    settings$Analyse_Datasets = "C:/Data/AHAdata/3. Analyse Datasets"
    settings$Visuals = "C:/Data/AHAdata/5. Visuals and Tableau workbooks"
    settings$Results = "C:/Data/AHAdata/4. Analyse Resultaten"}
  
  # Laptop Jacco Heres Alliander
  else if (Sys.info()["nodename"] =="L-AW23JB") {
    settings$Bron_Datasets = "N:/Multivariate Analyse/AHAdata/0. Bron Datasets"
    settings$Ruwe_Datasets = "N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets"
    settings$Input_Datasets = "N:/Multivariate Analyse/AHAdata/2. Input Datasets"
    settings$Analyse_Datasets = "N:/Multivariate Analyse/AHAdata/3. Analyse Datasets"}
  
  # Citrix server liander
  else if (Sys.info()["nodename"] =="ST0067") {
    settings$Bron_Datasets = "N:/Multivariate Analyse/AHAdata/0. Bron Datasets"
    settings$Ruwe_Datasets = "N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets"
    settings$Input_Datasets = "N:/Multivariate Analyse/AHAdata/2. Input Datasets"
    settings$Analyse_Datasets = "N:/Multivariate Analyse/AHAdata/3. Analyse Datasets"
    memory.limit(32000)}
  
  # Chris Mutsaerts (oude laptop)
  else if (Sys.info()["nodename"] =="NLAMS4044343A") {
    settings$Bron_Datasets = "F:/2. Datasets/1. Alliander/AHAdata/0. Bron Datasets"
    settings$Ruwe_Datasets = "F:/2. Datasets/1. Alliander/AHAdata/1. Ruwe Datasets"
    settings$Input_Datasets = "F:/2. Datasets/1. Alliander/AHAdata/2. Input Datasets"
    settings$Analyse_Datasets = "F:/2. Datasets/1. Alliander/AHAdata/3. Analyse Datasets"
    settings$Visuals = "F:/2. Datasets/1. Alliander/AHAdata/5. Visuals and Tableau workbooks"
    settings$Results = "F:/2. Datasets/1. Alliander/AHAdata/4. Analyse Resultaten"  }
  
  # Laptop Pieter Stel en overig
  else{
    settings$Bron_Datasets = "N:/Multivariate Analyse/AHAdata/0. Bron Datasets"
    settings$Ruwe_Datasets = "N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets"
    settings$Input_Datasets = "N:/Multivariate Analyse/AHAdata/2. Input Datasets"
    settings$Analyse_Datasets = "N:/Multivariate Analyse/AHAdata/3. Analyse Datasets"
    warning("Computer hostname unknown please check\n")}
  
  # Save settings to global variable space for access later  
  return(settings)
}