AHA_Data_Batch_Processing = function(range)
{
# Settings ---------------------
mode ="save"

# Processing of registered NAN-KLAK---------------
AHA_Data_Import("Validatie_data","Koppeling KLAK-NRG","Koppeling KLAK-NRG",mode)

# KLAK/GISmutaties processing ----------------
AHA_Data_Import("KLAK","KLAK_KOPPEL_MELDING_GROEP","KLAK_KOPPEL_MELDING_GROEP",mode)
AHA_Data_Import("KLAK","KLAK_LS","KLAK_LS",mode,"yes")
AHA_Data_Import("KLAK","KLAK_MS","KLAK_MS",mode,"yes")
AHA_Data_Import("GIS-mutaties","GISMUTATIE","GISMUTATIE",mode)
  
# BARlog processing ---------------
AHA_Data_Import("BARlog","MH_NRG_LS_HLD","MH_NRG_LS_HLD",mode)
AHA_Data_Import("BARlog","MH_NRG_LS_KABELS","MH_NRG_LS_KABELS",mode)
AHA_Data_Import("BARlog","MH_NRG_LS_MOFFEN","MH_NRG_LS_MOFFEN",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_HLD","MH_NRG_MS_HLD",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_KABELS","MH_NRG_MS_KABELS",mode)
AHA_Data_Import("BARlog","MH_NRG_MS_MOFFEN","MH_NRG_MS_MOFFEN",mode)

# Add the XY coordinates in the same way NOR has them
processXY("MH_NRG_MS_KABELS","beginend",atype="kabels")
processXY("MH_NRG_LS_KABELS","beginend",atype="kabels")
processXY("MH_NRG_MS_MOFFEN","position",atype="moffen")
processXY("MH_NRG_LS_MOFFEN","position",atype="moffen")

# Add the XY coordinates in a spatial file
processXY("MH_NRG_LS_KABELS","polygons",atype="kabels")
processXY("MH_NRG_MS_KABELS","polygons",atype="kabels")
processXY("MH_NRG_MS_MOFFEN","points",atype="moffen")
processXY("MH_NRG_LS_MOFFEN","points",atype="moffen")

# Add the PC_6 locations of the assets
processPC6("MH_NRG_LS_KABELS","van_naar")
processPC6("MH_NRG_MS_KABELS","van_naar")
processPC6("MH_NRG_MS_MOFFEN","punt")
processPC6("MH_NRG_LS_MOFFEN","punt")
  
# NOR processing ------------------
  folder="NOR"
  setfolder     = list.files(settings$Bron_Datasets,pattern=folder)[1]; 
  dir.create(paste0(settings$Ruwe_Datasets,"/",setfolder), showWarnings = FALSE)
  a=list.files(paste0(settings$Bron_Datasets,"/",setfolder),pattern = "ELCVERBINDINGEN.csv")

  for (n in substr(a[1:95],17,20)) {
   AHA_Data_Import("NOR",paste0("ELCVERBINDINGEN_",n),"ELCVERBINDINGEN",mode)
   AHA_Data_Import("NOR",paste0("ELCVERBINDINGSDELEN_",n),"ELCVERBINDINGSDELEN",mode)
   AHA_Data_Import("NOR",paste0("ELCVERBINDINGSKNOOPPUNTEN_",n),"ELCVERBINDINGSKNOOPPUNTEN",mode)
  }

AHA_Data_NOR_Log("ELCVERBINDINGEN")
AHA_Data_NOR_Log("ELCVERBINDINGSDELEN")
AHA_Data_NOR_Log("ELCVERBINDINGSKNOOPPUNTEN")

# Post processing
AHA_Data_NOR_Log_Postprocessing()
}

processXY = function(file,mode,atype){
  cat("Loading file\n");tic();
  load(paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,".Rda"));toc();
  cat(paste0("Starting ", mode," file: ",file, "\n"))
  
  veld = switch (file,
                 MH_NRG_MS_KABELS= "Ligging",
                 MH_NRG_LS_KABELS= "Ligging",
                 MH_NRG_MS_MOFFEN= "Locatie",
                 MH_NRG_LS_MOFFEN= "Lokatie")
  setnames(mindataset,veld,"veld")
  
  mindataset = switch (mode,
          polygons= SpatialPolygonsDataFrame(AHA_Data_BAR_GEOMETRY(mindataset$veld,mode,atype),data=mindataset[,veld:=NULL]),
          points  = AHA_Data_BAR_GEOMETRY(mindataset$veld,mode,atype,mindataset),
          cbind(mindataset,AHA_Data_BAR_GEOMETRY(mindataset$veld,mode,atype)))
  try(mindataset[,veld:=NULL]) 
  
  cat("saving\n")
  if (mode == "polygons" | mode == "points"){
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_Geospatial.Rda"))}
  else{
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_XY.Rda"))}
}


processPC6 = function(file,mode){
# Function calculates the PC6 of files-----------------
  cat("starting\n")
  a=1
  
  switch (mode,
        van_naar= {
                load(paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_XY.Rda"));
                datatable <<-mindataset; rm("mindataset")
                cat("Coordinates naar\n")
                AHA_Data_Determine_PC(x="Coo_X_naar",y="Coo_Y_naar",PC="PC_6_naar")
                cat("Coordinates van\n")
                AHA_Data_Determine_PC(x="Coo_X_van",y="Coo_Y_van",PC="PC_6_van",extrainfo=TRUE)
        },        
        punt= {load(paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_XY.Rda"));
               datatable <<-mindataset; rm("mindataset")
               AHA_Data_Determine_PC(extrainfo=TRUE)}
)
  mindataset = datatable; rm("datatable",envir=.GlobalEnv)
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/1. BARlog/",file,"_XY_PC6.Rda"))
}
