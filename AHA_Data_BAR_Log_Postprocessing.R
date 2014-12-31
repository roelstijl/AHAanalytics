AHA_Data_BAR_Log_Postprocessing  = function()
  {
# Load and prepare some data --------------------------------------------------
assets = list(); 
changes = list();
  
# Hoofdleidingen ------------------------------
cat("Processing HLD data\n")
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_MS_HLD.Rda"))
assets$MSHLD    = unique(mindataset)

load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_HLD.Rda"))
assets$LSHLD = unique(mindataset)

# Kabels --------------------------------------
cat("Processing HLD data\n")
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_MS_KABELS.Rda"))
assets$MSkabels = unique(mindataset)
rbind(assets$MSkabels,AHA_Data_BAR_GEOMETRY(assets$MSkabels$Ligging,"beginend"))
assets$MSkabels[,Ligging:=NULL]

# load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_KABELS.Rda"))
# assets$LSkabels = mindataset
# rbind(assets$LSkabels,AHA_Data_BAR_GEOMETRY(assets$LSkabels$Ligging,"beginend"))
# assets$LSkabels[,Ligging:=NULL]

# Moffen------------------------------ 
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_MOFFEN.Rda"))
assets$LSmoffen = unique(mindataset)
rbind(assets$LSmoffen,AHA_Data_BAR_GEOMETRY(assets$LSmoffen$Ligging,"position"))

load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_MS_MOFFEN.Rda"))
assets$MSmoffen = unique(mindataset)
rbind(assets$MSmoffen,AHA_Data_BAR_GEOMETRY(assets$MSmoffen$Ligging,"position"))

# hoofdleidingen koppel ----------------------
  cat("Loading verbindingen\n"); tic()
  load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGEN.Rda"))
  verbindingen = unique(setorder(masterdataset, "DateAdded"),by=c("ID_Verbinding","Beheerder"))
  
# kabels ---------------
  # Add the correct voltage levels
  toc(); cat("Loading verbindingsdelen\n"); tic()
  load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSDELEN.Rda"))
  toc(); cat("Beginning calculations\n"); tic()
  assets$kabels = masterdataset; rm("masterdataset")
  assets$kabels = merge(assets$kabels,Conv_voltage,by="Spanningsniveau",all.x=TRUE); 
  
  # Correct for missing PC6 naar
  XYinPC = AHA_Data_Determine_PC(assets$kabels[,c("Coo_X_naar","Coo_Y_naar","ID_unique"),with=FALSE],"PC_6","Coo_X_naar","Coo_Y_naar")
  save("XYinPC",file="XYinPC.Rda");
  assets$kabels[,PC_6_naar:=XYinPC$POSTCODE]; 
  remove("XYinPC")
  
  # Add the length changes
  setorder(changes,"Date","ID_unique");  i= 2*(1:(nrow(changes)/2)); 
  lengthch = data.table(changes$Lengte[i-1]-changes$Lengte[i]); 
  setnames(lengthch,"V1","Length_ch")
  changes = changes[i-1][lengthch$Length_ch!=0,c("ID_unique","Date"),with=FALSE];
  lengthch = merge(cbind(changes,lengthch[lengthch$Length_ch!=0]),assets$kabels,by="ID_unique",all.x=TRUE);
  remove("changes");
  setnames(lengthch,"Date","Date_Length_ch")
  lengthch$Status_ID = "Length changed"
  assets$kabels$Date_Length_ch=NA
  assets$kabels$Length_ch=NA
  assets$kabels = rbind(assets$kabels,lengthch); remove("lengthch");
  
  # Add the HLD
  assets$kabels = merge(assets$kabels,verbindingen[,c("ID_Verbinding","Beheerder","ID_Hoofdleiding"),with=FALSE],all.x=TRUE,by=c("ID_Verbinding","Beheerder"))
  toc()
  save(assets,file=paste0(settings$Input_Datasets,"/Asset_Data_NOR_assets_",Sys.Date(),".Rda"))
  
# assets$moffen --------------------------
  cat("Loading assets$moffen\n"); tic()
  
  load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSKNOOPPUNTEN.Rda"))
  toc(); cat("Beginning calculations\n"); tic()
  assets$moffen = masterdataset; rm("masterdataset")
  
  missing       = is.na(assets$moffen$ID_Verbinding)
  setnames(assets$kabels,c("Coo_X_naar","Coo_Y_naar"),c("Coo_X","Coo_Y"))
  assets$moffen = rbind(
    merge(assets$moffen[missing ],unique(assets$kabels[,c("Coo_X","Coo_Y","ID_Hoofdleiding"),with=FALSE]),by=c("Coo_X","Coo_Y"),all.x=TRUE),
    merge(assets$moffen[!missing],unique(verbindingen[,c("ID_Verbinding","Beheerder","ID_Hoofdleiding"),with=FALSE]),all.x=TRUE,by=c("ID_Verbinding","Beheerder")))
  assets$moffen = merge(assets$moffen ,Conv_voltage,by="Spanningsniveau",all.x=TRUE)
  
  rm("missing")  
  
# Seperate the data based on the asset and voltage --------------------------------------------------
  toc(); cat("Saving to file\n"); tic()
  
  save(assets,file=paste0(settings$Input_Datasets,"/Asset_Data_NOR_assets_",Sys.Date(),".Rda"))
  toc()
}