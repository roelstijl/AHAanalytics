AHA_Data_NOR_Log_Postprocessing  = function(){
  # Load and prepare some data --------------------------------------------------
  
  assets = list()
  changes = list()
  Conv_voltage = setkey(unique(data.table(read.xlsx(paste0(settings$Ruwe_Datasets,"/6. NOR/Conversion_of_voltages.xlsx"),1))),Spanningsniveau)  
  
  load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGEN.Rda"))    
  verbindingen = masterdataset
  verbindingen = unique(setorderv(verbindingen, "DateAdded",order=-1),by=c("ID_Verbinding","Lengte"))
  
  load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSDELEN.Rda"))
  kabels = merge(masterdataset,Conv_voltage,by="Spanningsniveau",all.x=TRUE)
  kabels = merge(masterdataset,verbindingen,by=c("ID_verbinding","Lengte"),all.x=TRUE)
  
  setorder(changes,"Date","ID_unique")
  i= 2*(1:(nrow(changes)/2)); 
  lengthch = data.table(changes$Lengte[i-1]-changes$Lengte[i]); 
  setnames(lengthch,"V1","Length_ch")
  lengthch = merge(cbind(changes[i-1][("ID_unique","Date")],lengthch)[lengthch$Length_Change!=0],kabels,by="ID_unique",all.x=TRUE)
  setnames(lengthch,"Date","Date_Length_ch")
  lengthch$Status_ID = "Length changed"
  kabels$Date_Length_ch=""
  kabels$Length_ch=""
  rbind(kabels,lengtch)
  
  remove("verbindingen"); remove("changes")
  
  load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSKNOOPPUNTEN.Rda"))
  moffen = merge(masterdataset,Conv_voltage,by="Spanningsniveau",all.x=TRUE)
  
  )

# Find the verbindingen and hoofdleidingen -----------------------------------------------------------
masterdataset = unique(masterdataset,by="ID_Verbinding")
setnames(a,"ID_Verbinding_van","ID_Verbinding")
a=merge(a,masterdataset[,c("ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by="ID_Verbinding",all.x=TRUE)
setnames(a,"ID_Hoofdleiding","ID_Hoofdleiding_van")
setnames(a,"ID_Verbinding","ID_Verbinding_van")

setnames(a,"ID_Verbinding_naar","ID_Verbinding")
a=merge(a,masterdataset[,c("ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by="ID_Verbinding",all.x=TRUE)
setnames(a,"ID_Hoofdleiding","ID_Hoofdleiding_naar")
setnames(a,"ID_Verbinding","ID_Verbinding_naar")

sum(!(a$ID_Hoofdleiding_van %in% a$ID_Hoofdleiding_naar))
setnames(kabels,c("Coo_X_van","Coo_Y_van"),c("Coo_X","Coo_Y"))
kabels = unique(kabels,by=c("Coo_X","Coo_Y"))
moffen=merge(moffen,kabels[,c("Coo_X","Coo_Y","ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by=c("Coo_X","Coo_Y"),all.x=TRUE)

setnames(moffen,"ID_Verbinding","ID_Verbinding_van")
setnames(moffen,"ID_Hoofdleiding","ID_Hoofdleiding_van")

setnames(kabels,c("Coo_X","Coo_Y"),c("Coo_X_van","Coo_Y_van"))
setnames(kabels,c("Coo_X_naar","Coo_Y_naar"),c("Coo_X","Coo_Y"))

kabels = unique(kabels,by=c("Coo_X","Coo_Y"))

moffen=merge(moffen,kabels[,c("Coo_X","Coo_Y","ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by=c("Coo_X","Coo_Y"),all.x=TRUE)
setnames(moffen,"ID_Verbinding","ID_Verbinding_naar")
setnames(moffen,"ID_Hoofdleiding","ID_Hoofdleiding_naar")
setnames(kabels,c("Coo_X","Coo_Y"),c("Coo_X_naar","Coo_Y_naar"))


load("C:/Datasets/AHAdata/2. Input Datasets/6. NOR/masterdataset_ELCVERBINDINGEN.Rda")

masterdataset = unique(masterdataset,by="ID_Verbinding")
setnames(a,"ID_Verbinding_van","ID_Verbinding")
a=merge(a,masterdataset[,c("ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by="ID_Verbinding",all.x=TRUE)
setnames(a,"ID_Hoofdleiding","ID_Hoofdleiding_van")
setnames(a,"ID_Verbinding","ID_Verbinding_van")

setnames(a,"ID_Verbinding_naar","ID_Verbinding")
a=merge(a,masterdataset[,c("ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by="ID_Verbinding",all.x=TRUE)
setnames(a,"ID_Hoofdleiding","ID_Hoofdleiding_naar")
setnames(a,"ID_Verbinding","ID_Verbinding_naar")

# Seperate the data based on the asset and voltage --------------------------------------------------
assets$mof_OS = kabels[!kabels$Netvlak=="MS"&!kabels[kabels$Netvlak=="LS"]]
assets$mof_LS = kabels[kabels$Netvlak=="LS"]
assets$mof_MS = kabels[kabels$Netvlak=="MS"]

assets$kbl_OS = kabels[!kabels$Netvlak=="MS"&!kabels[kabels$Netvlak=="LS"]]
assets$kbl_LS = kabels[kabels$Netvlak=="LS"]
assets$kbl_MS = kabels[kabels$Netvlak=="MS"]  

}