AHA_Data_BAR_Log  = function()
  {
# Creates as delta set from the statis BAR set
# Load and prepare some data --------------------------------------------------
assets = list(); 
changes = list();
pb = tkProgressBar(title = paste0("AHA_Data_BAR_Log_Postprocessing, ",as.character(Sys.time())), label = "Start", min = 0, max = 7, initial = 0, width = 450);

# Kabels --------------------------------------
setTkProgressBar(pb, 1,label = "Processing MS kabel data"); 
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_MS_KABELS_XY_PC6.Rda"))
assets$MSkabels = (Calc_Dates(mindataset))
try(setnames(assets$MSkabels,c("Invoeringsdatum","Hoofdleiding","ID_Hoofdleiding"),c("Datum_Invoering","ID_Hoofdleiding","ID_HLD_MS")))

setTkProgressBar(pb, 2,label = "Processing LS kabel data");
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_KABELS_XY_PC6.Rda"))
try(setnames(mindataset,c("Datum_begin","Datum_eind"),c("Datum_Begin","Datum_Eind")))
assets$LSkabels = Calc_Dates(mindataset)

try(setnames(assets$LSkabels,"LS_HLD_ID","ID_Verbinding"))
try(setnames(assets$MSkabels,"MS_HLD_ID","ID_Verbinding"))

# Moffen------------------------------ 
setTkProgressBar(pb, 3,label = "Processing MS moffen data")
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_MOFFEN_XY_PC6.Rda"))
try(setnames(mindataset,"Datum_End","Datum_Eind"))
assets$LSmoffen = (Calc_Dates(mindataset,"Moffen"))

setTkProgressBar(pb, 4,label = "Processing LS moffen data")
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_MS_MOFFEN_XY_PC6.Rda"))
try(setnames(mindataset,c("Datum_begin","Datum_eind"),c("Datum_Begin","Datum_Eind")))
assets$MSmoffen = (Calc_Dates(mindataset,"Moffen"))

# hoofdleidingen koppel ----------------------
setTkProgressBar(pb, 5,label = "Processing MS routenamen data")
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_hoofdleidingen.Rda"))
try(setnames(mindataset,c("Nummer","Routenaam"),c("ID_Hoofdleiding","Routenaam_MS")))
assets$MSHLDROUTE    = (mindataset)

setkey(assets$MSkabels,ID_Hoofdleiding)
setkey(assets$MSHLDROUTE,ID_Hoofdleiding)
assets$MSkabels = unique(assets$MSHLDROUTE[,list(Routenaam_MS,ID_Hoofdleiding)])[assets$MSkabels]

# Bereken postcode 4
assets$LSmoffen[,PC_4:=substr(assets$LSmoffen$PC_6,1,4)]
assets$LSkabels[,PC_4_van:=substr(assets$LSkabels$PC_6_van,1,4)]
assets$LSkabels[,PC_4_naar:=substr(assets$LSkabels$PC_6_naar,1,4)]  
assets$MSmoffen[,PC_4:=substr(assets$MSmoffen$PC_6,1,4)]
assets$MSkabels[,PC_4_van:=substr(assets$MSkabels$PC_6_van,1,4)]
assets$MSkabels[,PC_4_naar:=substr(assets$MSkabels$PC_6_naar,1,4)]  

# Brontabel
assets$LSkabels = assets$LSkabels[,Brontabel := "ls_kabels"]
assets$MSkabels = assets$MSkabels[,Brontabel := "ms_kabels"]
assets$LSmoffen = assets$LSmoffen[,Brontabel := "ls_moffen"]
assets$MSmoffen = assets$MSmoffen[,Brontabel := "ms_moffen"]

# Seperate the data based on the asset and voltage --------------------------------------------------
  ; cat("Saving to file\n"); 
  setTkProgressBar(pb, 6,label = "Saving to file")
  save(assets,file=paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_BAR_assets.Rda"))
  all_ID_NAN = laply(assets,function(x) try(unique(x$ID_NAN)))
  save(all_ID_NAN,file=paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_BAR_all_ID_NAN.Rda"))
  setTkProgressBar(pb, 7,label = "Done!")

}

# Function used to calculate the Assets added removed and length changed-------------------------
Calc_Dates = function(mindataset,assettype="kabels")
{# correct date format
#     l_ply(list("Datum_Begin","Datum_Eind","Datum_Wijziging"),
#         function(x) {;
#                      eval(parse(text=paste0("mindataset[,",x,":=",
#               switch (lapply(mindataset, class)[x][[1]][1],character ="dmy", Date      ="", POSIXct   = "as.Date")
#               ,"(",x,")]")))
#               })
  
# Added
setkey(mindataset,ID_NAN)
mindataset[,DateAdded:=min(Datum_Begin),by=ID_NAN]
mindataset[,Status_ID:="Active"]

# Length changed
if (assettype == "kabels")
{
  setorder(mindataset,ID_NAN,Datum_Wijziging)
  setkey(mindataset,ID_NAN,Datum_Wijziging)
  mindataset[,Lengte:= sqrt(abs(Coo_X_van-Coo_X_naar)^2+abs(Coo_Y_van-Coo_Y_naar)^2)]
  lch = c(0,mindataset[2:nrow(mindataset),Lengte] - mindataset[1:(nrow(mindataset)-1),Lengte])
  logi = duplicated(mindataset,by="ID_NAN")
  mindataset[logi,Length_ch:=lch[logi]]
  mindataset[Length_ch!=0,DateLength_ch:=(Datum_Wijziging)]
  mindataset[!is.na(DateLength_ch),Status_ID:="Length_changed"]}

# Status changed
if (assettype == "kabels")
{
  setorder(mindataset,ID_NAN,Datum_Wijziging,na.last=T)
  setkey(mindataset,ID_NAN,Datum_Wijziging)
  sch = c("",paste0(mindataset[1:(nrow(mindataset)-1),Status],"->",mindataset[2:(nrow(mindataset)),Status]))
  logi = duplicated(mindataset,by="ID_NAN") & c(F,mindataset[1:(nrow(mindataset)-1),Status] != mindataset[2:(nrow(mindataset)),Lengte])
  mindataset[logi,Status_ch:=sch[logi]]
  mindataset[logi,DateStatus_ch:=(Datum_Wijziging)]
  mindataset[!is.na(DateStatus_ch),Status_ID:="Status_changed"]
}

# Removed
mindataset[,DateRemoved:=(max(Datum_Eind)),by=ID_NAN]
mindataset[DateRemoved>"2090-01-01",DateRemoved:=NA]
mindataset[!is.na(DateRemoved),Status_ID:="Removed"]

return(mindataset)}