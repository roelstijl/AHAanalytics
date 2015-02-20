AHA_Data_BAR_Log  = function()
  {
# Creates as delta set from the statis BAR set
# Load and prepare some data --------------------------------------------------
require(RANN)
assets = list(); 
changes = list();
pb = tkProgressBar(title = paste0("AHA_Data_BAR_Log_Postprocessing, ",as.character(Sys.time())), label = "Start", min = 0, max = 7, initial = 0, width = 450);

# Kabels --------------------------------------
setTkProgressBar(pb, 1,label = "Processing MS kabel data"); 
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_MS_KABELS_XY_PC6.Rda"))
assets$MSkabels = (Calc_Dates(mindataset))
try(setnames(assets$MSkabels,c("Invoeringsdatum","ID_MS_HLD"),c("Datum_Invoering","MS_HLD_ID")))

setTkProgressBar(pb, 2,label = "Processing LS kabel data");
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_KABELS_XY_PC6.Rda"))
try(setnames(mindataset,c("Ls_Verbinding"),c("LS_HLD_ID")))
assets$LSkabels = Calc_Dates(mindataset)

# try(setnames(assets$LSkabels,"LS_HLD_ID","ID_Verbinding"))
# try(setnames(assets$MSkabels,"MS_HLD_ID","ID_Verbinding"))

# Moffen------------------------------ 
setTkProgressBar(pb, 3,label = "Processing MS moffen data")
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_MOFFEN_XY_PC6.Rda"),verbose=T)
try(setnames(mindataset,"Datum_End","Datum_Eind"))
assets$LSmoffen = (Calc_Dates(mindataset,"Moffen"))

setTkProgressBar(pb, 4,label = "Processing MS moffen data")
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_MS_MOFFEN_XY_PC6.Rda"))
try(setnames(mindataset,c("Datum_begin","Datum_eind"),c("Datum_Begin","Datum_Eind")))
assets$MSmoffen = (Calc_Dates(mindataset,"Moffen"))

# hoofdleidingen koppel ----------------------
setTkProgressBar(pb, 5,label = "Processing MS routenamen data")
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS hoofdleidingen.Rda"))
try(setnames(mindataset,c("ID_Hoofdleiding","Nummer","MS_Routenaam"),c("MS_HLD_ID","ID_Hoofdleiding","Routenaam_MS")))
# try(setnames(assets$MSkabels,c("ID_MS_HLD"),c("ID_Verbinding")))
assets$MSHLDROUTE    = (mindataset)
names(assets$MSkabels)
setkey(assets$MSkabels,MS_HLD_ID)
setkey(assets$MSHLDROUTE,MS_HLD_ID)
assets$MSkabels =  assets$MSHLDROUTE[,list(Routenaam_MS,MS_HLD_ID,ID_Hoofdleiding)][assets$MSkabels] #Koppel kabel op MS_HLD_ID (ID uit NRG tabel, dit is uniek)

#koppel MS moffen aan MS kabels
MSkabels_allXY  =  assets$MSkabels[, c("ID_BAR","Coo_X_van","Coo_Y_van"),with=F]
setnames(MSkabels_allXY,c("Coo_X_van","Coo_Y_van"),c("Coo_X_naar","Coo_Y_naar"))
MSkabels_allXY  =  rbind(MSkabels_allXY,assets$MSkabels[, c("ID_BAR","Coo_X_naar","Coo_Y_naar"),with=F])
setnames(MSkabels_allXY,c("Coo_X_naar","Coo_Y_naar"),c("Coo_X","Coo_Y"))
moffen_NN       = nn2(MSkabels_allXY[,c("Coo_X","Coo_Y"),with=F],assets$MSmoffen[,c("Coo_X","Coo_Y"),with=F],k=1)
assets$MSmoffen[,MSkabel_ID_BAR:=  MSkabels_allXY$ID_BAR[moffen_NN$nn.idx]]

setkey(assets$MSkabels,ID_BAR)
assets$MSmoffen[,Routenaam_MS  := assets$MSkabels[list(assets$MSmoffen$MSkabel_ID_BAR)][,c("Routenaam_MS"),with=F]] #Koppel MS Routenaam aan MS mof
#sum(assets$MSmoffen$Routenaam_MS=="",na.rm=T)
rm(MSkabels_allXY,mindataset)

#koppel LS moffen aan LS kabels
LSkabels_allXY  =  assets$LSkabels[, c("ID_BAR","Coo_X_van","Coo_Y_van"),with=F]
setnames(LSkabels_allXY,c("Coo_X_van","Coo_Y_van"),c("Coo_X_naar","Coo_Y_naar"))
LSkabels_allXY  =  rbind(LSkabels_allXY,assets$LSkabels[, c("ID_BAR","Coo_X_naar","Coo_Y_naar"),with=F])
setnames(LSkabels_allXY,c("Coo_X_naar","Coo_Y_naar"),c("Coo_X","Coo_Y"))
moffen_NN       =  nn2(LSkabels_allXY[,c("Coo_X","Coo_Y"),with=F],assets$LSmoffen[,c("Coo_X","Coo_Y"),with=F],k=1)
assets$LSmoffen[,LSkabel_ID_BAR:=  LSkabels_allXY$ID_BAR[moffen_NN$nn.idx]]
setkey(assets$LSkabels,ID_BAR)
assets$LSmoffen[,ID_Hoofdleiding  := assets$LSkabels[list(assets$LSmoffen$LSkabel_ID_BAR)][,c("ID_Hoofdleiding","LS_HLD_ID"),with=F]] #Koppel MS Routenaam aan MS mof

#s1 = sum(assets$LSmoffen$ID_Hoofdleiding=="",na.rm=T)
#s2 = sum(is.na(assets$LSmoffen$ID_Hoofdleiding),na.rm=T)


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
  mindataset[Length_ch==0,Length_ch:=NA]
  mindataset[Length_ch!=0,DateLength_ch:=(Datum_Wijziging)]
  mindataset[!is.na(DateLength_ch),Status_ID:="Length_changed"]}

# Status changed
setorder(mindataset,ID_NAN,Datum_Wijziging,na.last=T)
setkey(mindataset,ID_NAN,Datum_Wijziging)
sch  <- c("",paste0(mindataset[1:(nrow(mindataset)-1),Status],"->",mindataset[2:(nrow(mindataset)),Status]))
logi <- duplicated(mindataset,by="ID_NAN") & c(F,mindataset[1:(nrow(mindataset)-1),Status] != mindataset[2:(nrow(mindataset)),Status])
mindataset[logi,Status_ch:=sch[logi]]
mindataset[logi,DateStatus_ch:=(Datum_Wijziging)]
mindataset[!is.na(DateStatus_ch),Status_ID:="Status_changed"]
if (assettype == "kabels"){
  mindataset[(!is.na(DateStatus_ch) & !is.na(DateLength_ch)),Status_ID:="Length_and_Status_changed"]}

# Removed
mindataset[,DateRemoved:=(max(Datum_Eind)),by=ID_NAN]
mindataset[DateRemoved>"2090-01-01",DateRemoved:=NA]
mindataset[!is.na(DateRemoved),Status_ID:="Removed"]

return(mindataset)}