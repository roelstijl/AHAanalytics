# Creates as delta set from the statis BAR set

AHA_Data_BAR_Log  = function(){
# Load and prepare some data --------------------------------------------------
assets = list(); 
changes = list();
pb = pbarwrapper(title = paste0("AHA_Data_BAR_Log_Postprocessing, ",as.character(Sys.time())), label = "Start", min = 0, max = 7, initial = 0, width = 450);

# Kabels --------------------------------------
setpbarwrapper(pb, 1,label = "Processing MS kabel data"); 
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_MS_KABELS_XY_PC6.Rda"))
assets$MSkabels = (Calc_Dates(mindataset))

setpbarwrapper(pb, 2,label = "Processing LS kabel data");
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_KABELS_XY_PC6.Rda"))
assets$LSkabels = Calc_Dates(mindataset)

# Moffen------------------------------ 
setpbarwrapper(pb, 3,label = "Processing MS moffen data")
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_LS_MOFFEN_XY_PC6.Rda"))
try(setnames(mindataset,"Datum_End","Datum_Eind"))
assets$LSmoffen = (Calc_Dates(mindataset,"Moffen"))

setpbarwrapper(pb, 4,label = "Processing LS moffen data")
load(paste0(settings$Ruwe_Datasets,"/1. BARlog/MH_NRG_MS_MOFFEN_XY_PC6.Rda"))
try(setnames(mindataset,c("Datum_begin","Datum_eind"),c("Datum_Begin","Datum_Eind")))
assets$MSmoffen = (Calc_Dates(mindataset,"Moffen"))

# hoofdleidingen koppel ----------------------
setpbarwrapper(pb, 5,label = "Processing MS routenamen data")
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_hoofdleidingen.Rda"))
MSHLDRoute    = (mindataset)
setkey(assets$MSkabels,ID_Hoofdleiding)
setkey(MSHLDRoute,ID_Hoofdleiding)
assets$MSkabels[,Routenaam:=unique(MSHLDRoute)[assets$MSkabels,Routenaam]]

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

# Make sure we don't have any out of bound XY
assets$LSmoffen[Coo_X<60000 | Coo_X>260000 |Coo_Y<400000|Coo_Y>650000, Coo_X:=NA]
assets$LSmoffen[Coo_X<60000 | Coo_X>260000 |Coo_Y<400000|Coo_Y>650000, Coo_Y:=NA]
assets$LSkabels[Coo_X_van<60000 | Coo_X_van>260000 |Coo_Y_van<400000|Coo_Y_van>650000, Coo_X_van:=NA]
assets$LSkabels[Coo_X_van<60000 | Coo_X_van>260000 |Coo_Y_van<400000|Coo_Y_van>650000, Coo_Y_van:=NA]
assets$LSkabels[Coo_X_naar<60000 | Coo_X_naar>260000 |Coo_Y_naar<400000|Coo_Y_naar>650000, Coo_X_naar:=NA]
assets$LSkabels[Coo_X_naar<60000 | Coo_X_naar>260000 |Coo_Y_naar<400000|Coo_Y_naar>650000, Coo_Y_naar:=NA]
assets$MSmoffen[Coo_X<60000 | Coo_X>260000 |Coo_Y<400000|Coo_Y>650000, Coo_X:=NA]
assets$MSmoffen[Coo_X<60000 | Coo_X>260000 |Coo_Y<400000|Coo_Y>650000, Coo_Y:=NA]
assets$MSkabels[Coo_X_van<60000 | Coo_X_van>260000 |Coo_Y_van<400000|Coo_Y_van>650000, Coo_X_van:=NA]
assets$MSkabels[Coo_X_van<60000 | Coo_X_van>260000 |Coo_Y_van<400000|Coo_Y_van>650000, Coo_Y_van:=NA]
assets$MSkabels[Coo_X_naar<60000 | Coo_X_naar>260000 |Coo_Y_naar<400000|Coo_Y_naar>650000, Coo_X_naar:=NA]
assets$MSkabels[Coo_X_naar<60000 | Coo_X_naar>260000 |Coo_Y_naar<400000|Coo_Y_naar>650000, Coo_Y_naar:=NA]

# Montagedatum
l_ply(assets,function(curfield){
  l_ply(names(curfield)[laply(curfield,function(x) class(x) =="Date")],
     function(x) {
     eval(parse(text = 
     paste0("curfield[",x," > as.Date(\"2015-06-01\") & !is.na(",x,"),",x," := ", 
     x," - years(100)]")))})  
})

# Add XY
setnames(assets$LSkabel,"ID_NAN","ID_NAN_kabel")
setnames(assets$MSkabel,"ID_NAN","ID_NAN_kabel")

LSmoffenkoppel = assets$LSmoffen[,list(ID_NAN,Coo_X,Coo_Y)]
MSmoffenkoppel = assets$MSmoffen[,list(ID_NAN,Coo_X,Coo_Y)]

LSmoffenkoppel[!is.na(Coo_X),ID_Verbinding:=nnsearch_kabel_mof(assets$LSkabels,LSmoffenkoppel,"ID_Verbinding")]
LSmoffenkoppel[!is.na(Coo_X),ID_Hoofdleiding:=nnsearch_kabel_mof(assets$LSkabels,LSmoffenkoppel,"ID_Hoofdleiding")]
LSmoffenkoppel[!is.na(Coo_X),ID_NAN_kabel:=nnsearch_kabel_mof(assets$LSkabels,LSmoffenkoppel,"ID_NAN_kabel")]
LSmoffenkoppel[!is.na(Coo_X),Datum_Bouwjaar_kabel:=nnsearch_kabel_mof(assets$LSkabels,LSmoffenkoppel,"Datum_Bouwjaar")]

MSmoffenkoppel[!is.na(Coo_X),ID_Verbinding:=nnsearch_kabel_mof(assets$MSkabels,MSmoffenkoppel,"ID_Verbinding")]
MSmoffenkoppel[!is.na(Coo_X),ID_Hoofdleiding:=nnsearch_kabel_mof(assets$MSkabels,MSmoffenkoppel,"ID_Hoofdleiding")]
MSmoffenkoppel[!is.na(Coo_X),ID_NAN_kabel:=nnsearch_kabel_mof(assets$MSkabels,MSmoffenkoppel,"ID_NAN_kabel")]
MSmoffenkoppel[!is.na(Coo_X),Routenaam:=nnsearch_kabel_mof(assets$MSkabels,MSmoffenkoppel,"Routenaam")]
MSmoffenkoppel[!is.na(Coo_X),Datum_Bouwjaar_kabel:=nnsearch_kabel_mof(assets$MSkabels,MSmoffenkoppel,"Datum_Bouwjaar")]

setkey(assets$LSmoffen,ID_NAN)
setkey(assets$MSmoffen,ID_NAN)
setkey(LSmoffenkoppel,ID_NAN)
setkey(MSmoffenkoppel,ID_NAN)

assets$LSmoffen = unique(LSmoffenkoppel[,list(ID_Verbinding,ID_Hoofdleiding,ID_NAN_kabel,Datum_Bouwjaar,ID_NAN)])[assets$LSmoffen]
assets$MSmoffen = unique(MSmoffenkoppel[,list(ID_Verbinding,ID_Hoofdleiding,ID_NAN_kabel,Routenaam,Datum_Bouwjaar,ID_NAN)])[assets$MSmoffen]

setnames(assets$LSkabel,"ID_NAN_kabel","ID_NAN")
setnames(assets$MSkabel,"ID_NAN_kabel","ID_NAN")

# Change some names mostly from tableau insights
try(setnames(assets$MSmoffen,"Wijziging_Naam","Naam_Wijziging"))
assets$LSmoffen[Datum_Eind=="2099-12-31",Datum_Eind:=NA]
assets$MSmoffen[Datum_Eind=="2099-12-31",Datum_Eind:=NA]


# Save
setpbarwrapper(pb, 6,label = "Saving to file")
save(assets,file=paste0(settings$Input_Datasets,"/2. All assets/Asset_Data_BAR_assets.Rda"))
setpbarwrapper(pb, 7,label = "Done!")

}

# Function used to calculate the assets added removed and length changed-------------------------
Calc_Dates = function(mindataset,assettype="kabels")
{
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
  mindataset[!is.na(DateLength_ch),Status_ID:="Length_changed"]
}

# Status changed
if (assettype == "kabels")
{
  setorder(mindataset,ID_NAN,Datum_Wijziging,na.last=T)
  setkey(mindataset,ID_NAN,Datum_Wijziging)
  sch = c("",paste0(mindataset[1:(nrow(mindataset)-1),Status],"->",mindataset[2:(nrow(mindataset)),Status]))
  logi = duplicated(mindataset,by="ID_NAN") & c(F,mindataset[1:(nrow(mindataset)-1),Status] != mindataset[2:(nrow(mindataset)),Lengte])
  logi[is.na(logi)] = F
  mindataset[logi,Status_ch:=sch[logi]]
  mindataset[logi,DateStatus_ch:=(Datum_Wijziging)]
  mindataset[!is.na(DateStatus_ch),Status_ID:="Status_changed"]
}

# Removed
mindataset[,DateRemoved:=(max(Datum_Eind)),by=ID_NAN]
mindataset[DateRemoved>"2090-01-01",DateRemoved:=NA]
mindataset[!is.na(DateRemoved),Status_ID:="Removed"]
mindataset[Datum_Eind>"2090-01-01",Datum_Eind:=NA]

return(mindataset)}
