AHA_Data_BAR_Log  = function()
{
# Creates as delta set from the statis BAR set
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
assets$MSkabels = unique(MSHLDRoute[,list(Routenaam,ID_Hoofdleiding)])[assets$MSkabels]

#koppelen van kabelgegevens aan bijbehorende mof
assets$MSmoffen$Routenaam        = NA
assets$MSmoffen$ID_Hoofdleiding  = NA
assets$MSmoffen$Datum_Bouwjaar   = NA
assets$MSmoffen$Routenaam        = nnsearch_kabel_mof(assets$MSkabels,assets$MSmoffen, "Routenaam")
assets$MSmoffen$ID_Hoofdleiding  = nnsearch_kabel_mof(assets$MSkabels,assets$MSmoffen, "ID_Hoofdleiding")
assets$MSmoffen$Datum_Bouwjaar   = nnsearch_kabel_mof(assets$MSkabels,assets$MSmoffen, "Datum_Bouwjaar")

assets$LSmoffen$ID_Verbinding    = NA
assets$LSmoffen$ID_Hoofdleiding  = NA
assets$LSmoffen$Datum_Bouwjaar   = NA
assets$LSmoffen$ID_Verbinding    = nnsearch_kabel_mof(assets$LSkabels,assets$LSmoffen, "ID_Verbinding")
assets$LSmoffen$ID_Hoofdleiding  = nnsearch_kabel_mof(assets$LSkabels,assets$LSmoffen, "ID_Hoofdleiding")
assets$LSmoffen$Datum_Bouwjaar   = nnsearch_kabel_mof(assets$LSkabels,assets$LSmoffen, "Datum_Bouwjaar")

assets <- lapply(assets,function(x){x$Datum_Bouwjaar[which(x$Datum_Bouwjaar > as.Date("2015-03-13"))] =
  x$Datum_Bouwjaar[which(x$Datum_Bouwjaar>as.Date("2015-03-13"))]- years(100);return(x)})

assets$LSmoffen$Montagedatum[which(assets$LSmoffen$Montagedatum > as.Date("2015-03-13"))] =
  assets$LSmoffen$Montagedatum[which(assets$LSmoffen$Montagedatum>as.Date("2015-03-13"))]- years(100);
assets$MSmoffen$Datum_montage[which(assets$MSmoffen$Datum_montage > as.Date("2015-03-13"))] =
  assets$MSmoffen$Datum_montage[which(assets$MSmoffen$Datum_montage>as.Date("2015-03-13"))]- years(100);

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

# Save
setpbarwrapper(pb, 6,label = "Saving to file")
save(assets,file=paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_BAR_assets.Rda"))
setpbarwrapper(pb, 7,label = "Done!")

}

# Function used to calculate the Assets added removed and length changed-------------------------
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

return(mindataset)}

#Functie om kabels aan moffen te koppelen
nnsearch_kabel_mof = function(kabelsset,moffenset,variable){
  # Third use NN for Routenaam
  nearest = nn2(kabelsset[!is.na(Coo_X_naar) & !is.na(kabelsset[[variable]]),list(Coo_X_naar,Coo_Y_naar)],moffenset[!is.na(Coo_X)&is.na(moffenset[[variable]]),list(Coo_X,Coo_Y)],k=1)
  nearest2= nn2(kabelsset[!is.na(Coo_X_naar) & !is.na(kabelsset[[variable]]),list(Coo_X_van,Coo_Y_van)],moffenset[!is.na(Coo_X)&is.na(moffenset[[variable]]),list(Coo_X,Coo_Y)],k=1)
  nnd = (nearest$nn.dists[,1]>=nearest2$nn.dists[,1])
  nni = nearest$nn.idx[,1]; nni[nnd] = nearest2$nn.idx[nnd,1]
  nni[!is.na(moffenset$Coo_X)&is.na(moffenset[[variable]])] = nni
  
  # output = rep(NA, nrow(moffenset))
  
  # output[is.na(kabelsset[[variable]])&!is.na(nni)] =
  
  output = kabelsset[[variable]][!is.na(kabelsset$Coo_X_naar)&!is.na(kabelsset[[variable]])][nni[!is.na(nni) & is.na(moffenset[[variable]])]]
  
  return(output)
}