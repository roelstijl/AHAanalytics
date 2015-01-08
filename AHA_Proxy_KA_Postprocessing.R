AHA_Proxy_KA_Postprocessing = function(Proxy=F,GoogleMaps=F,FullSet=F)
{
# This function will plot the data files used in project AHA onto google maps
# Input is a series of PC4 areas or a single PC4 area PC_4 = 6810:6823 for Arnhem
# Method include proxy 
# Settings ---------------------------------------------------------
pb = tkProgressBar(title = paste0("AHA_Proxy_KA_Postprocessing, ",as.character(Sys.time())),label = "Loading files", min = 0, max = 14, initial = 0, width = 450);

# Load the required data files -------------------------------------
load(paste0(settings$Input_Datasets,"/23. Validatie_data/Validatie koppelingen.Rda"))
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_BAR.Rda"))
assets_BAR=assets
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_NOR.Rda"))
assets_NOR=assets
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))

if (GoogleMaps){
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_6_Spatial.Rda"))
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_4_Spatial.Rda"))  
  pc6@data$PC_4 = substr(pc6@data$POSTCODE,1,4)
  
}

l_ply(assets_BAR[1:4],function(x) try(setnames(x,"Status_R","Status_ID")))
rm(assets)

# Convert some stuff ------------------------------
setTkProgressBar(pb, 1,label = "Converting objects"); 

# BAR
assets_BAR$kabels = rbind(assets_BAR$LSkabels,assets_BAR$MSkabels,fill=TRUE)[,list(Status_ID,DateLength_ch,ID_NAN,Length_ch,DateRemoved,DateAdded,Coo_X_van,Coo_Y_van,PC_6_van,Coo_X_naar,Coo_Y_naar,PC_6_naar,PC_4_van,PC_4_naar)]
setkey(assets_BAR$kabels,ID_NAN);   
assets_BAR$kabels = assets_BAR$kabels[DateAdded>"2014-01-04"]
assets_BAR$kabels = unique(assets_BAR$kabels)

assets_BAR$moffen = rbind(assets_BAR$LSmoffen,assets_BAR$MSmoffen,fill=TRUE)[,list(Status_ID,ID_NAN,DateRemoved,DateAdded,Coo_X,Coo_Y,PC_4,PC_6)]
assets_BAR$moffen = assets_BAR$moffen[DateAdded>"2014-01-04"]
assets_BAR$moffen = unique(assets_BAR$moffen)

assets_BAR$all    = rbind(assets_BAR$moffen, assets_BAR$kabels,fill=TRUE)
setkey(assets_BAR$all,ID_NAN);   
assets_BAR$all    = unique(assets_BAR$all)

# NOR
assets_NOR$kabels = rbind(assets_NOR$LSkabels,assets_NOR$MSkabels,fill=TRUE)[,list(Brontabel,Status_ID,DateLength_ch,ID_NAN,Voltage,Length_ch,DateRemoved,DateAdded,Coo_X_van,Coo_Y_van,PC_6_van,Coo_X_naar,Coo_Y_naar,PC_6_naar,PC_4_van,PC_4_naar)]
setkey(assets_NOR$kabels,ID_NAN);   
assets_NOR$kabels = assets_NOR$kabels[DateAdded>"2014-01-04"]
assets_NOR$kabels = unique(assets_NOR$kabels)

assets_NOR$moffen = rbind(assets_NOR$LSmoffen,assets_NOR$MSmoffen,fill=TRUE)[,list(Brontabel,Status_ID,ID_NAN,Voltage,DateRemoved,DateAdded,Coo_X,Coo_Y,PC_4,PC_6)]
assets_NOR$moffen = assets_NOR$moffen[DateAdded>"2014-01-04"]

assets_NOR$all    = rbind(assets_NOR$moffen, assets_NOR$kabels,fill=TRUE)
setkey(assets_NOR$all,ID_NAN);   

# KLAK
storingen$all = rbind(storingen$LS,storingen$MS,fill=TRUE)[,list(Datum,Status,ID_KLAK_Melding,Getroffen_klanten_totaal,Duur_onderbreking,Datum_Eerste_Ontwerp,Klacht,Netcomponent,Mof,Veroorzaker,Tijdstip_definitief_einde,Coo_X,Coo_Y,PC_4,PC_6,Aantal_Melders)]
storingen$all[,Coo_X:=as.numeric(gsub(",",".",Coo_X))];storingen$all[,Coo_Y:=as.numeric(gsub(",",".",Coo_Y))];
storingen$KLAKMelders[,Coo_X:=as.numeric(gsub(",",".",Coo_X))];storingen$KLAKMelders[,Coo_Y:=as.numeric(gsub(",",".",Coo_Y))];
setkey(storingen$all,ID_KLAK_Melding)
storingen$all[,Storing:="Storing"]

# KLAK melders  
storingen$KLAKMelders$Melders = "Melder"
# setnames(storingen$KLAKMelders,"ID_KLAK_Melding","ID_KLAK_Melding_oud")
# setkey(storingen$KLAKMelders,ID_Groep)
# 
# temp = unique(storingen$KLAKMelders[ID_Groep!="" & !is.na(ID_Groep) & ST_Groep_eerste=="Ja",
#                                     list(ID_Groep,ID_KLAK_Melding_oud)])
# setnames(temp,"ID_KLAK_Melding_oud","ID_KLAK_Melding")
# setkey(temp,ID_Groep)
#   
# storingen$KLAKMelders=temp[storingen$KLAKMelders]

setkey(storingen$all,ID_KLAK_Melding)
setkey(ValidatieSet,ID_KLAK_Melding)
ValidatieSet=storingen$all[,list(ID_KLAK_Melding,Aantal_Melders)][ValidatieSet,]

# Create file with found assets  -------------------------------
setTkProgressBar(pb, 2,label = "Loading full asset datasets");  
if (FullSet){
FullDataAnalytics("NOR")  
FullDataAnalytics("BAR")
}

setTkProgressBar(pb, 2,label = "Calculating Proxy");  
if(Proxy)
{cat("Select a file for XY proxy\n")
ValidatieSet = IncludeProxy(ValidatieSet)
cat("Select a file for PC proxy\n")
ValidatieSet = IncludeProxy(ValidatieSet)}

setTkProgressBar(pb, 2,label = "Calculating ValidatieSet");  
ValidatieSet[,in_NORlog:=(ID_NAN %in% assets_NOR$all$ID_NAN)]
ValidatieSet[,in_BARlog:=(ID_NAN %in% assets_BAR$all$ID_NAN)]
ValidatieSet[,in_KLAK:=(ID_KLAK_Melding %in% storingen$all$ID_KLAK_Melding)]
ValidatieSet[,in_KLAKMelders:=(ID_KLAK_Melding %in% storingen$KLAKMelders$ID_KLAK_Melding)]
ValidatieSet[,in_NORlog_KLAK := (in_NORlog&in_KLAK)]
ValidatieSet[,in_BARlog_KLAK := (in_BARlog&in_KLAK)]
setkey(ValidatieSet,ID_KLAK_Melding)
ValidatieSet[,KLAK_has_Coordinates := (!is.na(storingen$all[ValidatieSet]$Coo_X))]
setkey(ValidatieSet,ID_NAN)
ValidatieSet[,NORlog_has_Coordinates := (!is.na(assets_NOR$all[ValidatieSet]$Coo_X)|!is.na(assets_NOR$all[ValidatieSet]$Coo_X_van))]
ValidatieSet[,BARlog_has_Coordinates := (!is.na(assets_BAR$all[ValidatieSet]$Coo_X)|!is.na(assets_BAR$all[ValidatieSet]$Coo_X_van))]
ValidatieSet[,in_NORlog_and_XY:=(in_NORlog_KLAK&KLAK_has_Coordinates&NORlog_has_Coordinates)]
ValidatieSet[,in_BARlog_and_XY:=(in_BARlog_KLAK&BARlog_has_Coordinates&KLAK_has_Coordinates)]

molten = melt(ValidatieSet, id.vars = c("Regio","Datum","ID_KLAK_Melding","ID_NAN","Opmerkingen"),,)

write.xlsx(molten,file=paste0(settings$Analyse_Datasets,"/2. Proxy validatie/Validatie_Meta.xlsx"),sheetName="Format Melt",row.names=FALSE, append=TRUE)
write.xlsx(ValidatieSet,file=paste0(settings$Analyse_Datasets,"/2. Proxy validatie/Validatie_Meta.xlsx"),sheetName="Format Wide",row.names=FALSE, append=TRUE)

ggplot(molten,aes(variable,fill=paste(value))) + geom_bar(colour="black") + coord_flip()

  # Create the maps ----------------------------
  if (GoogleMaps){
    
    setTkProgressBar(pb,3,label="Starting Google maps"); pc=3;
    pcmax = 10/(sum(ValidatieSet$in_NORlog_and_XY)+sum(ValidatieSet$in_BARlog_and_XY))
    
    for (ID_KLAK in ValidatieSet$ID_KLAK_Melding[as.logical(ValidatieSet$in_NORlog_and_XY)]){
      setTkProgressBar(pb, pc,label =paste0("Generating google maps, KLAK: ",ID_KLAK)); pc=pc+pcmax
      Createmaps(assets_NOR,storingen,pc4,pc6,ID_KLAK,ValidatieSet[ID_KLAK_Melding==ID_KLAK,ID_NAN],"NORlog")
    }
    for (ID_KLAK in ValidatieSet$ID_KLAK_Melding[as.logical(ValidatieSet$in_BARlog_and_XY)]){
      setTkProgressBar(pb, pc,label = paste0("Generating google maps, KLAK: ",ID_KLAK)); pc=pc+pcmax
      Createmaps(assets_BAR,storingen,pc4,pc6,ID_KLAK,ValidatieSet[ID_KLAK_Melding==ID_KLAK,ID_NAN],"BARlog")
    }
  }
  setTkProgressBar(pb, 14,label = "done"); 
  
}

# Put it on a map -------------------------------------------------------
Createmaps = function(assets,storingen,pc4,pc6,ID_KLAK_M,ID_NAN_M,asset_source){
  geosets = list()
  PC_4_M=storingen$all[ID_KLAK_M == storingen$all$ID_KLAK_M,PC_4]
  
  # Kabels
  kabels =assets$kabels[!is.na(Coo_X_van) & (PC_4_van== PC_4_M |PC_4_naar== PC_4_M)]
  if(nrow(kabels)>0){
    geosets$kabels=
      SpatialLinesDataFrame(
        SpatialLines(
          alply(kabels,1,
                function(set) Lines(list(Line(t(rbind(set[,list(Coo_X_van,Coo_X_naar)],
                                                      set[,list(Coo_Y_van,Coo_Y_naar)],use.names=FALSE)))),set$ID_NAN)),
          proj4string=CRS("+init=epsg:28992")), 
        assets$kabels[!is.na(Coo_X_van) & (PC_4_van== PC_4_M |PC_4_naar== PC_4_M)],match.ID = FALSE)}
  else{kabels}
  
  # Moffen
  geosets$moffen                 = (assets$moffen[!is.na(assets$moffen$Coo_X)&PC_4 == PC_4_M])
  if(nrow(geosets$moffen )>0){
    coordinates(geosets$moffen)    = geosets$moffen[,list(Coo_X,Coo_Y)]
    proj4string(geosets$moffen)    = CRS("+init=epsg:28992")
  }
  
  # Storingen
  geosets$storingen              = (storingen$all[!is.na(storingen$all$Coo_X)&PC_4 == PC_4_M])
  if(nrow(geosets$storingen)>0){
    coordinates(geosets$storingen) = geosets$storingen  [,list(Coo_X,Coo_Y)]
    proj4string(geosets$storingen) = CRS("+init=epsg:28992")
  }
  
  # Melders
  geosets$KLAKMelders              = (storingen$KLAKMelders[!is.na(storingen$KLAKMelders$Coo_X)& ID_KLAK_Melding ==ID_KLAK_M])
  if(nrow(geosets$KLAKMelders)>0){  
    coordinates(geosets$KLAKMelders) = geosets$KLAKMelders  [,list(Coo_X,Coo_Y)]
    proj4string(geosets$KLAKMelders) = CRS("+init=epsg:28992")
  }
  
  map=pGMwrapper(pc6[pc6$PC_4==PC_4_M,],"Postcode 6 gebieden","POSTCODE",heat.colors(nrow(pc6[pc6$PC_4==PC_4_M,])),1,FALSE,options=",fillOpacity=0.2")
  
  map=pGMwrapper(geosets$storingen[geosets$storingen$ID_KLAK_Melding!=ID_KLAK_M,],"Omringende Storingen","Storing","#FF8566",3,FALSE,map)
  map=pGMwrapper(geosets$moffen[geosets$moffen$ID_NAN!=ID_NAN_M,],"Omringende Moffen","Status_ID",c("#248F24","#B26B24"),3,FALSE,map)
  map=pGMwrapper(geosets$kabels[geosets$kabels$ID_NAN!=ID_NAN_M,],"Omringende Kabels","Status_ID",c("#248F24","#003399","#B26B24"),3,FALSE,map)
  
  map=pGMwrapper(geosets$kabels[geosets$kabels$ID_NAN==ID_NAN_M,],"Betrokken Kabels","Status_ID","#B80000",5,FALSE,map)
  map=pGMwrapper(geosets$moffen[geosets$moffen$ID_NAN==ID_NAN_M,],"Betrokken Moffen","Status_ID","#B80000",5,FALSE,map)
  
  map=pGMwrapper(geosets$KLAKMelders[geosets$KLAKMelders$ID_KLAK_Melding==ID_KLAK_M,],"Betrokken Melders","Melders","#E65C00",3,FALSE,map)
  map=pGMwrapper(geosets$storingen[geosets$storingen$ID_KLAK_Melding==ID_KLAK_M,],"Betrokken Storing","Storing","#B80000",3,last=TRUE,previousmap=map,
                 file=paste0(settings$Analyse_Datasets,"/3. Geo data/Google_maps_KLAK_",ID_KLAK_M,"_",asset_source,".html"))
  
  
}

# Create the maps without too much hassle ------------------------------------
pGMwrapper = function (data,layername,group,colors,width,last=FALSE,previousmap=0,file="",options=""){
  # Simple wrapper to make plotting more convenient
  options(warn=-1)
  txt1 = ifelse(!last,",add=TRUE","")
  txt2 = ifelse(class(previousmap)=="list",",previousMap=previousmap","")
  txt3 = ifelse(file!="",",filename = file","")
  ROADMAP = "ROADMAP"
  
  try(if(nrow(data)>0)
  {eval(parse(text=
                paste0("map=plotGoogleMaps(",
                       "data,legend=FALSE,layerName=layername,zcol=group,",
                       "colPalette = colors,strokeWeight=width,mapTypeId=ROADMAP",
                       txt1,txt2,txt3,options,
                       ")"
                )))
   return(map)}
  else if(class(previousmap)=="list"){
    return(previousmap)
  }
  else{
    cat("Input is empty!\n")
  })
  return(previousmap)
  
  options(warn=0)
}

# Analyses the entire Asset Database (very memory intensive!) -----------------
FullDataAnalytics = function(AssetName = "NOR")
{
  ValidatieSet = data.table(read.xlsx(paste0(settings$Analyse_Datasets,"/2. Proxy validatie/Validatie_Meta.xlsx"),sheetName="Format Wide"),key=ID_NAN)
#   loadObj(paste0("C:/Datasets/AHAdata/2. Input Datasets/2. All Assets/Asset_Data_",AssetName,"_assets.Rda"))
  load(paste0("C:/Datasets/AHAdata/2. Input Datasets/2. All Assets/Asset_Data_",AssetName,"_assets.Rda"))


  llply(assets, function(x) setkey(x,ID_NAN))
  setorder(assets$kabels,DateLength_ch,na.last=TRUE)
  assets = llply(assets,function(x) unique(x)[Brontabel,ID_NAN,DateRemoved,DateAdded,DateLength_ch,Length_ch])
  
  write.xlsx(ValidatieSet[rbindlist(assets)],
             file=paste0(settings$Analyse_Datasets,"/2. Proxy validatie/Validatie_Meta.xlsx"),
             sheetName=paste0("All_Assets_",AssetName), append=TRUE)
}

# Analyse the proxy results ---------------------------------------------
IncludeProxy = function(ValidatieSet)
{
  #   load("C:/Datasets/AHAdata/3. Analyse Datasets/1. KA Proxy/assetslPC2015-01-07 11.12.00.Rda")
  file = file.choose()
  load(file); 
  
  ProxyName = paste0(laply(strsplit(substr(basename(file),8,13),"")[[1]], function(x) ifelse(suppressMessages(is.na(as.numeric(x))),x,"")),collapse = '')
  
  val = assetsltb; 
  rm("assetsltb")
#   load(paste0(settings$Input_Datasets,"/23. Validatie_data/Validatie koppelingen.Rda"))
  
  # Convert to data tables
  valdt = data.table(ldply(val,function(y)
  {  All_ID_KLAK = as.character(names(y));
     ldply(All_ID_KLAK, function(x) 
     { 
       if (any("DateLength_ch" %in% colnames(y[[x]])))
                         {z = y[[x]][,list(Brontabel,ID_NAN,DateRemoved,DateAdded,DateLength_ch,Length_ch,
                         Coo_X_van,Coo_Y_van,PC_6_van,Coo_X_naar,Coo_Y_naar,PC_6_naar)]} else 
                         {z = y[[x]][,list(Brontabel,ID_NAN,DateRemoved,DateAdded,Coo_X,Coo_Y,PC_6)]}
       z$ID_KLAK_Melding=x; z
     })
  } ,.progress="text"))
  
  setkey(ValidatieSet,ID_KLAK_Melding)
  setkey(valdt,ID_KLAK_Melding)
  
  ValidatieSet = valdt[ValidatieSet][,any(ID_NAN %in% i.ID_NAN),by=ID_KLAK_Melding][ValidatieSet]
  setnames(ValidatieSet,"V1",paste0("Proxi_",ProxyName,"_Correct"))
  
  write.xlsx(valdt[ValidatieSet],
           file=paste0(settings$Analyse_Datasets,"/2. Proxy validatie/Validatie_Meta.xlsx"),
           sheetName=paste0("Proxy_",ProxyName), append=TRUE)
  
  return(ValidatieSet)

  load(paste0(settings$Input_Datasets,"/23. Validatie_data/Validatie koppelingen.Rda"))
  
  load("C:/Datasets/AHAdata/2. Input Datasets/2. All Assets/Asset_Data_NOR_assets.Rda")
  setkey(assets$kabels,ID_NAN)
  setkey(assets$moffen,ID_NAN)
  setorder(assets$kabels,DateLength_ch,na.last=TRUE)
  assets$kabels=unique(assets$kabels)
  assets$moffen=unique(assets$moffen)
  set_NOR=rbind(assets$kabels[ValidatieSet][!is.na(Bronsysteem)],assets$moffen[ValidatieSet][!is.na(Bronsysteem)],fill=TRUE)
  save(set_NOR,file=paste0(settings$Analyse_Datasets,"/2. Proxy validatie/All_Asset_NOR_Info_Validatie.Rda"))
  
  load(paste0(settings$Input_Datasets,"/23. Validatie_data/Validatie koppelingen.Rda"))
  load("C:/Datasets/AHAdata/2. Input Datasets/2. All Assets/Asset_Data_BAR_assets.Rda")
  setkey(assets$MSkabels,ID_NAN)
  setkey(assets$MSmoffen,ID_NAN)
  setorder(assets$MSkabels,DateLength_ch,na.last=TRUE)
  setkey(assets$MSkabels,ID_NAN)
  assets$MSkabels=unique(assets$MSkabels)
  assets$MSmoffen=unique(assets$MSmoffen)
  
  setkey(assets$LSkabels,ID_NAN)
  setkey(assets$LSmoffen,ID_NAN)
  setorder(assets$LSkabels,DateLength_ch,na.last=TRUE)
  setkey(assets$LSkabels,ID_NAN)
  assets$LSkabels=unique(assets$LSkabels)
  assets$LSmoffen=unique(assets$LSmoffen)  
  setkey(ValidatieSet,ID_NAN)

  set_BAR=rbind(assets$MSkabels[ValidatieSet][!is.na(Brontabel)],assets$MSmoffen[ValidatieSet][!is.na(Brontabel)],
                assets$LSkabels[ValidatieSet][!is.na(Brontabel)],assets$LSmoffen[ValidatieSet][!is.na(Brontabel)],fill=TRUE)
  save(set_BAR,file=paste0(settings$Analyse_Datasets,"/2. Proxy validatie/All_Asset_BAR_Info_Validatie.Rda"))
}

# for (KLAK in ValidatieSet[ValidatieSet$in_NORlog_and_XY|ValidatieSet$in_BARlog_and_XY,ID_KLAK_Melding]){
#   file = list.files(path=paste0(settings$Ruwe_Datasets,"/17. Storingsschetsen"),
#                     pattern=paste0(KLAK,".html"),full.names = TRUE)
#   z=file.copy(file[1],
#             "C:/Datasets/AHAdata/5. Visuals and Tableau workbooks/4. HTML Files")
# }

