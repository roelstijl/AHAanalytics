AHA_Proxy_KA_Postprocessing = function(Proxy_files_to_import=0,GoogleMaps=F,includePCmaps=F,FullSet=F)
{
# This function will plot the data files used in project AHA onto google maps
# Input is a series of PC4 areas or a single PC4 area PC_4 = 6810:6823 for Arnhem
# Method include proxy 
# Settings ---------------------------------------------------------
cfg               = list()
cfg$Proxy         = Proxy_files_to_import  
cfg$includePCmaps = includePCmaps
cfg$FullSet       = FullSet
cfg$GoogleMaps    = GoogleMaps
pb = pbarwrapper(title = paste0("AHA_Proxy_KA_Postprocessing, ",as.character(Sys.time())),label = "Loading files", max = 5+Proxy_files_to_import);

# Load the required data files -------------------------------------
load(paste0(settings$Input_Datasets,"/23. Validatie_data/Validatie koppelingen.Rda"))
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_BAR.Rda"))
assets_BAR=assets
load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets_NOR.Rda"))
assets_NOR=assets
rm(assets)

load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))

if (cfg$GoogleMaps){
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_6_Spatial.Rda"))
  load(paste0(settings$Ruwe_Datasets,"/10. BAG/PC_4_Spatial.Rda"))  
  pc6@data$PC_4 = substr(pc6@data$POSTCODE,1,4)
}

l_ply(assets_BAR[1:4],function(x) try(setnames(x,"Status_R","Status_ID")))
setnames(assets_BAR$LSkabels,"Hoofdleiding","ID_Hoofdleiding")
setnames(assets_BAR$LSkabels,"ID_LS_Hoofdleiding","ID_Verbinding")
setnames(assets_BAR$MSkabels,"ID_HLD_MS","ID_Verbinding")
setnames(assets_BAR$LSkabels,"Bouwejaar","Bouwjaar")

# Convert some stuff ------------------------------
setpbarwrapper(pb,label = "Converting objects"); 

# BAR
assets_BAR$MSHLDROUTE = NULL
assets_BAR$kabels = rbind(assets_BAR$LSkabels,assets_BAR$MSkabels,fill=TRUE)[,list(Status_ID,DateLength_ch,ID_NAN,Length_ch,DateRemoved,DateAdded,Coo_X_van,Coo_Y_van,PC_6_van,Coo_X_naar,Coo_Y_naar,PC_6_naar,ID_Hoofdleiding,ID_Verbinding)]
setkey(assets_BAR$kabels,ID_NAN);   
assets_BAR$kabels = assets_BAR$kabels[DateAdded>"2014-01-04"]
assets_BAR$kabels = unique(assets_BAR$kabels)

assets_BAR$moffen = rbind(assets_BAR$LSmoffen,assets_BAR$MSmoffen,fill=TRUE)[,list(Status_ID,ID_NAN,DateRemoved,DateAdded,Coo_X,Coo_Y,PC_4,PC_6)]
assets_BAR$moffen = assets_BAR$moffen[DateAdded>"2014-01-04"]
assets_BAR$moffen = unique(assets_BAR$moffen)

assets_BAR$moffen[,PC_4:=substr(assets_BAR$moffen$PC_6,1,4)]
assets_BAR$kabels[,PC_4_van:=substr(assets_BAR$kabels$PC_6_van,1,4)]
assets_BAR$kabels[,PC_4_naar:=substr(assets_BAR$kabels$PC_6_naar,1,4)]  

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

# KLAK melders  
storingen$KLAKMelders$Melders = "Melder"

# KLAK
storingen$all = rbind(storingen$LS,storingen$MS,fill=TRUE)[,list(Datum,Status,ID_KLAK_Melding,Getroffen_klanten_totaal,Duur_onderbreking,Datum_Eerste_Ontwerp,Netcomponent,Mof,Veroorzaker,Tijdstip_definitief_einde)]#,Coo_X,Coo_Y,PC_4,PC_6
storingen$KLAKMelders[,Coo_X:=as.numeric(gsub(",",".",Coo_X))];storingen$KLAKMelders[,Coo_Y:=as.numeric(gsub(",",".",Coo_Y))];

storingen$all[,Storing:="Storing"]
setkey(storingen$KLAKMelders,ID_KLAK_Melding)
setkey(storingen$all,ID_KLAK_Melding)
storingen$all = unique(storingen$KLAKMelders[,list(Coo_X,Coo_Y,ID_KLAK_Melding)])[storingen$all]

setkey(ValidatieSet,ID_KLAK_Melding)
ValidatieSet=unique(storingen$KLAKMelders[,list(ID_KLAK_Melding,Aantal_Melders)])[ValidatieSet]

# Create file with found assets  -------------------------------
setpbarwrapper(pb, label = "Calculating Proxy  ");  

if(cfg$Proxy>0) 
{setpbarwrapper(pb,"Select files for proxy\n")
ValidatieSet = IncludeProxy(ValidatieSet,pb,cfg)
}

setpbarwrapper(pb, label = "Calculating ValidatieSet");  
ValidatieSet[,in_NORlog:=(ID_NAN %in% assets_NOR$all$ID_NAN)]
ValidatieSet[,in_BARlog:=(ID_NAN %in% assets_BAR$all$ID_NAN)]
ValidatieSet[,in_KLAK:=(ID_KLAK_Melding %in% storingen$all$ID_KLAK_Melding)]
ValidatieSet[,in_KLAKMelders:=(ID_KLAK_Melding %in% storingen$KLAKMelders$ID_KLAK_Melding)]
ValidatieSet[,in_NORlog_KLAK := (in_NORlog&in_KLAK)]
ValidatieSet[,in_BARlog_KLAK := (in_BARlog&in_KLAK)]
setkey(ValidatieSet,ID_KLAK_Melding)
ValidatieSet[,KLAK_has_Coordinates:=!is.na(storingen$all[ValidatieSet]$Coo_X)]
setkey(ValidatieSet,ID_NAN)
ValidatieSet[,NORlog_has_Coordinates := (!is.na(assets_NOR$all[ValidatieSet]$Coo_X)|!is.na(assets_NOR$all[ValidatieSet]$Coo_X_van))]
ValidatieSet[,BARlog_has_Coordinates := (!is.na(assets_BAR$all[ValidatieSet]$Coo_X)|!is.na(assets_BAR$all[ValidatieSet]$Coo_X_van))]
ValidatieSet[,in_NORlog_and_XY:=(in_NORlog_KLAK&KLAK_has_Coordinates&NORlog_has_Coordinates)]
ValidatieSet[,in_BARlog_and_XY:=(in_BARlog_KLAK&BARlog_has_Coordinates&KLAK_has_Coordinates)]

if (cfg$FullSet){
  setpbarwrapper(pb, label = "Loading full asset datasets");  
  ValidatieSet = FullDataAnalytics("NOR",ValidatieSet,FALSE)  
  ValidatieSet = FullDataAnalytics("BAR",ValidatieSet,TRUE)
}

# Write the results to an excel file
setpbarwrapper(pb, label = "Saving to xlsx");  

time = gsub(":","-",as.character(Sys.time()))
write.xlsx(ValidatieSet,file=paste0(settings$Analyse_Datasets,"/2. Proxy validatie/Validatie_Meta_Resultaten_",time,".xlsx"),sheetName="Format Wide",row.names=FALSE,append=cfg$FullSet)

ValidatieSet[,Aantal_Melders:=NULL]
molten = melt(ValidatieSet, id.vars = c("Regio","Datum","ID_KLAK_Melding","ID_NAN","Opmerkingen"))
write.xlsx(molten,file=paste0(settings$Analyse_Datasets,"/2. Proxy validatie/Validatie_Resulaten_",time,".xlsx"),sheetName="Format Melt",row.names=FALSE,  append=TRUE)


ggplot(molten,aes(variable,fill=paste(value))) + geom_bar(colour="black") + coord_flip()

# Create the maps ----------------------------
  if (cfg$GoogleMaps){
    
    setpbarwrapper(pb,label="Starting Google maps");
    
     for (ID_KLAK in ValidatieSet$ID_KLAK_Melding[as.logical(ValidatieSet$in_NORlog_and_XY)]){
       setpbarwrapper(pb, pc,label =paste0("Generating google maps, KLAK: ",ID_KLAK)); pc=pc+pcmax
       Createmaps(assets_NOR,storingen,pc4,pc6,ID_KLAK,ValidatieSet[ID_KLAK_Melding==ID_KLAK,ID_NAN],"NORlog",cfg)
     }
    for (ID_KLAK in ValidatieSet$ID_KLAK_Melding[as.logical(ValidatieSet$in_BARlog_and_XY)]){
      try({setpbarwrapper(pb, pc,label = paste0("Generating google maps, KLAK: ",ID_KLAK)); pc=pc+pcmax})
      Createmaps(assets_BAR,storingen,pc4,pc6,ID_KLAK,ValidatieSet[ID_KLAK_Melding==ID_KLAK,ID_NAN],"BARlog",cfg)
    }
  }
  setpbarwrapper(pb, label = "done"); 
  
}

# Put it on a map -------------------------------------------------------
Createmaps = function(assets,storingen,pc4,pc6,ID_KLAK_M,ID_NAN_M,asset_source,cfg){
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
  
  map = 0 # Prevents issues on empty set
  
  if(cfg$includePCmaps){
    map=pGMwrapper(pc6[pc6$PC_4==PC_4_M,],"Postcode 6 gebieden","POSTCODE",heat.colors(nrow(pc6[pc6$PC_4==PC_4_M,])),1,FALSE,options=",fillOpacity=0.2",map)
    cfg.fileadded = "PC"
  } else {cfg.fileadded = ""}
  
  map=pGMwrapper(geosets$storingen[geosets$storingen$ID_KLAK_Melding!=ID_KLAK_M,],"Omringende Storingen","Storing","#FF8566",3,FALSE,map)
  map=pGMwrapper(geosets$moffen[geosets$moffen$ID_NAN!=ID_NAN_M,],"Omringende Moffen","Status_ID",c("#248F24","#B26B24"),3,FALSE,map)
  map=pGMwrapper(geosets$kabels[geosets$kabels$ID_NAN!=ID_NAN_M,],"Omringende Kabels","Status_ID",c("#248F24","#003399","#B26B24"),3,FALSE,map)
  
  map=pGMwrapper(geosets$kabels[geosets$kabels$ID_NAN==ID_NAN_M,],"Betrokken Kabels","Status_ID","#B80000",5,FALSE,map)
  map=pGMwrapper(geosets$moffen[geosets$moffen$ID_NAN==ID_NAN_M,],"Betrokken Moffen","Status_ID","#B80000",5,FALSE,map)
  
  map=pGMwrapper(geosets$KLAKMelders[geosets$KLAKMelders$ID_KLAK_Melding==ID_KLAK_M,],"Betrokken Melders","Melders","#E65C00",3,FALSE,map)
  map=pGMwrapper(geosets$storingen[geosets$storingen$ID_KLAK_Melding==ID_KLAK_M,],"Betrokken Storing","Storing","#B80000",3,last=TRUE,previousmap=map,
                 file=paste0(settings$Analyse_Datasets,"/3. Geo data/Google_maps_KLAK_",ID_KLAK_M,"_",asset_source,cfg.fileadded,".html"))
  
  
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
FullDataAnalytics = function(AssetName = "NOR",ValidatieSet,append)
{
load(paste0("C:/Datasets/AHAdata/2. Input Datasets/2. All Assets/Asset_Data_",AssetName,"_assets.Rda"))
assets$MSHLDROUTE =NULL

l_ply(assets, function(x) setkey(x,ID_NAN))
setorder(assets$kabels,DateLength_ch,na.last=TRUE)

assets = rbindlist(llply(assets,function(x) 
{setkey(x,ID_NAN);
  if(any(names(x)=="Length_ch"))
    {x = unique(x[,list(Brontabel,ID_NAN,Status_ID,DateRemoved,DateAdded,DateLength_ch,Length_ch,Coo_X_van,Coo_Y_van,Coo_X_naar,Coo_Y_naar,ID_Hoofdleiding,Routenaam_MS)])}
    else
    {x = unique(x[,list(Brontabel,ID_NAN,Status_ID,DateRemoved,DateAdded,Coo_X,Coo_Y,ID_Hoofdleiding,Routenaam_MS)])}
  return(x)
}),fill=TRUE)

ValidatieSet[,inAllNOR:=ID_NAN %in% assets$ID_NAN]

assets = assets[ValidatieSet]

assets[ID_NAN %in% assets$ID_NAN]

ValidatieSet[,inAllNOR:=ID_NAN %in% assets$ID_NAN]

write.xlsx(assets,
           file=paste0(settings$Analyse_Datasets,"/2. Proxy   validatie/Validatie_Meta.xlsx"),
           sheetName=AssetName,row.names=FALSE, append=append)

return(ValidatieSet)
}

# Analyse the proxy results ---------------------------------------------
IncludeProxy = function(ValidatieSet,pb,cfg)
{
  files = llply (1:cfg$Proxy,
  function(x) choose.files(default = paste0(settings$Analyse_Datasets,"/1. KA Proxy/*.Rda")))

                           
  for (zz in 1:cfg$Proxy){
    file=files[[zz]]
    setpbarwrapper(pb,label=paste0("Importing proxy data for file: ",basename(file)));  
    load(file); 
    
    ProxyName = substr(basename(file),9,24) 
    # Convert to data tables
    
    valdt = data.table(ldply(assetsltb[[2]],calcklak1))
    
    setkey(ValidatieSet,ID_KLAK_Melding)
    setkey(valdt,ID_KLAK_Melding)
    
    write.csv(valdt[ValidatieSet],file=paste0(substr(file,1,nchar(file)-4),".csv"))
    ValidatieSet = valdt[ValidatieSet][,any(ID_NAN %in% i.ID_NAN),by=ID_KLAK_Melding][ValidatieSet]
    setnames(ValidatieSet,"V1",ProxyName)
  }
  
  return(ValidatieSet)
}                        

calcklak1 = function(y)
{  All_ID_KLAK = as.character(names(y));
   ldply(All_ID_KLAK, calcklak2,y=y)
}

calcklak2 = function(x,y) 
{ 
  a=1
  if (any("DateLength_ch" %in% colnames(y[[x]])) | any("DateLength_ch" %in% colnames(y)))
    {ifelse(any(class(y[[x]])=="character"),
                    {z = y[,list(ID_NAN,DateRemoved,DateAdded,DateLength_ch,Length_ch,Coo_X_van,Coo_Y_van,PC_6_van,Coo_X_naar,Coo_Y_naar,PC_6_naar)]},
                    {z = y[[x]][,list(ID_NAN,DateRemoved,DateAdded,DateLength_ch,Length_ch,Coo_X_van,Coo_Y_van,PC_6_van,Coo_X_naar,Coo_Y_naar,PC_6_naar)]})}
  else 
    {ifelse(any(class(y[[x]])=="character"),
                    {z = y[,list(ID_NAN,DateRemoved,DateAdded,Coo_X,Coo_Y,PC_6)]},
                    {z = y[[x]][,list(ID_NAN,DateRemoved,DateAdded,Coo_X,Coo_Y,PC_6)]})}
  z$ID_KLAK_Melding=x; 
  z
}