AHA_Proxy_KA_BAR_NOR = 
  function(method,assettypes=c("LSkabels","MSkabels","LSmoffen","MSmoffen")) 
  {
    # This function calculates the asset id - klak id proxy for the asset health analytics project
    # Data should be loaded using the AHA_Proxy_Dataset function (global environment)
    #
    # Input:
    # Method refers to the proxy method used, GEO, PC, HLD or OLD
    # Asset refers to the asset type, e.g. moffen, kabels or both
    # Voltage refers to the voltage to use, can be MS, LS or both
    
# Configuration parameters and settings ---------------------------
config = list()
config$timediff$min = -30 # Aantal dagen tussen storing en verwijdering assets
config$timediff$max = 70  # Aantal dagen tussen storing en verwijdering assets
config <<- config

# Load data if not available -----------------------------
if (!exists("assets")) {
  cat("Importing data file \n"); tic()
  load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets.Rda"),envir = .GlobalEnv)
  load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo.Rda"),envir = .GlobalEnv)
  load(paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"),envir = .GlobalEnv)
  toc();
  }; 

# Quick Fixes -------------------------------------
try(setnames(storingen$LS,"PC_6.x","PC_6"))
storingen$LS[,PC_6:=gsub(" ","",storingen$LS$PC_6, fixed=TRUE)]
storingen$MS[,PC_6:=gsub(" ","",storingen$MS$PC_6, fixed=TRUE)]
storingen$LS = as.Date(storingen$LS$Tijdstip_begin_storing)
storingen$MS = as.Date(storingen$MS$Tijdstip_begin_storing)

#storingen$LS                                          <- data.frame(storingen$LS )   #Quick fix
#storingen$MS                                          <- data.frame(storingen$MS )   #Quick fix

# Omzetten assetset -------------------------------------------------------

#assetstemp    <- assets
#assets        <- list()
#assets$moffen <- rbind(assetstemp$mof_LS,assetstemp$mof_MS)
#assets$kabels <- rbind(assetstemp$kbl_LS,assetstemp$kbl_MS)

#set keys for different methods
switch(method,
       "PC"={
         setkey(assets$LSmoffen,PC_6)
         setkey(assets$MSmoffen,PC_4)
         setkey(assets$LSkabels,PC_6_van,PC_6_naar)
         setkey(assets$MSkabels,PC_4_van,PC_4_naar)
       },
       "TOPO"={
         setkey(assets$LSmoffen,ID_Hoofdleiding)
         #setkey(assets$MSmoffen,MS_Routenaam)
         setkey(assets$LSkabels,ID_Hoofdleiding)
         setkey(assets$MSkabels,Routenaam_MS)
       },
       "XY"={
         setkey(assets$LSmoffen,Coo_X,Coo_Y)
         setkey(assets$MSmoffen,Coo_X,Coo_Y)
         setkey(assets$LSkabels,Coo_X_naar,Coo_Y_naar,Coo_X_van,Coo_Y_van)
         setkey(assets$MSkabels,Coo_X_naar,Coo_Y_naar,Coo_X_van,Coo_Y_van)
       })
setkey(storingen$KLAKMELDERS,ID_Groep)

# Loop over assettype --------------------------------------------------
cat("")
for (assettype in assettypes) {
      voltage = substr(paste(assettype),1,2)
      assetklak= data.table(
                   ID_KLAK_Melding        =character(), 
                   Netcomponent           =character(), 
                   Tijdstip_begin_storing = as.Date(character()),
                   Gmu_Verwerking_Gereed  = as.Date(character()),
                   PC_6                   = character(),
                   ID_Groep               = character())
                     
      cat(paste(assettype,voltage,nrow(storingen[[voltage]]),"\n"))
      #aanmaken klakgegevenstabel
      klaktabel    <- storingen[[voltage]][,c('ID_KLAK_Melding','Netcomponent','Tijdstip_begin_storing',"Datum_Verwerking_Gereed", 'PC_6', 'ID_Groep'),with=FALSE]   #aanmaken tabel met klakmeldingen
      
for(klaknr in klaktabel$ID_KLAK_Melding){
  Proxy_PC_6(klaktabel[ID_KLAK_Melding==klaknr],storingen$KLAKMELDERS[ID_KLAK_Melding==klaknr],assettype) 
      }

}
}

#  Postcode 6 proxy ---------------------------------    
Proxy_PC_6 = function(klakl,klakmelders,assettype)
{
    assetl<-assets[[assettype]][PC_6_van==klakl$PC_6] # Verschil kabel/mof
    
    if sum(klakadd$Status_ID=="Removed"|klakadd$Status_ID=="Removed") >0
    
  { # minimaal 1 asset
  ###voeg datumverschillen toe
  assetl$Adiff<-assetl$DateAdded   - klakl$Tijdstip_begin_storing
  assetl$Rdiff<-assetl$DateRemoved - klakl$Tijdstip_begin_storing # Voor kabels ook lengte
  assetl$Rdiff<-assetl$length - klakl$Tijdstip_begin_storing # Voor kabels ook lengte
  
  klakl$Valid.diff<- (assetl$Adiff > config$timediff$min) & (assetl$Adiff < config$timediff$max) | 
                     (assetl$Rdiff > config$timediff$min) & (assetl$Rdiff < config$timediff$max) |
    
  
  ### Maak dataframe met mogelijk gevonden klakstoringen
  klaktabel<-data.frame(table(klakl$ID_KLAK_Melding))
  assetl$countadded<-sapply(klaktabel$Var1,function(x) sum(klakl$Adiffc[which(klakl$ID_KLAK_Melding==x)]))     #aantal toegevoegde moffen
  assetl$countremoved<-sapply(klaktabel$Var1,function(x) sum(klakl$Rdiffc[which(klakl$ID_KLAK_Melding==x)]))   #aantal weggehaalde moffen
  #max(klaktabel$countremoved)
  klaktabel[,c("storing")]<-sapply(klaktabel$countadded, function (x) if(x>0){1}else{0})*sapply(klaktabel$countremoved, function (x) if(x>0 & x<6){1}else{0})
  
  colnames(klaktabel)[1]<-"ID_KLAK_Melding"
  klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-NA
  
  tabel<-sapply(klaktabel$ID_KLAK_Melding,function(x) klak$ID_Bron[which(klak$ID_KLAK_Melding==x)])
  tabel<-t(data.frame(lapply(tabel,function(x) x[1:5])))
  
  klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-tabel
  klaktabel<-klaktabel[which(klaktabel$storing==1),]
  
  klaktabelkabelsLS<-klaktabel
    }
}

Proxy_filter = function()
{
  #Selecteer alleen LS kabels
  kabelsklak        <- kabelsklak[which(kabelsklak$Voltage %in% c("400V","Onbekend")),]
  
  #Selecteer verwijderde moffen die in de rondom de storing vervangen zijn
  LSkabelsklakhld <- kabelsklak[which((kabelsklak$Rdiffc==1 & kabelsklak$vervc ==1)|kabelsklak$Ldiffc ==1),]
  LSkabelsklakhld <- transform(LSkabelsklakhld, freq.KLAKmelding=ave(seq(nrow(LSkabelsklakhld)),ID_KLAK_Melding,FUN=length))
  LSkabelsklakhld <- LSkabelsklakhld[which(LSkabelsklakhld$freq.KLAKmelding<6),]
  table(table(LSkabelsklakhld$ID_KLAK_Melding))
  table(LSkabelsklakhld$Netcomponent)
}
  