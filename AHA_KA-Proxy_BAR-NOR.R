AHA_Proxy_KA_BAR_NOR = 
  function(method)
  {
    # This function calculates the asset id - klak id proxy for the asset health analytics project
    # Data should be loaded using the AHA_Proxy_Dataset function (global environment)
    #
    # Input:
    # Method refers to the proxy method used, GEO, PC, HLD or OLD
    # Asset refers to the asset type, e.g. moffen, kabels or both
    # Voltage refers to the voltage to use, can be MS, LS or both
    
# Configuration parameters and settings ---------------------------
<<<<<<< HEAD
=======
assettypes=c("kabels","moffen")
voltages  = c("MS","LS")
>>>>>>> Added nonfunctioning proxy
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
<<<<<<< HEAD

# Quick Fixes -------------------------------------
try(setnames(storingen$LS,"PC_6.x","PC_6"))
storingen$LS[,PC_6:=gsub(" ","",storingen$LS$PC_6, fixed=TRUE)]
storingen$MS[,PC_6:=gsub(" ","",storingen$MS$PC_6, fixed=TRUE)]
storingen$LS = as.Date(storingen$LS$Tijdstip_begin_storing)
storingen$MS = as.Date(storingen$MS$Tijdstip_begin_storing)

#storingen$LS                                          <- data.frame(storingen$LS )   #Quick fix
#storingen$MS                                          <- data.frame(storingen$MS )   #Quick fix

# Omzetten assetset -------------------------------------------------------
=======
>>>>>>> Added nonfunctioning proxy

# Quick Fixes -------------------------------------
try(setnames(storingen$LS,"PC_6.x","PC_6")) # doe dit altijd met TRY -> als ik de data dan fix draait het programma ook dan nog
storingen$LS[,PC_6:=gsub(" ","",storingen$LS$PC_6, fixed=TRUE)]
storingen$MS[,PC_6:=gsub(" ","",storingen$MS$PC_6, fixed=TRUE)]
storingen$LS = as.Date(storingen$LS$Tijdstip_begin_storing)
storingen$MS = as.Date(storingen$MS$Tijdstip_begin_storing)

# Omzetten assetset -------------------------------------------------------
#set keys for different methods
switch(method, #netjes
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
<<<<<<< HEAD
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
=======
for (voltage in voltages) { # Je hoeft alleen over voltages te loopen - je weet nog niet wat voor asset
      assetklak= data.table( # dit kan dus zo ;), ik weet alleen niet of dit ècht nodig is
         ID_KLAK_Melding        =character(), 
         Netcomponent           =character(), 
         Tijdstip_begin_storing = as.Date(character()),
         Gmu_Verwerking_Gereed  = as.Date(character()),
         PC_6                   = character(),
         ID_Groep               = character())
                     
      cat(paste(assettype,voltage,nrow(storingen[[voltage]]),"\n"))
      
>>>>>>> Added nonfunctioning proxy
      #aanmaken klakgegevenstabel
      klaktabel    <- storingen[[voltage]][,c('ID_KLAK_Melding','Netcomponent','Tijdstip_begin_storing',"Datum_Verwerking_Gereed", 'PC_6', 'ID_Groep'),with=FALSE]   #aanmaken tabel met klakmeldingen
      
for(klaknr in klaktabel$ID_KLAK_Melding){
<<<<<<< HEAD
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
=======
  assets.suspect = # De output bestaat uit een lijst met mogelijke assets removed, assets added
    switch (method,
          PC = Proxy_PC_6(klaktabel[ID_KLAK_Melding==klaknr],
                          storingen$KLAKMELDERS[ID_KLAK_Melding==klaknr],
                          voltage) # Hier vindt je alle moffen EN kabels die passen bij de melding
          NETTOPO =  functie()
          XY = functie())
  
  # Bepaal vervolgens wel assets in out 
}}}

#  Postcode 6 proxy ---------------------------------    
Proxy_PC_6 = function(klakl,klakmelders,voltage,assettype)
{
  # Kies de assets die voldoen aan de PC6
  assetl = switch (assettype,
          assets = assets[[assettype]][PC_6_van==klakl$PC_6]|assets[[assettype]][PC_6_naar==klakl$PC_6], # Verschil kabel/mof
          moffen = assets[[assettype]][PC_6==klakl$PC_6] # Verschil kabel/mof)  
    
 if sum(assetl$Status_ID=="Removed"|assetl$Status_ID=="Active"|assetl$Status_ID=="Lengthch") ==0 { 
  } else  
  {
  # Bereken de datum verschillen 
  klaklmoffen = calc.date.diff(assetl,klakl,"kabels") # is een funtie die hieronder staat
  
  # Maak dataframe met mogelijk gevonden assets
  assets.suspect = assetl[klakl$Valid.diff]
  
  return(assetl)
>>>>>>> Added nonfunctioning proxy
    }
}

Proxy_filter = function()
{
<<<<<<< HEAD
  #Selecteer alleen LS kabels
  kabelsklak        <- kabelsklak[which(kabelsklak$Voltage %in% c("400V","Onbekend")),]
  
  #Selecteer verwijderde moffen die in de rondom de storing vervangen zijn
  LSkabelsklakhld <- kabelsklak[which((kabelsklak$Rdiffc==1 & kabelsklak$vervc ==1)|kabelsklak$Ldiffc ==1),]
  LSkabelsklakhld <- transform(LSkabelsklakhld, freq.KLAKmelding=ave(seq(nrow(LSkabelsklakhld)),ID_KLAK_Melding,FUN=length))
  LSkabelsklakhld <- LSkabelsklakhld[which(LSkabelsklakhld$freq.KLAKmelding<6),]
  table(table(LSkabelsklakhld$ID_KLAK_Melding))
  table(LSkabelsklakhld$Netcomponent)
}
  
=======
  #Selecteer alleen de verwachte asset
  kabelsklak        <- 
  
  #Selecteer verwijderde moffen die in de rondom de storing vervangen zijn

}
  
calc.date.diff = function(assetl,klakl,assettype){ # je moet deze even fixen voor moffen + kabels
  diff = 
    switch(moffen = data.table(
        a = assetl$DateAdded   - klakl$Tijdstip_begin_storing,
        b = assetl$DateRemoved - klakl$Tijdstip_begin_storing),
          kabel = data.table(
        a = assetl$DateAdded   - klakl$Tijdstip_begin_storing,
        b = assetl$DateRemoved - klakl$Tijdstip_begin_storing, # Voor kabels ook lengte
        c = assetl$DateLength_ch - klakl$Tijdstip_begin_storing) # Voor kabels ook lengte
    )
  if assettype == "moffen"{
    diff[c:=NULL]}
  
  cbind(diff[1], mycol = na.omit(unlist(diff[-1])))
  
  assetl$in.timediff((assetl$Adiff > config$timediff$min) & (assetl$Adiff < config$timediff$max) | 
    (assetl$Rdiff > config$timediff$min) & (assetl$Rdiff < config$timediff$max) |
    (assetl$Ldiff > config$timediff$min) & (assetl$Ldiff < config$timediff$max) )
  return(assetl)
}
>>>>>>> Added nonfunctioning proxy
