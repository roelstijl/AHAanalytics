#Written by Michiel Musterd - 26-02-2015
#Script to convert the KNMI set into the set needed for the MVA

AHA_KNMIconvertMonth = function (){
  
  #read in the KNMI set
  load(paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_20150223_v02.Rda"))

  count(mindataset[,Plaatsnaam_Weerstation])
  
  #add info to the dataset indicating the month
  
  maand_index=data.table(regexpr("-",mindataset$Datum_Meting))
  mindataset[,Maand_Jaar_Meting:=substr(Datum_Meting,maand_index$V1+1,nchar(Datum_Meting))]
  
  
  
  #add info indicating whether this is a dry day or not
  mindataset[,Dry_Days:=ifelse(is.na(as.numeric(Etmaalsom_Neerslag)),NA,ifelse(as.numeric(Etmaalsom_Neerslag)>0,0,1))]
  
  #select the different pieces of information per station and month
  meanTemp=mindataset[,mean(as.numeric(Etmaalgemiddelde_Temperatuur)),by=c("Plaatsnaam_Weerstation","Maand_Jaar_Meting")]
  sumRain=mindataset[,sum(as.numeric(Etmaalsom_Neerslag)),by=c("Plaatsnaam_Weerstation","Maand_Jaar_Meting")]
  dryDays=mindataset[,sum(as.numeric(Dry_Days)),by=c("Plaatsnaam_Weerstation","Maand_Jaar_Meting")]
  
  setkey(meanTemp,Plaatsnaam_Weerstation,Maand_Jaar_Meting)
  setkey(sumRain,Plaatsnaam_Weerstation,Maand_Jaar_Meting)
  setkey(dryDays,Plaatsnaam_Weerstation,Maand_Jaar_Meting)
  setkey(mindataset,Plaatsnaam_Weerstation,Maand_Jaar_Meting)
  
  unimindataset=unique(mindataset)
  
  CoupledT=unimindataset[meanTemp]
  CoupledTR=CoupledT[sumRain]
  CoupledTRD=CoupledTR[dryDays]
  setnames(CoupledTRD,c("V1","i.V1","i.V1.1"),c("gem_temperatuur","som_neerslag","aantal_drogedagen"))

  jr_index=data.table(regexpr("-",CoupledTRD$Maand_Jaar_Meting))
  CoupledTRD[,Maand_Meting:=substr(Maand_Jaar_Meting,1,jr_index$V1-1)]
  CoupledTRD[,Jaar_Meting:=substr(Maand_Jaar_Meting,jr_index$V1+1,nchar(Maand_Jaar_Meting))]
  
  mindataset=CoupledTRD[,c("Coo_X","Coo_Y","Plaatsnaam_Weerstation","Maand_Jaar_Meting","Maand_Meting","Jaar_Meting","gem_temperatuur","som_neerslag","aantal_drogedagen"),with=F]
  
  #set the classes properly
  mindataset$Coo_X=as.numeric(mindataset$Coo_X)
  mindataset$Coo_Y=as.numeric(mindataset$Coo_Y)
  mindataset$Maand_Meting=as.factor(mindataset$Maand_Meting)
  mindataset$Jaar_Meting=as.factor(mindataset$Jaar_Meting)
  
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_ByMonth.Rda"))
  
 
}

AHA_KNMIconvertYear = function (){
  
  #read in the KNMI set
  load(paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_20150223_v02.Rda"))
  
  count(mindataset[,Plaatsnaam_Weerstation])
  
  #add info to the dataset indicating the month
  
  maand_index=data.table(regexpr("-",mindataset$Datum_Meting))
  mindataset[,Maand_Jaar_Meting:=substr(Datum_Meting,maand_index$V1+1,nchar(Datum_Meting))]  
  jaar_index=data.table(regexpr("-",mindataset$Maand_Jaar_Meting))
  mindataset[,Jaar_Meting:=substr(Maand_Jaar_Meting,jaar_index$V1+1,nchar(Maand_Jaar_Meting))]  
  
  
  #add info indicating whether this is a dry day or not
  mindataset[,Dry_Days:=ifelse(is.na(as.numeric(Etmaalsom_Neerslag)),NA,ifelse(as.numeric(Etmaalsom_Neerslag)>0,0,1))]
  
  #select the different pieces of information per station and month
  meanTemp=mindataset[,mean(as.numeric(Etmaalgemiddelde_Temperatuur)),by=c("Plaatsnaam_Weerstation","Jaar_Meting")]
  sumRain=mindataset[,sum(as.numeric(Etmaalsom_Neerslag)),by=c("Plaatsnaam_Weerstation","Jaar_Meting")]
  dryDays=mindataset[,sum(as.numeric(Dry_Days)),by=c("Plaatsnaam_Weerstation","Jaar_Meting")]
  
  setkey(meanTemp,Plaatsnaam_Weerstation,Jaar_Meting)
  setkey(sumRain,Plaatsnaam_Weerstation,Jaar_Meting)
  setkey(dryDays,Plaatsnaam_Weerstation,Jaar_Meting)
  setkey(mindataset,Plaatsnaam_Weerstation,Jaar_Meting)
  
  unimindataset=unique(mindataset)
  
  CoupledT=unimindataset[meanTemp]
  CoupledTR=CoupledT[sumRain]
  CoupledTRD=CoupledTR[dryDays]
  setnames(CoupledTRD,c("V1","i.V1","i.V1.1"),c("gem_temperatuur","som_neerslag","aantal_drogedagen"))
  
  
  mindataset=CoupledTRD[,c("Coo_X","Coo_Y","Plaatsnaam_Weerstation","Jaar_Meting","gem_temperatuur","som_neerslag","aantal_drogedagen"),with=F]
  
  #set the classes properly
  mindataset$Coo_X=as.numeric(mindataset$Coo_X)
  mindataset$Coo_Y=as.numeric(mindataset$Coo_Y)
  mindataset$Jaar_Meting=as.factor(mindataset$Jaar_Meting)
  
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_ByYear.Rda"))
  
  
}

AHA_KNMIconvert20072014 = function (){
  
  #read in the KNMI set
  load(paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_20150223_v02.Rda"))
  
  count(mindataset[,Plaatsnaam_Weerstation])
  
  #add info to the dataset indicating the month
  
  maand_index=data.table(regexpr("-",mindataset$Datum_Meting))
  mindataset[,Maand_Jaar_Meting:=substr(Datum_Meting,maand_index$V1+1,nchar(Datum_Meting))]  
  jaar_index=data.table(regexpr("-",mindataset$Maand_Jaar_Meting))
  mindataset[,Jaar_Meting:=substr(Maand_Jaar_Meting,jaar_index$V1+1,nchar(Maand_Jaar_Meting))]  
  
  
  #add info indicating whether this is a dry day or not
  mindataset[,Dry_Days:=ifelse(is.na(as.numeric(Etmaalsom_Neerslag)),NA,ifelse(as.numeric(Etmaalsom_Neerslag)>0,0,1))]
  
  #select only the rows with year>2007
  mindataset=mindataset[as.numeric(mindataset$Jaar_Meting)>2007,]
  
  #select the different pieces of information per station and month
  meanTemp=mindataset[,mean(as.numeric(Etmaalgemiddelde_Temperatuur),na.rm=T),by=c("Plaatsnaam_Weerstation")]
  sumRain=mindataset[,sum(as.numeric(Etmaalsom_Neerslag),na.rm=T),by=c("Plaatsnaam_Weerstation")]
  dryDays=mindataset[,sum(as.numeric(Dry_Days),na.rm=T),by=c("Plaatsnaam_Weerstation")]
  
  setkey(meanTemp,Plaatsnaam_Weerstation)
  setkey(sumRain,Plaatsnaam_Weerstation)
  setkey(dryDays,Plaatsnaam_Weerstation)
  setkey(mindataset,Plaatsnaam_Weerstation)
  
  unimindataset=unique(mindataset)
  
  CoupledT=unimindataset[meanTemp]
  CoupledTR=CoupledT[sumRain]
  CoupledTRD=CoupledTR[dryDays]
  setnames(CoupledTRD,c("V1","i.V1","i.V1.1"),c("gem_temperatuur","som_neerslag","aantal_drogedagen"))
  
  
  mindataset=CoupledTRD[,c("Coo_X","Coo_Y","Plaatsnaam_Weerstation","gem_temperatuur","som_neerslag","aantal_drogedagen"),with=F]
  
  #set the classes properly
  mindataset$Coo_X=as.numeric(mindataset$Coo_X)
  mindataset$Coo_Y=as.numeric(mindataset$Coo_Y)
  
  save(mindataset,file=paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_grouped_2007_2014.Rda"))
  
  
}