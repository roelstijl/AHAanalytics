AHA_Data_KA_Proxy_Preprocessing = function(datasets=c("storingen","assets","nettopo"),initialdate="1401",months=12)
  {
  # Loads several months of AHA datasets and puts this into a dataset

# Settings ----------------------------------------------------------------  

  dates = as.Date(paste0(initialdate,"01"), "%y%m%d")
  month(dates) = month(dates)+(1:months)-1
  maandenold = format(dates, format="%y%m")
  maanden = sapply(format(dates, format="%y%m"),firstFri)
  
for (m in datasets)
  {switch (m,
          
assets = {        
# NOR Data ----------------------------------------------------------------
  cat("Load NOR data\n"); tic();

  load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))
  assets$moffen = assets$moffen[c %in% maanden | assets$moffen$DateRemoved %in% maanden]
  setnames(assets$moffen,"PC_XY","PC_6")
  assets$moffen$PC_4=substr(assets$moffen$PC_XY,1,4)
  assets$kabels = assets$kabels[assets$kabels$DateAdded %in% maanden | assets$kabels$DateRemoved %in% maanden | assets$kabels$Date_Length_ch %in% maanden]
  setnames(assets$kabels,c("PC_XY_van","PC_XY_naar"),c("PC_6_van","PC_6_naar"))
  assets$kabels$PC_4_van=substr(assets$kabels$PC_6_van,1,4)  
  assets$kabels$PC_4_naar=substr(assets$kabels$PC_6_naar,1,4)  
  save(assets,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_assets.Rda"))
  remove("assets")
  
},
nettopo = {
# EAN-Hoofdleiding-XY-PC data ----------------
  toc(); cat("Load EAN data\n"); tic();
  load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/aansluitingen_stationinclbehuizing.Rda"))
  aansluitingen1 = data.table(mindataset)
  setkey(aansluitingen1,ID_EAN)
  load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/aansluitingengeotrace.Rda"))
  aansluitingen2 = data.table(mindataset)
  setkey(aansluitingen2,ID_EAN)
  nettopo = list();
  tablecount<-data.frame(table(aansluitingen1$ID_EAN))
  setnames(tablecount,"Var1","ID_EAN")
  data3=merge(aansluitingen1[-which(aansluitingen1$ID_EAN==""),], tablecount, by="ID_EAN")
  data3=data3[which(data3$Freq==1)]
  pm = pmatch(colnames(aansluitingen1),colnames(aansluitingen2))
  aansluitingen2 = aansluitingen2[,pm,with=FALSE]
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/8. CAR/CAR_2013_XY.Rda")
  mindataset$ID_EAN= as.character(mindataset$ID_EAN)
  EAN_to_XY_PC6 = (mindataset[,c("PC_6","Huisnr","Coo_X","Coo_Y","ID_EAN"),with=FALSE])
  EAN_to_XY_PC6$PC_4=substr(EAN_to_XY_PC6$PC_6,1,4)
  setkey(EAN_to_XY_PC6,ID_EAN)

  nettopo$EAN_koppel<-merge(rbind(aansluitingen2,data3[,1:9,with=FALSE]),EAN_to_XY_PC6,by="ID_EAN",all.x=TRUE)
  save(nettopo,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_nettopo.Rda"))
},
storingen = {
# Storingsdata uit KLAK ------------------------
  toc(); cat("Load KLAK data\n"); tic();
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/8. CAR/CAR_2013_XY.Rda")
  mindataset$ID_EAN= as.character(mindataset$ID_EAN)
  EAN_to_XY_PC6 = (mindataset[,c("PC_6","Huisnr","Coo_X","Coo_Y","ID_EAN"),with=FALSE])
  EAN_to_XY_PC6$PC_4=substr(EAN_to_XY_PC6$PC_6,1,4)
  setkey(EAN_to_XY_PC6,ID_EAN)
  storingen=list()
  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_LS.Rda"))
  mindataset$Maand = format(mindataset$Datum, format="%y%m")
  storingen$LS= data.table(mindataset[pmatch(mindataset$Maand, maandenold, dup = TRUE,nomatch=0)>0,]);
  
  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_MS.Rda"))
  mindataset$Maand = format(mindataset$Datum, format="%y%m")
  storingen$MS= data.table(mindataset[pmatch(mindataset$Maand, maandenold, dup = TRUE,nomatch=0)>0,])
  
  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_COMPENSATIE.Rda"))  
  storingen$Compensatie = data.table(mindataset);

  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_KOPPEL_MELDING_GROEP.Rda"))
  storingen$KLAKMELDERS = data.table(mindataset);
  frequ          = data.table(data.frame(table(storingen$KLAKMELDERS$ID_Groep)))
  setnames(frequ,c("ID_Groep","Aantal_Melders"))
  ugroep         = merge(storingen$KLAKMELDERS[storingen$KLAKMELDERS$ST_Groep_eerste=="Ja",],frequ,by="ID_Groep",all.x=TRUE)
  ugroep         = merge(ugroep,EAN_to_XY_PC6[!duplicated(EAN_to_XY_PC6[,c("PC_6","Huisnr"),with=FALSE])],by=c("PC_6","Huisnr"),all.x=TRUE)
  
  ugroep$PC_4=substr(ugroep$PC_6,1,4)

  storingen$LS=merge(storingen$LS, ugroep[,c("ID_KLAK_Melding","Aantal_Melders","ID_Groep","Coo_X","Coo_Y","PC_6","PC_4","Huisnr"),with=FALSE], by = "ID_KLAK_Melding", all.x=TRUE)
  storingen$MS=merge(storingen$MS, ugroep[,c("ID_KLAK_Melding","Aantal_Melders","ID_Groep","Coo_X","Coo_Y","PC_6","PC_4","Huisnr"),with=FALSE], by = "ID_KLAK_Melding", all.x=TRUE)

  load(paste0(settings$Ruwe_Datasets,"/21. GIS-mutaties/GISMUTATIE.Rda"))
  storingen$LS=merge(storingen$LS,mindataset, by = "ID_KLAK_Melding", all.x=TRUE)
  storingen$MS=merge(storingen$MS,mindataset, by = "ID_KLAK_Melding", all.x=TRUE)
  save(storingen,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_storingen.Rda"))

  
# Zit er nog niet in:
# BARlog
# Koppeling mof -> kabel -> hoofdleiding ->station (-> route, later)
})}

# Save the data ----------------------------------------------------------------
toc; cat("Save all data\n"); tic();

# save(assets,storingen,nettopo,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data.Rda"))

}

fixnumber = function(x) {
  val= strsplit(x,",")[[1]];
  
  if (suppressWarnings(!is.na(as.numeric(val[1])))){
    len=length(val); 
    cor=switch(nchar(val[len]),"1"=10,"2"=100,"3"=1000)
    if(len==1) {a=val[1]
    } else if(len==2) {
      a=(as.numeric(val[1])+as.numeric(val[2])/cor)
    } else if(len==3) {
      a=(as.numeric(val[1])*1000+as.numeric(val[2])+as.numeric(val[3])/cor)
    }
  }
  else{
    a=NA
  }
  #cat(paste0(a,", "))
  return(as.numeric(a))
}

firstFri = function(initialdate)
{
  #   Aproximate date of NOR generation, first friday + 2 days
  date = as.Date(paste0(initialdate,"01"), "%y%m%d")
  dow = sapply(seq(0,6),function(x) wday(date+days(x)))
  firstFriday = date + days(which(dow==5)-1)+2
  return(firstFriday)
}