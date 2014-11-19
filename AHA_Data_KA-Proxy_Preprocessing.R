AHA_Data_KA_Proxy_Preprocessing = function(initialdate="1401",months=10)
  {
  # Loads several months of AHA datasets and puts this into a dataset

# Settings ----------------------------------------------------------------  
  dates = as.Date(paste0(initialdate,"01"), "%y%m%d")
  month(dates) = month(dates)+(1:months)-1
  maanden = format(dates, format="%y%m")
  
# NOR Data ----------------------------------------------------------------
  cat("Load NOR data\n"); tic();

  load(paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))
  assets$moffen = assets$moffen[assets$moffen$DateAdded %in% maanden | assets$moffen$DateRemoved %in% maanden]
  assets$moffen$PC_4=substr(assets$moffen$PC_XY,1,4)
  assets$kabels = assets$kabels[assets$kabels$DateAdded %in% maanden | assets$kabels$DateRemoved %in% maanden | assets$kabels$Date_Length_ch %in% maanden]
  assets$kabels$PC_4=substr(assets$kabels$PC_XY_van,1,4)  
  assets$kabels$PC_4=substr(assets$kabels$PC_XY_naar,1,4)  


# EAN-Hoofdleiding-XY-PC data ----------------
  toc; cat("Load EAN data\n"); tic();
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
  setnames(EAN_to_XY_PC6,c("CO_X","CO_Y"),c("Coo_X","Coo_Y"))
  EAN_to_XY_PC6 = data.table(mindataset[,c("PC_6","Huisnr","Coo_X","Coo_Y","ID_EAN")])
  EAN_to_XY_PC6$PC_4=substr(EAN_to_XY_PC6$PC_6,1,4)
  setkey(EAN_to_XY_PC6,ID_EAN)

  nettopo$EAN_koppel<-merge(rbind(aansluitingen2,data3[,1:9,with=FALSE]),EAN_to_XY_PC6,by="ID_EAN",all.x=TRUE)

# Storingsdata uit KLAK ------------------------
  toc; cat("Load KLAK data\n"); tic();
  storingen=list()
  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_LS.Rda"))
  mindataset$Maand = sapply(mindataset$Datum,fixdates)
  storingen$LS= data.table(mindataset[pmatch(mindataset$Maand, maanden, dup = TRUE,nomatch=0)>0,]);
  
  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_MS.Rda"))
  mindataset$Maand = sapply(mindataset$Datum,fixdates) 
  storingen$MS= data.table(mindataset[pmatch(mindataset$Maand, maanden, dup = TRUE,nomatch=0)>0,])
  
  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_COMPENSATIE.Rda"))
  storingen$Compensatie = data.table(mindataset);

  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_KOPPEL_MELDING_GROEP.Rda"))
  storingen$KLAKMELDERS = data.table(mindataset);
  frequ          = data.table(data.frame(table(storingen$KLAKMELDERS$ID_Groep)))
  setnames(frequ,c("ID_Groep","Aantal_Melders"))
  frequ$ID_Groep = as.integer(frequ$ID_Groep ); 
  ugroep         = merge(storingen$KLAKMELDERS[storingen$KLAKMELDERS$ST_Groep_eerste=="Ja",],frequ,by="ID_Groep",all.x=TRUE)
  setnames(ugroep,c("MELDING","PC6"),c("ID_KLAK_Melding","PC_6"))
  ugroep$Huisnr  = (as.character(ugroep$Huisnr))
  ugroep         = merge(ugroep,EAN_to_XY_PC6[!duplicated(EAN_to_XY_PC6[,c("PC_6","Huisnr"),with=FALSE])],by=c("PC_6","Huisnr"),all.x=TRUE)
  
  storingen$LS=merge(x = storingen$LS, y = ugroep[,c("ID_KLAK_Melding","Aantal_Melders","ID_Groep"),with=FALSE], by = "ID_KLAK_Melding", all.x=TRUE)
  storingen$MS=merge(x = storingen$MS, y = ugroep[,c("ID_KLAK_Melding","Aantal_Melders","ID_Groep"),with=FALSE], by = "ID_KLAK_Melding", all.x=TRUE)

  load(paste0(settings$Ruwe_Datasets,"/21. GIS-mutaties/gis mutaties alles.Rda"))
  storingen$LS=merge(storingen$LS, storingen$LS, by = "ID_KLAK_Melding", all.x=TRUE)
  storingen$MS=merge(storingen$MS, storingen$MS, by = "ID_KLAK_Melding", all.x=TRUE)

  
# Zit er nog niet in:
# BARlog
# Koppeling mof -> kabel -> hoofdleiding ->station (-> route, later)

# Save the data ----------------------------------------------------------------
toc; cat("Save all data\n"); tic();

save(assets,storingen,nettopo,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_",Sys.Date(),".Rda"))

}

fixdates = function(x) {
  return(format(as.Date(x,"%d-%m-%Y"), format="%y%m"))
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

convert_SDO_GEOMETRY = function(mdsys){
  mdsys = "MDSYS.SDO_GEOMETRY(2001,28992,MDSYS.SDO_POINT_TYPE(185462.693,436911.424,NULL),NULL,NULL)"
  split1 = strsplit(mdsys, "\\(|\\)")[[1]]
  funsplit = function(x) {strsplit(x,",")}
  split2 = sapply(split1,funsplit,simplify = TRUE, USE.NAMES = FALSE)
  output = split2[[3]][1:length(split2[[3]])-1]
  Return(output)
}