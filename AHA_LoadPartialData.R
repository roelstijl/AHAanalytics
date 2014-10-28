AHA_LoadPartialData = function()
{
  # Loads several month of AHA datasets
  
  maanden = c("1206", "1205","1204", "1203", "1202", "1201", "1112", "1111", "1110", "1109")
  
  # Load new data
  
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/6. NOR/masterset_backupELCVERBINDINGSKNOOPPUNTEN_1206.Rda")
  moffen = masterset[pmatch(masterset$DateAdded, maanden, dup = TRUE,nomatch=0)|pmatch(masterset$DateRemoved, maanden, dup = TRUE,nomatch=0)>0,]
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/6. NOR/masterset_backupELCVERBINDINGSDELEN_1206.Rda")
  kabels = masterset[pmatch(masterset$DateAdded, maanden, dup = TRUE,nomatch=0)|pmatch(masterset$DateRemoved, maanden, dup = TRUE,nomatch=0)>0,]
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/6. NOR/masterset_backupELCVERBINDINGEN_1206.Rda")
  verbindingen = masterset[pmatch(masterset$DateAdded, maanden, dup = TRUE,nomatch=0)|pmatch(masterset$DateRemoved, maanden, dup = TRUE,nomatch=0)>0,]
  
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/11. Nettopologie/aansluitingen_stationinclbehuizing.Rda")
  aansluitingen1 = mindataset
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/11. Nettopologie/aansluitingengeotrace.Rda")
  aansluitingen2 = mindataset
  
  # Koppeling EAN naar hoofdleiding
  tablecount<-data.frame(table(aansluitingen1$ID_EAN))               
  data3=merge(aansluitingen1[-which(aansluitingen1$ID_EAN==""),], tablecount, by.x="ID_EAN", by.y="Var1")
  data3=data3[which(data3$Freq==1),]
  pm = pmatch(colnames(aansluitingen1),colnames(aansluitingen2))
  aansluitingen2 = aansluitingen2[,pm]
  EANtoHFD<-rbind(aansluitingen2,data3[,1:9])

  
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_LS.Rda")
  mindataset$Maand = sapply(mindataset$Maand,fixdates)
  KLAK_LS= mindataset[pmatch(mindataset$Maand, maanden, dup = TRUE,nomatch=0)>0,]
  KLAK_LS = data.frame(KLAK_LS)
  KLAK_LS$Coo_X=as.numeric(sapply(KLAK_LS$Coo_X,fixnumber))
  KLAK_LS$Coo_Y=as.numeric(sapply(KLAK_LS$Coo_Y,fixnumber))
  
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_MS.Rda")
  mindataset$Maand = sapply(mindataset$Maand,fixdates) 
  KLAK_MS= mindataset[pmatch(mindataset$Maand, maanden, dup = TRUE,nomatch=0)>0,]
  
  KLAK_MS = data.frame(KLAK_MS)
  KLAK_MS$Coo_X=as.numeric(sapply(KLAK_MS$Coo_X,fixnumber))
  KLAK_MS$Coo_Y=as.numeric(sapply(KLAK_MS$Coo_Y,fixnumber))
  
  
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_COMPENSATIE.Rda")
  KLAK_COMP = mindataset;
  
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/8. CAR/CAR_2012_XY.Rda")
  CARXYPC = mindataset[,c("PC_6","Huisnr","CO_X","CO_Y","ID_EAN")]
  
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_KOPPEL_MELDING_GROEP.Rda")
  KLAKMELDERS = mindataset;
  
  # Combine KLAK data
  nmelders = table(KLAKMELDERS$ID_Groep)
  ugroep   = KLAKMELDERS[KLAKMELDERS$ST_Groep_eerste=="Ja",]
  frequ    = table(KLAKMELDERS[,2])
  ugroep  = merge(x=ugroep,y=frequ,by.x="ID_Groep",by.y="Var1",all.x=TRUE)
  colnames(ugroep)[c(1,2,9)]=c("ID_KLAK_Melding","ID_Groep","Aantal_Melders")
  
  KLAK_LS=merge(x = KLAK_LS, y = ugroep[,c("ID_KLAK_Melding","Aantal_Melders","ID_Groep")], by = "ID_KLAK_Melding", all.x=TRUE)
  KLAK_MS=merge(x = KLAK_MS, y = ugroep[,c("ID_KLAK_Melding","Aantal_Melders","ID_Groep")], by = "ID_KLAK_Melding", all.x=TRUE)
  
  # Zit er nog niet in:
  # BARlog
  # Koppeling mof -> kabel -> hoofdleiding (-> route, later)
  # 
  
  save(moffen,kabels,EANtoHFD,CARXYPC,KLAK_COMP,verbindingen,KLAK_LS,KLAK_MS,KLAKMELDERS,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data.Rda"))
}

fixdates = function(x) {
  paste0(substr(x,3,4),substr(x,6,7))
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

convert_SDO_GEOMETRY = function(mdsys)
{
  mdsys = "MDSYS.SDO_GEOMETRY(2001,28992,MDSYS.SDO_POINT_TYPE(185462.693,436911.424,NULL),NULL,NULL)"
  split1 = strsplit(mdsys, "\\(|\\)")[[1]]
  funsplit = function(x) {strsplit(x,",")}
  split2 = sapply(split1,funsplit,simplify = TRUE, USE.NAMES = FALSE)
  output = split2[[3]][1:length(split2[[3]])-1]
  Return(output)
}

# MDSYS.SDO_GEOMETRY(2001,28992,MDSYS.SDO_POINT_TYPE(185462.693,436911.424,NULL),NULL,NULL)  

