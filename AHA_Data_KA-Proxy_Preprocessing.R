AHA_Proxy_Dataset = function(initialdate="1401",months=10)
  {
  # Loads several months of AHA datasets and puts this into a dataset

# Settings ----------------------------------------------------------------  
  dates = as.Date(paste0(initialdate,"01"), "%y%m%d")
  month(dates) = month(dates)+(1:months)-1
  maanden = format(dates, format="%y%m")
  
# Load the data ----------------------------------------------------------------
  load(paste0(settings$Input_Datasets,"/Asset_Data_NOR_kabels_2014-11-17.Rda"))
  assets$kbl_LS = assets$kbl_LS[assets$kbl_LS$DateAdded %in% maanden | assets$kbl_LS$DateRemoved %in% maanden | assets$kbl_LS$Date_Length_ch %in% maanden]
  assets$kbl_MS = assets$kbl_MS[assets$kbl_MS$DateAdded %in% maanden | assets$kbl_MS$DateRemoved %in% maanden | assets$kbl_LS$Date_Length_ch %in% maanden]
  temp=assets

  load(paste0(settings$Input_Datasets,"/Asset_Data_NOR_moffen_2014-11-17.Rda"))
  assets$mof_LS = assets$mof_LS[assets$mof_LS$DateAdded %in% maanden | assets$mof_LS$DateRemoved %in% maanden]
  assets$mof_MS = assets$mof_MS[assets$mof_MS$DateAdded %in% maanden | assets$mof_MS$DateRemoved %in% maanden]
  assets$kbl_LS = temp$kbl_LS  
  assets$kbl_MS = temp$kbl_LS 

  load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/aansluitingen_stationinclbehuizing.Rda"))
  aansluitingen1 = data.table(mindataset)
  setkey(aansluitingen1$ID_EAN)
  load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/aansluitingengeotrace.Rda"))
  aansluitingen2 = data.table(mindataset)
  setkey(aansluitingen2$ID_EAN)
  
  # Koppeling EAN naar hoofdleiding
nettopo = list();
  tablecount<-data.frame(table(aansluitingen1$ID_EAN))               
  data3=merge(aansluitingen1[-which(aansluitingen1$ID_EAN==""),], tablecount, by.x="ID_EAN", by.y="Var1")
  data3=data3[which(data3$Freq==1)]
  pm = pmatch(colnames(aansluitingen1),colnames(aansluitingen2))
  aansluitingen2 = aansluitingen2[,pm,with=FALSE]
  nettopo$EAN_to_HLD<-rbind(aansluitingen2,data3[,1:9,with=FALSE])

  storingen=list()
  load(paste0(settings$Ruwe_Datasets,"/4. KLAK/KLAK_LS.Rda"))
  mindataset$Maand = sapply(mindataset$Maand,fixdates)
  storingen$LS= data.table(mindataset[pmatch(mindataset$Maand, maanden, dup = TRUE,nomatch=0)>0,])
  storingen$LS = data.frame(storingen$LS)
  storingen$LS$Coo_X=as.numeric(sapply(storingen$LS$Coo_X,fixnumber))
  storingen$LS$Coo_Y=as.numeric(sapply(storingen$LS$Coo_Y,fixnumber))
  
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_MS.Rda")
  mindataset$Maand = sapply(mindataset$Maand,fixdates) 
  storingen$MS= data.table(mindataset[pmatch(mindataset$Maand, maanden, dup = TRUE,nomatch=0)>0,])
  
  storingen$MS = data.frame(storingen$MS)
  storingen$MS$Coo_X=as.numeric(sapply(storingen$MS$Coo_X,fixnumber))
  storingen$MS$Coo_Y=as.numeric(sapply(storingen$MS$Coo_Y,fixnumber))
  
  
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_COMPENSATIE.Rda")
  storingen$Compensatie = data.table(mindataset);
  
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/8. CAR/CAR_2013_XY.Rda")
  nettopo$EAN_to_XY_PC6 = data.table(mindataset[,c("PC_6","Huisnr","CO_X","CO_Y","ID_EAN")])
  
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_KOPPEL_MELDING_GROEP.Rda")
  storingen$KLAKMELDERS = data.table(mindataset);
  
  # Combine KLAK data
  nmelders = table(storingen$KLAKMELDERS$ID_Groep)
  ugroep   = storingen$KLAKMELDERS[storingen$KLAKMELDERS$ST_Groep_eerste=="Ja",]
  frequ    = setnames(data.table(table(storingen$KLAKMELDERS$ID_Groep)),"Aantal_Melders")
  frequ$ID_Groep = as.integer(rownames(frequ))
  ugroep  = merge(x=ugroep,y=frequ,by="ID_Groep",all.x=TRUE)
  setnames(ugroep,c("MELDING","ID_Groep","Aantal_Melders"),c("ID_KLAK_Melding","ID_Groep","Aantal_Melders"))
  
  storingen$LS=merge(x = storingen$LS, y = ugroep[,c("ID_KLAK_Melding","Aantal_Melders","ID_Groep"),with=FALSE], by = "ID_KLAK_Melding", all.x=TRUE)
  storingen$MS=merge(x = storingen$MS, y = ugroep[,c("ID_KLAK_Melding","Aantal_Melders","ID_Groep"),with=FALSE], by = "ID_KLAK_Melding", all.x=TRUE)
  
  # Zit er nog niet in:
  # BARlog
  # Koppeling mof -> kabel -> hoofdleiding ->station (-> route, later)
  
  # Save the data ----------------------------------------------------------------
  
  save(assets,storingen,nettopo,file=paste0(settings$Input_Datasets,"/1. AID KID proxy/AHA_Proxy_partial_data_",Sys.Date(),".Rda"))
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

convert_SDO_GEOMETRY = function(mdsys){
  mdsys = "MDSYS.SDO_GEOMETRY(2001,28992,MDSYS.SDO_POINT_TYPE(185462.693,436911.424,NULL),NULL,NULL)"
  split1 = strsplit(mdsys, "\\(|\\)")[[1]]
  funsplit = function(x) {strsplit(x,",")}
  split2 = sapply(split1,funsplit,simplify = TRUE, USE.NAMES = FALSE)
  output = split2[[3]][1:length(split2[[3]])-1]
  Return(output)
}