AHA_Proxy_Dataset = function(initialdate="1401",months=10)
  {
  # Loads several month of AHA datasets
  
  # Settings ----------------------------------------------------------------  

  dates = as.Date(paste0(initialdate,"01"), "%y%m%d")
  month(dates) = month(dates)+(1:months)-1
  maanden = format(dates, format="%y%m")
  
  # Load the data ----------------------------------------------------------------
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/6. NOR/masterset_ELCVERBINDINGSKNOOPPUNTEN.Rda")
  moffen = masterset[pmatch(masterset$DateAdded, maanden, dup = TRUE,nomatch=0)|pmatch(masterset$DateRemoved, maanden, dup = TRUE,nomatch=0)>0,]
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/6. NOR/masterset_ELCVERBINDINGSDELEN.Rda")
  kabels = masterset[pmatch(masterset$DateAdded, maanden, dup = TRUE,nomatch=0)|pmatch(masterset$DateRemoved, maanden, dup = TRUE,nomatch=0)>0,]
  load("C:/Datasets/AHAdata/1. Ruwe Datasets/6. NOR/masterdataset_ELCVERBINDINGSDELEN.Rda")
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
  
  # Save the data ----------------------------------------------------------------
  
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

convert_SDO_GEOMETRY = function(mdsys){
  mdsys = "MDSYS.SDO_GEOMETRY(2001,28992,MDSYS.SDO_POINT_TYPE(185462.693,436911.424,NULL),NULL,NULL)"
  split1 = strsplit(mdsys, "\\(|\\)")[[1]]
  funsplit = function(x) {strsplit(x,",")}
  split2 = sapply(split1,funsplit,simplify = TRUE, USE.NAMES = FALSE)
  output = split2[[3]][1:length(split2[[3]])-1]
  Return(output)
}

AHA_Proxy_Dataset_post = function(){
  setwd("N:/Multivariate Analyse/AHAdata/2. Input Datasets")
  load("AHA_Proxy_partial_data.Rda")
  load("6. NOR/masterdataset_ELCVERBINDINGEN.Rda")
  verbindingen   <-   masterdataset
  load("6. NOR/masterdataset_ELCVERBINDINGSKNOOPPUNTEN.Rda")
  moffen         <-   masterdataset
  load("6. NOR/masterdataset_ELCVERBINDINGSDELEN.Rda")
  kabels         <-   masterdataset
  
  countsv<-table(verbindingen$DateAdded,verbindingen$DateRemoved)
  barplot(countsv,col=c("darkblue","red"),legend = rownames(countsv), beside=TRUE,ylim=c(0,5000))
  
  countsk<-table(kabels$DateAdded)
  countsk2<-table(kabels$DateRemoved)
  countsk1<-cbind(countsk,countsk2)
  png("plotkabelsNOR.png", width=1500, height=600)
  barplot(t(countsk1),log="y",col=c("darkblue","red"),legend=c("Added","Removed"), beside=TRUE,ylim=c(100,5000000))
  legend("bottomright",c("Added","Removed"),fill=c("darkblue","red"),bg="white")
  dev.off()
  
  
  countsm1<-table(moffen$DateAdded)
  countsm2<-table(moffen$DateRemoved)
  countsm<-cbind(countsm1,countsm2)
  png("plotmoffenNOR.png", width=2000, height=600)
  barplot(t(countsm),log="y",col=c("darkblue","red"),legend=c("Added","Removed"), beside=TRUE,ylim=c(100,5000000))
  legend("bottomright",c("Added","Removed"),fill=c("darkblue","red"),bg="white")
  dev.off()
  
  maanden <- c("1301","1302","1303","1304","1305","1306","1307","1308","1309","1310","1311","1312")
  moffen2 <- moffen[which(moffen$DateAdded %in% maanden | moffen$DateRemoved %in% maanden),]
  kabels2 <- kabels[which(kabels$DateAdded %in% maanden | kabels$DateRemoved %in% maanden),]
  
  
  countsk<-table(kabels2$DateAdded)
  countsk2<-table(kabels2$DateRemoved)
  countsk1<-cbind(countsk[maanden],countsk2[maanden])
  png("plotkabels2NOR.png", width=800, height=400)
  barplot(t(countsk1[maanden,]),col=c("darkblue","red"),legend=c("Added","Removed"), beside=TRUE,ylim=c(0,20000))
  dev.off()
  
  
  countsm1<-table(moffen2$DateAdded)
  countsm2<-table(moffen2$DateRemoved)
  countsm<-cbind(countsm1[maanden],countsm2[maanden])
  png("plotmoffen2NOR.png", width=800, height=400)
  barplot(t(countsm[maanden,]),col=c("darkblue","red"),legend=c("Added","Removed"), beside=TRUE,ylim=c(0,20000))
  dev.off()
  
  kabels<-kabels2
  moffen<-moffen2
  
  #functies van Roel
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
  maanden <- c("1110","1111","1112","1201","1202","1203","1204","1205","1206","1207","1208","1209","1210","1211","1212","1301")
  #Laden klak-data en koppelen aan klakmelders
  load("N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_LS.Rda")
  mindataset$Maand = sapply(mindataset$Maand,fixdates)
  KLAK_LS= mindataset[pmatch(mindataset$Maand, maanden, dup = TRUE,nomatch=0)>0,]
  KLAK_LS = data.frame(KLAK_LS)
  KLAK_LS$Coo_X=as.numeric(sapply(KLAK_LS$Coo_X,fixnumber))
  KLAK_LS$Coo_Y=as.numeric(sapply(KLAK_LS$Coo_Y,fixnumber))
  
  load("N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_MS.Rda")
  mindataset$Maand = sapply(mindataset$Maand,fixdates) 
  KLAK_MS= mindataset[pmatch(mindataset$Maand, maanden, dup = TRUE,nomatch=0)>0,]
  KLAK_MS = data.frame(KLAK_MS)
  KLAK_MS$Coo_X=as.numeric(sapply(KLAK_MS$Coo_X,fixnumber))
  KLAK_MS$Coo_Y=as.numeric(sapply(KLAK_MS$Coo_Y,fixnumber))
  
  load("N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_KOPPEL_MELDING_GROEP.Rda")
  KLAKMELDERS = mindataset;
  
  # Combine KLAK data
  nmelders = table(KLAKMELDERS$ID_Groep)
  ugroep   = KLAKMELDERS[KLAKMELDERS$ST_Groep_eerste=="Ja",]
  frequ    = table(KLAKMELDERS[,2])
  ugroep  = merge(x=ugroep,y=frequ,by.x="ID_Groep",by.y="Var1",all.x=TRUE)
  colnames(ugroep)[c(1,2,9)]=c("ID_KLAK_Melding","ID_Groep","Aantal_Melders")
  
  KLAK_LS=merge(x = KLAK_LS, y = ugroep[,c("ID_KLAK_Melding","Aantal_Melders","ID_Groep")], by = "ID_KLAK_Melding", all.x=TRUE)
  KLAK_MS=merge(x = KLAK_MS, y = ugroep[,c("ID_KLAK_Melding","Aantal_Melders","ID_Groep")], by = "ID_KLAK_Melding", all.x=TRUE)
  
  save(moffen,kabels,EANtoHFD,CARXYPC,KLAK_COMP,verbindingen,KLAK_LS,KLAK_MS,KLAKMELDERS,file="AHA_Proxy_partial_dataN.Rda")
  
  }
  
  