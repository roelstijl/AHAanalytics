AHA_Proxy_BL_Nettopologie = function()
{
  ####dit stuk wordt werkwerkt in AHA_Import
  # setwd("N:/Multivariate Analyse/Asset Health Analytics/0. Ongebruikte en brondata/11. Nettopologie")
  # library(data.table)
  # #data<-read.csv("aansluitingen_stationinclbehuizing.csv",header=TRUE)
  # data<-data.frame(fread("aansluitingen_stationinclbehuizing.csv",header=TRUE))
  # data2<-read.csv("aansluitingengeotrace.csv",header=TRUE,colClasses=c("character"))
  # datacar<-read.csv("N:/Multivariate Analyse/Asset Health Analytics/0. Ongebruikte en brondata/8. CAR/CAR_D_KRM_AANSLUITING/export/MY_SCHEMA/TA/TAB1/data",sep="," ,header=FALSE)
  profielendata=read.csv("H:/Documents/Probabilistisch Netwerk Model/E1A_stat/profielen Elektriciteit 2014 versie R.csv")
  #  
  # colnames(data2)[2]<-"ID"
  # colnames(data2)[3]<-"STATION_ID"
  # colnames(data2)[9]<-"STANDAARD_JAARVERBRUIK"
  # 
  # colnames(profielendata)[3:12]<-c("E1a","E1b","E1c","E2a","E2b","E3a","E3b","E3c","E3d","E4a")
  # colnames(datacar)
  
  ####selecteer unieke EAN's
  
  # #data$COUNTEAN<-ave(data$EAN,data$EAN,FUN=length)
  # tablecount<-data.frame(table(data$EAN))               
  # data3<-merge(data[-which(data$EAN==""),], tablecount, by.x="EAN", by.y="Var1")
  # data3<-data3[which(data3$Freq==1),]
  # data3<-data3[colnames(data2)]
  # dataean<-rbind(data2,data3)
  # save(dataexport, file="N:/Multivariate Analyse/Asset Health Analytics/1. Ruwe Datasets/11. Nettopologie/EANtabel.Rdata")
  
  # View(data2[1:100,])
  
  ## Koppelen SJV's en doorrekenen belasting
  library(base)
  load("N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets/11. Nettopologie/EANtabel.Rdata")
  datalshld<-data.frame(table(dataean$LS_HLD))                               #basisset voor hoofdleidingen
  datalshld[c("gem","max","tweeuur","kwad","diff")]<-NA                      #toevoegen extra kolomnamen
  
  shift <- function(d, k) rbind( tail(d,k), head(d,-k), deparse.level = 0 )  #functie om belastingdata 1 rij om te schuiven om minimum over 2 uur te berekenen
  for(i in 2:nrow(datalshld)){
    eans <- dataexport$EAN[which(dataexport$LS_HLD==datalshld$Var1[i])]     #eans behorend bij LS hoofdleiding
    sjv <- datacar[which(datacar[,2] %in% eans),5]+datacar[which(datacar[,2] %in% eans),6] #sjv's behorend bij eans tel normaal en laagtarfief bij elkaar op
    sjv[is.na(sjv)]<-0                                                      #als sjv niet bekend is, vul 0 in
    profiel <- datacar[which(datacar[,2] %in% eans),4]                      #profiel behorend bij ean
    profiel[which(profiel=="<Missing Value>")]<-"E1a"                       #als profiel niet bekend is, vul E1a in
    ll<-length(sjv)
    belasting=0*c(1:35040)
    for(j in 1:ll){                                                        #Optellen belasting van verschillende eans, de factor 4 komt van de verschuiving van kWh naar kVA
      belasting<-(belasting+4*sjv[j]*profielendata[,which(colnames(profielendata)==profiel[j])])
    }
    datalshld$gem[i]<-mean(belasting)                                       #gemiddelde van de belasting
    datalshld$max[i]<-max(belasting)                                        #maximum van de belasting
    belastingshift<-data.frame(belasting)                                  
    for(k in 1:7){
      belastingshift[,k+1]<-shift(belastingshift,k)$belasting
    }
    datalshld$tweeuur[i]<-max(apply(belastingshift,1,min))                  #maximum van de 2uursduurbelasting
    datalshld$kwad[i]<-mean(belasting**2)                                   #gemiddelde van het kwadraat van de belasting
    datalshld$diff[i]<-mean((belastingshift[,1]-belastingshift[,2])**2)     #gemiddelde van het kwadraat van de tijdsafgeleide van de belasting
    if(i %% 10 == 0){print(i)}
  }
  
  
}
