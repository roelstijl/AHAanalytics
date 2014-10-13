####dit stuk wordt werkwerkt in AHA_Import
 setwd("N:/Multivariate Analyse/Asset Health Analytics/0. Ongebruikte en brondata/11. Nettopologie")
 library(data.table)
 #data<-read.csv("aansluitingen_stationinclbehuizing.csv",header=TRUE)
 data<-data.frame(fread("aansluitingen_stationinclbehuizing.csv",header=TRUE))
 data2<-read.csv("aansluitingengeotrace.csv",header=TRUE,colClasses=c("character"))
 datacar<-read.csv("N:/Multivariate Analyse/Asset Health Analytics/0. Ongebruikte en brondata/8. CAR/CAR_D_KRM_AANSLUITING/export/MY_SCHEMA/TA/TAB1/data",sep="," ,header=FALSE)
 profielendata=read.csv("H:/Documents/Probabilistisch Netwerk Model/E1A_stat/profielen Elektriciteit 2014 versie R.csv")
  
 colnames(data2)[2]<-"ID"
 colnames(data2)[3]<-"STATION_ID"
 colnames(data2)[9]<-"STANDAARD_JAARVERBRUIK"
 
 colnames(profielendata)[3:12]<-c("E1a","E1b","E1c","E2a","E2b","E3a","E3b","E3c","E3d","E4a")
 colnames(datacar)

####selecteer unieke EAN's

#data$COUNTEAN<-ave(data$EAN,data$EAN,FUN=length)
tablecount<-data.frame(table(data$EAN))               
data3<-merge(data[-which(data$EAN==""),], tablecount, by.x="EAN", by.y="Var1")
data3<-data3[which(data3$Freq==1),]
data3<-data3[colnames(data2)]
dataean<-rbind(data2,data3)
save(dataexport, file="N:/Multivariate Analyse/Asset Health Analytics/1. Ruwe Datasets/11. Nettopologie/EANtabel.Rdata")

View(data2[1:100,])

## Koppelen SJV's
library(base)
dataean<-load("N:/Multivariate Analyse/Asset Health Analytics/1. Ruwe Datasets/11. Nettopologie/EANtabel.Rdata")
datalshld<-data.frame(table(dataean$LS_HLD))
datalshld[c("gem","max","tweeuur","kwad","diff")]<-NA
 
shift <- function(d, k) rbind( tail(d,k), head(d,-k), deparse.level = 0 )
 for(i in 2:100){
   tic()
   eans<-dataexport$EAN[which(dataexport$LS_HLD==datalshld$Var1[i])]
   sjv<-datacar[which(datacar[,2] %in% eans),5]+datacar[which(datacar[,2] %in% eans),6]
   profiel<-datacar[which(datacar[,2] %in% eans),4]
   ll<-length(sjv)
   belasting=0*c(1:35040)
   if(!is.na(sjv[1])){
    for(j in 1:ll){
     belasting=4*(belasting+sjv[j]*profielendata[,which(colnames(profielendata)==profiel[j])])
    }
   }
   datalshld$gem[i]<-mean(belasting)
   datalshld$max[i]<-max(belasting)
    belastingshift<-data.frame(belasting)
    for(k in 1:7){
     belastingshift[,k+1]<-shift(belastingshift,k)$belasting
    }
   datalshld$tweeuur[i]<-max(apply(belastingshift,1,min))
   datalshld$kwad[i]<-mean(belasting**2)
   datalshld$diff[i]<-mean((belastingshift[,1]-belastingshift[,2])**2)
   toc()
 }

