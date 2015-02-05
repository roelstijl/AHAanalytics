Belasting=data.table(Belasting)
Totale_Belasting=data.table(Totale_Belasting)
MSR=data.table(MSR)

setkey(Belasting,ID_Behuizing)
setkey(Totale_Belasting,ID_Behuizing)
setnames(MSR,"MSRlist","ID_Behuizing")
setkey(MSR,ID_Behuizing)
MSR[,X:=NULL]

Belasting = MSR[Belasting]
setnames(Totale_Belasting,"ID_Behuizing","MSRn")
Totale_Belasting[,Absolute_Belasting:=abs(Totale_Belasting),by=list(Scenario_nr,Datum,MSRn)]
Belasting[,Absolute_Belasting:=abs(Belasting),by=list(Scenario_nr,Datum,MSRn,Component)]

Totale_Belasting[,X=NULL]
Belasting[,X=NULL]

write.csv(Totale_Belasting,file="Totale_Belasting.csv",row.names=F)
write.csv(Belasting,file="Belasting.csv",row.names=F)
write.csv(MSR,file="MSR",row.names=F)



folder = "F:/1. Alliander/7. Livelab data (Tim)/"
files  = list.files(folder)[c(1:9,23)]
namesdata  = c("Achter de Wiel","Bloemkeshof","De Vergt","Hooimijt","Inktfordseweg","Kloetijn","Mozartstraat","Nieuwe Tijningen","Steenweg","Nieuwe Tijningen min hoofdleidingen")
longitude = 

data        = llply(1:10,function(x)
              {a = data.table(read.csv2(paste0(folder,files[[x]])))
              a[,MSR := namesdata[x]]
              a}
              )
bindeddata = rbindlist(data)

setnames(bindeddata,c("Load_Ber","Load_Liv","Rank.Ber","Ber_Ex", "Rank.Liv", "Liv_Ex"),
         c("Belastings berekening","Belasting gemeten (LiveLab data)","Rank belasting","Percentage tijd aangegeven vermogen belasting",
           "Rank gemeten (LiveLab data)","Percentage tijd aangegeven vermogen gemeten (LiveLab data)"))

write.csv(bindeddata,file="Belastingen gemeter vs berekend Tim.csv",row.names = T)
