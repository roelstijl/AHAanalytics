load("C:/Data/AHAdata/2. Input Datasets/AHA_Proxy_partial_data.Rda")
load("C:/Data/AHAdata/2. Input Datasets/6. NOR/masterdataset_ELCVERBINDINGEN.Rda")
load("C:/Data/AHAdata/2. Input Datasets/6. NOR/NOR_verbindingen_all.Rda")

# Koppelen klak-groepsnummers ---------------------------------------------
KLAK_LS[,c("ID_Groep")]       <-sapply(KLAK_LS$ID_KLAK_Melding,
                                      function(x){if(length(KLAKMELDERS$ID_Groep[which(KLAKMELDERS$MELDING==x)])==0)
                                      {0}
                                      else
                                      {KLAKMELDERS$ID_Groep[which(KLAKMELDERS$MELDING==x)]}})

KLAK_LS                       <-KLAK_LS[!duplicated(KLAK_LS$ID_Groep),]                       #verwijderen dubbel voorkomende klakmeldingen

KLAK_MS[,c("ID_Groep")]       <-sapply(KLAK_MS$ID_KLAK_Melding,
                                      function(x){if(length(KLAKMELDERS$ID_Groep[which(KLAKMELDERS$MELDING==x)])==0)
                                      {0}
                                      else
                                      {KLAKMELDERS$ID_Groep[which(KLAKMELDERS$MELDING==x)]}})

KLAK_MS                       <-KLAK_MS[!duplicated(KLAK_MS$ID_Groep),]                       #verwijderen dubbel voorkomende klakmeldingen

# Koppelen EAN aan KLAKMELDER -------------------------------------------
KLAKMELDERS                                            <-  merge(KLAKMELDERS,CARXYPC,by.x=c("PC6","Huisnr"),by.y=c("PC_6","Huisnr"),all.x=TRUE)
names(KLAKMELDERS2)[names(KLAKMELDERS) == "ID_EAN.x"]  <-  "ID_EAN"
KLAKMELDERS                                            <-  KLAKMELDERS[,c("MELDING","ID_Groep","ST_Groep_eerste","PC6","Huisnr","ID_EAN","Straat","Klacht","SubKlacht")]
#sum(ifelse(is.na(KLAKMELDERS2$ID_EAN),1,0))
#table(table(KLAKMELDERS2$MELDING))

# proxy LS kabels via HLD ----------------------------------------------------

klaktabel<-KLAK_LS[c("ID_KLAK_Melding", "Component","Tijdstip_begin_storing", "PC_6", "ID_Groep")]    #aanmaken tabel met klakmeldingen
kabelsklak<-kabels[0,]                                                                        #aanmaken tabel met kabels + bijbehorende KLAK-melding
ll<-0                                                                                         #teller om # gekoppelde storingen bij te houden
i=2
for(i in 1:1000){
  klakmelders<-KLAKMELDERS[which(KLAKMELDERS$ID_Groep==klaktabel$ID_Groep[i]),]              #Koppel alle klakmelders aan melding, time=0.08
         #system.time(EANS<- CARXYPC[which(CARXYPC$PC_6 %in% klakmelders$PC6 & CARXYPC$Huisnr %in% klakmelders$Huisnr),]) #Koppel alle EANS's aan de klakmelders, time=0.35
  system.time(HFD<-EANtoHFD[which(EANtoHFD$ID_EAN %in% klakmelders$ID_EAN),])                              #Koppel alle LS hoofdleidingen aan de ean's van de melders, time=0.10
         #VBD<-masterdataset[which(masterdataset$ID_Verbinding %in% HFD$ID_Hoofdleiding_LS),]        #Koppel overige verbindingsID's
  kabelsklakadd<-kabels[which(kabels$ID_Verbinding %in% HFD$ID_Hoofdleiding_LS),]             #aanmaken deeltabel voor kabels gekoppeld aan storing, time=0.01
  countremoved<-sum(kabelsklakadd$DateRemoved != "")                                          #Teller hoeveel kabels zijn verwijderd
    if(countremoved>0){                                                                       #neem assets mee indien in dezelfde hoofdleiding assets verwijderd zijn
    kabelsklakadd[,c("ID_KLAK_Melding","Component","Tijdstip_begin_storing","PC_6")]<-klaktabel[i,c("ID_KLAK_Melding","Component","Tijdstip_begin_storing","PC_6")]
    kabelsklak<-rbind(kabelsklak,kabelsklakadd)
    ll<-ll+1}
  if(i %% 10 ==0){print(paste(i,ll))}
}

library(foreach)
library(parallel)
library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)

kabelsklakadd<-kabels[0,] 
kabelsklak<-foreach(i in 1:5, .combine="rbind") %dorpar%{
  #klakmelders<-KLAKMELDERS[which(KLAKMELDERS$ID_Groep==klaktabel$ID_Groep[i]),]              #Koppel alle klakmelders aan melding, time=0.08
  #HFD<-EANtoHFD[which(EANtoHFD$ID_EAN %in% klakmelders$ID_EAN),]                              #Koppel alle LS hoofdleidingen aan de ean's van de melders, time=0.10
  #kabelsklakadd<-kabels[which(kabels$ID_Verbinding %in% HFD$ID_Hoofdleiding_LS),]             #aanmaken deeltabel voor kabels gekoppeld aan storing, time=0.01
  kabelsklakadd<-kabels[i,]
  countremoved<-sum(kabelsklakadd$DateRemoved != "")                                          #Teller hoeveel kabels zijn verwijderd
  ifelse(countremoved>0,                                                                     #neem assets mee indien in dezelfde hoofdleiding assets verwijderd zijn
    kabelsklakadd[,c("ID_KLAK_Melding","Component","Tijdstip_begin_storing","PC_6")]<-klaktabel[i,c("ID_KLAK_Melding","Component","Tijdstip_begin_storing","PC_6")],
    kabelsklakadd[,c("ID_KLAK_Melding","Component","Tijdstip_begin_storing","PC_6")]<-c("NA","NA","NA"))
}




###voeg datumverschillen toe
kabelsklak[,c("Adiff")]<-as.Date(paste0(kabelsklak$DateAdded,"04"),format="%y%m%d")-as.Date(kabelsklak$Tijdstip_begin_storing,format="%d-%m-%Y")
kabelsklak[,c("Rdiff")]<-as.Date(paste0(kabelsklak$DateRemoved,"04"),format="%y%m%d")-as.Date(kabelsklak$Tijdstip_begin_storing,format="%d-%m-%Y")
checkverschil<-function(dagen,nmin,nmax){if(!is.na(dagen)){if(dagen <= nmax & dagen >=nmin){1}else{0} }else{0}}
kabelsklak[,c("Adiffc")]<-sapply(kabelsklak$Adiff,function(x) checkverschil(x,-30,70))                     #Kan asset verwijderd zijn door storing?
kabelsklak[,c("Rdiffc")]<-sapply(kabelsklak$Rdiff,function(x) checkverschil(x,-30,70))                     #Kan asset toegevoegd zijn door storing?

### Maak dataframe met mogelijk gevonden klakstoringen
klaktabel<-data.frame(table(kabelsklak$ID_KLAK_Melding))
klaktabel[,c("countadded")]<-sapply(klaktabel$Var1,function(x) sum(kabelsklak$Adiffc[which(kabelsklak$ID_KLAK_Melding==x)]))     #aantal toegevoegde moffen
klaktabel[,c("countremoved")]<-sapply(klaktabel$Var1,function(x) sum(kabelsklak$Rdiffc[which(kabelsklak$ID_KLAK_Melding==x)]))   #aantal weggehaalde moffen
#max(klaktabel$countremoved)
klaktabel[,c("storing")]<-sapply(klaktabel$countadded, function (x) if(x>0){1}else{0})*sapply(klaktabel$countremoved, function (x) if(x>0 & x<6){1}else{0})

colnames(klaktabel)[1]<-"ID_KLAK_Melding"
klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-NA

tabel<-sapply(klaktabel$ID_KLAK_Melding,function(x) kabelsklak$ID_Bron[which(kabelsklak$ID_KLAK_Melding==x)])
tabel<-t(data.frame(lapply(tabel,function(x) x[1:5])))

klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-tabel
klaktabel<-klaktabel[which(klaktabel$storing==1),]

klaktabelkabelsLS<-klaktabel

# Oude proxy MS kabels ----------------------------------------------------
# KLAK_MS<-KLAK_MS[which(KLAK_LS$Assetgroep=="Kabels. lijnen & garnituren"),]

###aanmaken tabel met kabels + bijbehorende KLAK-melding
kabels[,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]<-NA
kabelsklak<-kabels[0,]
ll<-0
for(i in 1:nrow(KLAK_MS)){
  klaklabels<-KLAK_MS[i,c("ID_KLAK_Melding","Tijdstip_begin","PC_6")]
  kabelsklakadd<-kabels[which(kabels$PC_XY_van==klaklabels$PC_6),]
  countremoved<-sum(kabelsklakadd$DateRemoved != "")
  countadded<-sum(kabelsklakadd$DateAdded != "")
  if(countremoved>0 & countadded>0){
    kabelsklakadd[,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]<-klaklabels
    kabelsklak<-rbind(kabelsklak,kabelsklakadd)
    ll<-ll+1}
}

###voeg datumverschillen toe
kabelsklak[,c("Adiff")]<-as.Date(paste0(kabelsklak$DateAdded,"04"),format="%y%m%d")-as.Date(kabelsklak$Tijdstip_begin_storing,format="%d-%m-%Y")
kabelsklak[,c("Rdiff")]<-as.Date(paste0(kabelsklak$DateRemoved,"04"),format="%y%m%d")-as.Date(kabelsklak$Tijdstip_begin_storing,format="%d-%m-%Y")
checkverschil<-function(dagen,nmin,nmax){if(!is.na(dagen)){if(dagen <= nmax & dagen >=nmin){1}else{0} }else{0}}
kabelsklak[,c("Adiffc")]<-sapply(kabelsklak$Adiff,function(x) checkverschil(x,-30,70))                     #Kan asset verwijderd zijn door storing?
kabelsklak[,c("Rdiffc")]<-sapply(kabelsklak$Rdiff,function(x) checkverschil(x,-30,70))                     #Kan asset toegevoegd zijn door storing?

### Maak dataframe met mogelijk gevonden klakstoringen
klaktabel<-data.frame(table(kabelsklak$ID_KLAK_Melding))
klaktabel[,c("countadded")]<-sapply(klaktabel$Var1,function(x) sum(kabelsklak$Adiffc[which(kabelsklak$ID_KLAK_Melding==x)]))     #aantal toegevoegde moffen
klaktabel[,c("countremoved")]<-sapply(klaktabel$Var1,function(x) sum(kabelsklak$Rdiffc[which(kabelsklak$ID_KLAK_Melding==x)]))   #aantal weggehaalde moffen
#max(klaktabel$countremoved)
klaktabel[,c("storing")]<-sapply(klaktabel$countadded, function (x) if(x>0){1}else{0})*sapply(klaktabel$countremoved, function (x) if(x>0 & x<6){1}else{0})

colnames(klaktabel)[1]<-"ID_KLAK_Melding"
klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-NA

tabel<-sapply(klaktabel$ID_KLAK_Melding,function(x) kabelsklak$ID_Bron[which(kabelsklak$ID_KLAK_Melding==x)])
tabel<-t(data.frame(lapply(tabel,function(x) x[1:5])))

klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-tabel
klaktabel<-klaktabel[which(klaktabel$storing==1),]

klaktabelkabelsMS<-klaktabel

# Overig ------------------------------------------------------------------


if(klaktabel[6,c("countadded")]>0 & klaktabel[6,c("countremoved")]>0 & klaktabel[6,c("countremoved")]<6){1}else{0}

moffenklak$ID_Knooppunt[which(moffenklak$ID_KLAK_Melding==klaktabel$Var1[2])]

png("plotverwijderdemoffen.png")
barplot(table(moffenklak$DateRemoved)[c(-1)])
dev.off()

sum(table(moffenklak$DateRemoved)[c(-1)])

for(i in 1:nrow(KLAK_LS)){
  test=KLAK_LS[i,c("Tijdstip_begin_storing","Postcode_6")]
  if(nrow(kabels[which(kabels$PC_XY_van==test$Postcode_6),])>0){ll<-ll+1}}


### Meerdere KLAK-melders
KLAKMELDERS<-KLAKMELDERS[-which(KLAKMELDERS$),]
tablecount<-data.frame(table(KLAKMELDERS$ID_Groep))               
data3<-merge(KLAKMELDERS, tablecount, by.x="ID_Groep", by.y="Var1")

addfreq<-function(dataframe,kolomnaam){
  tablecount<-data.frame(table(dataframe[kolomnaam]))
  for(i in 1:nrow(dataframe)){
    if(is.na(dataframe[i,"kolomnaam"]))
    {dataframe[i,"Freq"]<-0}
    else
    {dataframe[i,"Freq"]<-tablecount[which(tablecount[,1]==dataframe[i,"kolomnaam"]),2]}
  }
}

addfreq(KLAKMELDERS, "ID_Groep")

for(i in 1:nrow(KLAKMELDERS)){
  if(is.na(KLAKMELDERS[i,"ID_Groep"]))
   {KLAKMELDERS[i,"Freq"]<-0}
   else
   {KLAKMELDERS[i,"Freq"]<-tablecount[which(tablecount[,1]==KLAKMELDERS[i,"ID_Groep"]),2]}
 }

View(CARXYPC)


# Koppelen alle Klakmelders --------------------------------------------------------

###aanmaken tabel met moffen + bijbehorende KLAK-melding
moffenklak<-moffen[0,]
moffenklak[,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]<-NA
for(i in 1:nrow(KLAK_LS)){
  klakextract<-KLAK_LS[i,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]
  groepsid<-KLAKMELDERS$ID_groep[which(KLAKMELDERS$MELDING==klakextract$ID_KLAK_melding)]
  if(!is.na()){}
  moffenklakadd<-moffen[which(moffen$PC_XY==test$PC_6),]
  countremoved<-sum(moffenklakadd$DateRemoved != "")
  countadded<-sum(moffenklakadd$DateAdded != "")
  if(countremoved>0 & countadded>0){
    moffenklakadd[,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]<-test
    moffenklak<-rbind(moffenklak,moffenklakadd)
    ll<-ll+1}
}

###voeg datumverschillen toe
moffenklak[,c("Adiff")]<-as.Date(paste0(moffenklak$DateAdded,"04"),format="%y%m%d")-as.Date(moffenklak$Tijdstip_begin_storing,format="%d-%m-%Y")
moffenklak[,c("Rdiff")]<-as.Date(paste0(moffenklak$DateRemoved,"04"),format="%y%m%d")-as.Date(moffenklak$Tijdstip_begin_storing,format="%d-%m-%Y")
checkverschil<-function(dagen,nmin,nmax){if(!is.na(dagen)){if(dagen <= nmax & dagen >=nmin){1}else{0} }else{0}}
moffenklak[,c("Adiffc")]<-sapply(moffenklak$Adiff,function(x) checkverschil(x,-30,70))                     #Kan asset verwijderd zijn door storing?
moffenklak[,c("Rdiffc")]<-sapply(moffenklak$Rdiff,function(x) checkverschil(x,-30,70))                     #Kan asset toegevoegd zijn door storing?

### Maak dataframe met mogelijk gevonden klakstoringen
klaktabel<-data.frame(table(moffenklak$ID_KLAK_Melding))
klaktabel[,c("countadded")]<-sapply(klaktabel$Var1,function(x) sum(moffenklak$Adiffc[which(moffenklak$ID_KLAK_Melding==x)]))     #aantal toegevoegde moffen
klaktabel[,c("countremoved")]<-sapply(klaktabel$Var1,function(x) sum(moffenklak$Rdiffc[which(moffenklak$ID_KLAK_Melding==x)]))   #aantal weggehaalde moffen
#max(klaktabel$countremoved)
klaktabel[,c("storing")]<-sapply(klaktabel$countadded, function (x) if(x>0){1}else{0})*sapply(klaktabel$countremoved, function (x) if(x>0){1}else{0})

colnames(klaktabel)[1]<-"ID_KLAK_Melding"
klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-NA

tabel<-sapply(klaktabel$ID_KLAK_Melding,function(x) moffenklak$ID_Knooppunt[which(moffenklak$ID_KLAK_Melding==x)])
tabel<-t(data.frame(lapply(tabel,function(x)x[1:5])))

klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-tabel
klaktabel<-klaktabel[which(klaktabel$storing==1),]

klaktabelmoffenLS<-klaktabel
