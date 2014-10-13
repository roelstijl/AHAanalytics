load("C:/Data/AHAdata/2. Input Datasets/AHA_Proxy_partial_data.Rda")


# Oude proxy LS Moffen --------------------------------------------------

#KLAK_LS<-KLAK_LS[which(KLAK_LS$Assetgroep=="Kabels. lijnen & garnituren"),]

###aanmaken tabel met moffen + bijbehorende KLAK-melding
moffen[,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]<-NA
moffenklak<-moffen[0,]
for(i in 1:nrow(KLAK_MS)){
 test<-KLAK_LS[i,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]
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
klaktabel[,c("storing")]<-sapply(klaktabel$countadded, function (x) if(x>0){1}else{0})*sapply(klaktabel$countremoved, function (x) if(x>0 & x<6){1}else{0})

colnames(klaktabel)[1]<-"ID_KLAK_Melding"
klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-NA

tabel<-sapply(klaktabel$ID_KLAK_Melding,function(x) moffenklak$ID_Knooppunt[which(moffenklak$ID_KLAK_Melding==x)])
tabel<-t(data.frame(lapply(tabel,function(x)x[1:5])))

klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-tabel
klaktabel<-klaktabel[which(klaktabel$storing==1),]

klaktabelmoffenLS<-klaktabel

# Oude proxy MS Moffen ----------------------------------------------------

#KLAK_MS<-KLAK_MS[which(KLAK_LS$Assetgroep=="Kabels. lijnen & garnituren"),]

###aanmaken tabel met moffen + bijbehorende KLAK-melding
moffen[,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]<-NA
moffenklak<-moffen[0,]
ll<-0
for(i in 1:nrow(KLAK_MS)){
  test<-KLAK_MS[i,c("ID_KLAK_Melding","Tijdstip_begin","PC_6")]
  moffenklakadd<-moffen[which(moffen$PC_XY==test$PC_6),]
  countremoved<-sum(moffenklakadd$DateRemoved != "")
  countadded<-sum(moffenklakadd$DateAdded != "")
  if(countremoved>0 & countadded>0){
    moffenklakadd[,c("ID_KLAK_Melding","Tijdstip_begin","PC_6")]<-test
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
colnames(klaktabel)[1]<-"ID_KLAK_Melding"
klaktabel[,c("countadded")]<-sapply(klaktabel$ID_KLAK_Melding,function(x) sum(moffenklak$Adiffc[which(moffenklak$ID_KLAK_Melding==x)]))     #aantal toegevoegde moffen
klaktabel[,c("countremoved")]<-sapply(klaktabel$ID_KLAK_Melding,function(x) sum(moffenklak$Rdiffc[which(moffenklak$ID_KLAK_Melding==x)]))   #aantal weggehaalde moffen
#max(klaktabel$countremoved)
klaktabel[,c("storing")]<-sapply(klaktabel$countadded, function (x) if(x>0){1}else{0})*sapply(klaktabel$countremoved, function (x) if(x>0 & x<6){1}else{0})


klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-NA

tabel<-sapply(klaktabel$ID_KLAK_Melding, function(x) moffenklak$ID_Bron[which(moffenklak$ID_KLAK_Melding==x)])
tabel<-t(data.frame(lapply(tabel,function(x)x[1:5])))

klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-tabel
klaktabel<-klaktabel[which(klaktabel$storing==1),]

klaktabelmoffenMS<-klaktabel
#save(klaktabel,file="gestoordemoffen.Rda")

# Oude proxy LS kabels ----------------------------------------------------
# KLAK_LS<-KLAK_LS[which(KLAK_LS$Assetgroep=="Kabels. lijnen & garnituren"),]

###aanmaken tabel met kabels + bijbehorende KLAK-melding
kabels[,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]<-NA
kabelsklak<-kabels[0,]
ll<-0
for(i in 1:nrow(KLAK_LS)){
  klaklabels<-KLAK_LS[i,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]
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
