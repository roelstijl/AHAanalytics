load("C:/Data/AHAdata/2. Input Datasets/AHA_Proxy_partial_data.Rda")

# Oude proxy LS Moffen --------------------------------------------------

#KLAK_LS<-KLAK_LS[which(KLAK_LS$Assetgroep=="Kabels. lijnen & garnituren"),]

###aanmaken tabel met moffen + bijbehorende KLAK-melding

#moffenklak[,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]<-0
proxyLSmoffen<- function(){
moffenklak<-moffen[0,]
for(i in 1:nrow(KLAK_LS)){
 klakextract<-KLAK_LS[i,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]
 postcodelijst<-c(klakextract$PC_6,KLAKMELDERS$PC6[which(KLAKMELDERS$ID_Groep==KLAK_LS$ID_Groep[i])])
 moffenklakadd<-moffen[which(moffen$PC_XY %in% postcodelijst),]
 countremoved<-sum(moffenklakadd$DateRemoved != "")
 countadded<-sum(moffenklakadd$DateAdded != "")
 #print(length(unique(postcodelijst)))
 if(countremoved>0 & countadded>0){
  moffenklakadd[,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]<-klakextract
  moffenklak<-rbind(moffenklak,moffenklakadd)}
 }

### Maak dataframe met mogelijk gevonden klakstoringen
klaktabel<-data.frame(table(moffenklak$ID_KLAK_Melding))
colnames(klaktabel)[1]<-"ID_KLAK_Melding"

###voeg datumverschillen toe
moffenklak[,c("Adiff")]<-as.Date(paste0(moffenklak$DateAdded,"04"),format="%y%m%d")-as.Date(moffenklak$Tijdstip_begin_storing,format="%d-%m-%Y")
moffenklak[,c("Rdiff")]<-as.Date(paste0(moffenklak$DateRemoved,"04"),format="%y%m%d")-as.Date(moffenklak$Tijdstip_begin_storing,format="%d-%m-%Y")
checkverschil<-function(dagen,nmin,nmax){if(!is.na(dagen)){if(dagen <= nmax & dagen >=nmin){1}else{0} }else{0}}
moffenklak[,c("Adiffc")]<-sapply(moffenklak$Adiff,function(x) checkverschil(x,-30,70))                     #Kan asset verwijderd zijn door storing?
moffenklak[,c("Rdiffc")]<-sapply(moffenklak$Rdiff,function(x) checkverschil(x,-30,70))                     #Kan asset toegevoegd zijn door storing?
#Bekijk of Asset is vervangen door andere asset
system.time(for(i in 1:nrow(klaktabel)){
    removed<-moffenklak[which(moffenklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & moffenklak$DateRemoved!=""),c("Coo_X","Coo_Y","DateRemoved")]
    added  <-moffenklak[which(moffenklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & moffenklak$DateAdded!=""),c("Coo_X","Coo_Y","DateAdded")]
    dist   <-t(sapply(removed$Coo_X,function(x){(x-added$Coo_X)^2})+sapply(removed$Coo_Y,function(x){(x-added$Coo_Y)^2}))
    tijdsd <-t(sapply(removed$DateRemoved,function(x){as.Date(paste0(x,"04"),format="%y%m%d")-as.Date(paste0(added$DateAdded,"04"),format="%y%m%d")}))
    vervc  <-sapply(apply(floor(dist/4)+floor(abs(tijdsd/45)),1,min),function(x)ifelse(x>0,0,1))
    moffenklak[which(moffenklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & moffenklak$DateRemoved!=""),c("vervc")]<-vervc
     }
    )

### Maak dataframe met mogelijk gevonden klakstoringen
klaktabel[,c("countadded")]<-sapply(klaktabel$ID_KLAK_Melding,function(x) sum(moffenklak$Adiffc[which(moffenklak$ID_KLAK_Melding==x)]))     #aantal toegevoegde moffen
klaktabel[,c("countremoved")]<-sapply(klaktabel$ID_KLAK_Melding,function(x) sum(moffenklak$Rdiffc[which(moffenklak$ID_KLAK_Melding==x)]))   #aantal weggehaalde moffen
#max(klaktabel$countremoved)
klaktabel[,c("storing")]<-sapply(klaktabel$countadded, function(x) ifelse(x>0,1,0))*sapply(klaktabel$countremoved, function (x) ifelse(x>0,1,0))

klaktabel[,c("asset1","asset2","asset3","asset4","asset5","asset6")]<-NA

tabel<-sapply(klaktabel$ID_KLAK_Melding,function(x) moffenklak$ID_Knooppunt[which(moffenklak$ID_KLAK_Melding==x & moffenklak$vervc==1)])
tabel<-t(data.frame(lapply(tabel,function(x)x[1:6])))

klaktabel[,c("asset1","asset2","asset3","asset4","asset5","asset6")]<-tabel

return(klaktabel[which(klaktabel$storing==1 & !is.na(klaktabel$asset1)),])
}

klaktabelmoffenLS<-proxyLSmoffen()

# Oude proxy MS Moffen ----------------------------------------------------

#KLAK_MS<-KLAK_MS[which(KLAK_LS$Assetgroep=="Kabels. lijnen & garnituren"),]

###aanmaken tabel met moffen + bijbehorende KLAK-melding
moffen[,c("ID_KLAK_Melding","Tijdstip_begin_storing","PC_6")]<-NA
moffenklak<-moffen[0,]
ll<-0
i=9
for(i in 1:nrow(KLAK_MS)){
  klakextract<-KLAK_MS[i,c("ID_KLAK_Melding","Tijdstip_begin","PC_6")]
  postcodelijst<-c(klakextract$PC_6,KLAKMELDERS$PC6[which(KLAKMELDERS$ID_Groep==KLAK_MS$ID_Groep[i])])
  moffenklakadd<-moffen[which(moffen$PC_XY %in% postcodelijst),]
  countremoved<-sum(moffenklakadd$DateRemoved != "")
  countadded<-sum(moffenklakadd$DateAdded != "")
  if(countremoved>0 & countadded>0){
    moffenklakadd[,c("ID_KLAK_Melding","Tijdstip_begin","PC_6")]<-klakextract
    moffenklak<-rbind(moffenklak,moffenklakadd)
    ll<-ll+1
    }
}

###voeg datumverschillen toe
moffenklak[,c("Adiff")]<-as.Date(paste0(moffenklak$DateAdded,"04"),format="%y%m%d")-as.Date(moffenklak$Tijdstip_begin,format="%d-%m-%Y")
moffenklak[,c("Rdiff")]<-as.Date(paste0(moffenklak$DateRemoved,"04"),format="%y%m%d")-as.Date(moffenklak$Tijdstip_begin,format="%d-%m-%Y")
checkverschil<-function(dagen,nmin,nmax){if(!is.na(dagen)){if(dagen <= nmax & dagen >=nmin){1}else{0} }else{0}}
moffenklak[,c("Adiffc")]<-sapply(moffenklak$Adiff,function(x) checkverschil(x,-30,70))                     #Kan asset verwijderd zijn door storing?
moffenklak[,c("Rdiffc")]<-sapply(moffenklak$Rdiff,function(x) checkverschil(x,-30,70))                     #Kan asset toegevoegd zijn door storing?
#Bekijk of Asset is vervangen door andere asset
system.time(for(i in 1:nrow(klaktabel)){
  removed<-moffenklak[which(moffenklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & moffenklak$DateRemoved!=""),c("Coo_X","Coo_Y","DateRemoved")]
  added  <-moffenklak[which(moffenklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & moffenklak$DateAdded!=""),c("Coo_X","Coo_Y","DateAdded")]
  dist   <-t(sapply(removed$Coo_X,function(x){(x-added$Coo_X)^2})+sapply(removed$Coo_Y,function(x){(x-added$Coo_Y)^2}))
  tijdsd <-t(sapply(removed$DateRemoved,function(x){as.Date(paste0(x,"04"),format="%y%m%d")-as.Date(paste0(added$DateAdded,"04"),format="%y%m%d")}))
  vervc  <-sapply(apply(floor(dist/4)+floor(abs(tijdsd/45)),1,min),function(x)ifelse(x>0,0,1))
  moffenklak[which(moffenklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & moffenklak$DateRemoved!=""),c("vervc")]<-vervc
}
)

### Maak dataframe met mogelijk gevonden klakstoringen
klaktabel<-data.frame(table(moffenklak$ID_KLAK_Melding))
colnames(klaktabel)[1]<-"ID_KLAK_Melding"
klaktabel[,c("countadded")]<-sapply(klaktabel$ID_KLAK_Melding,function(x) sum(moffenklak$Adiffc[which(moffenklak$ID_KLAK_Melding==x)]))     #aantal toegevoegde moffen
klaktabel[,c("countremoved")]<-sapply(klaktabel$ID_KLAK_Melding,function(x) sum(moffenklak$Rdiffc[which(moffenklak$ID_KLAK_Melding==x)]))   #aantal weggehaalde moffen
#max(klaktabel$countremoved)
klaktabel[,c("storing")]<-sapply(klaktabel$countadded, function (x) if(x>0){1}else{0})*sapply(klaktabel$countremoved, function (x) if(x>0){1}else{0})


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

# Koppelen klak-groepsnummers, aanmaken data.tables ---------------------------------------------
KLAK_LS[,c("ID_Groep")]<-sapply(KLAK_LS$ID_KLAK_Melding,
                                function(x){if(length(KLAKMELDERS$ID_Groep[which(KLAKMELDERS$MELDING==x)])==0)
                                {0}
                                else
                                {KLAKMELDERS$ID_Groep[which(KLAKMELDERS$MELDING==x)]}})

KLAK_MS[,c("ID_Groep")]<-sapply(KLAK_MS$ID_KLAK_Melding,
                                function(x){if(length(KLAKMELDERS$ID_Groep[which(KLAKMELDERS$MELDING==x)])==0)
                                {0}
                                else
                                {KLAKMELDERS$ID_Groep[which(KLAKMELDERS$MELDING==x)]}})
#load("N:/Multivariate Analyse/AHAdata/2. Input Datasets/6. NOR/kabelsn.Rda")  #kabels met spanningsniveaus
library(data.table)
KLAKMELDERS    <-data.table(KLAKMELDERS); setkey(KLAKMELDERS,ID_Groep)
kabels         <-data.table(kabels)     ; setkey(kabels,PC_XY_van)
moffen         <-data.table(moffen)     ; setkey(moffen,PC_XY)
kabels$PC_4_van<-substr(kabels$PC_XY_van,1,4)
moffen$PC_4    <-substr(moffen$PC_XY,1,4)



# Verbeterde proxy LS moffen --------------------------------------------------------

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

# Verbeterde proxy MS Moffen ----------------------------------------------------

#KLAK_MS<-KLAK_MS[which(KLAK_LS$Assetgroep=="Kabels. lijnen & garnituren"),]
setkey(moffen,PC_4)

###aanmaken tabel met moffen + bijbehorende KLAK-melding
moffen[,ID_KLAK_Melding:=NA] ; moffen[,ID_KLAK_Melding:=as.integer(ID_KLAK_Melding)]
moffen[,Component:=NA]       ; moffen[,Component:=as.character(Component)]
moffen[,Tijdstip_begin:=NA]  ; moffen[,Tijdstip_begin:=as.character(Tijdstip_begin)]
moffen[,PC_6:=NA]            ; moffen[,PC_6:=as.character(PC_6)]
moffen[,ID_Groep:=NA]        ; moffen[,ID_Groep:=as.double(ID_Groep)]
moffenklak<-moffen[0,]

klaktabel    <- KLAK_MS[c("ID_KLAK_Melding", "Component","Tijdstip_begin", "PC_6", "ID_Groep")]   #aanmaken tabel met klakmeldingen
ll<-0
for(i in 1:nrow(klaktabel)){
  ifelse(is.na(klaktabel$ID_Groep[i]),
         klakmelders$PC6    <-klaktabel$PC_6[i],                                                  #Indien ID_groep onebekend is, kijk alleen naar KLAK-melding
        {klakmelders        <-KLAKMELDERS   [list(klaktabel$ID_Groep[i]),]                        #Koppel alle klakmelders aan melding
         klakmelders        <-klakmelders   [which(complete.cases(klakmelders$PC6)),]})
  klakmelders$PC4 <-substr        (klakmelders$PC6,1,4)                                           #converteer PC6 naar PC4
  moffenklakadd   <-moffen        [list(unique(klakmelders$PC4)),]
  moffenklakadd   <-moffenklakadd [complete.cases(moffenklakadd$DateRemoved)]
  countremoved    <-ifelse(is.na(sum(moffenklakadd$DateRemoved != "")),0,sum(moffenklakadd$DateRemoved != ""))
  klaktabel$countremoved[i] <-  countremoved                                                         #aantal verwijderde assets registreren
  if(countremoved>0){
    moffenklakadd[,c("ID_KLAK_Melding", "Component","Tijdstip_begin", "PC_6", "ID_Groep"):=klaktabel[i,c("ID_KLAK_Melding", "Component","Tijdstip_begin", "PC_6", "ID_Groep")]]
    moffenklak    <-rbind(moffenklak,moffenklakadd)
    ll<-ll+1
  }
}

###voeg datumverschillen toe
moffenklak[,c("Adiff")]  <- as.Date(paste0(moffenklak$DateAdded,"04"),format="%y%m%d")-as.Date(moffenklak$Tijdstip_begin,format="%d-%m-%Y")
moffenklak[,c("Rdiff")]  <- as.Date(paste0(moffenklak$DateRemoved,"04"),format="%y%m%d")-as.Date(moffenklak$Tijdstip_begin,format="%d-%m-%Y")
checkverschil            <- function(dagen,nmin,nmax){if(!is.na(dagen)){if(dagen <= nmax & dagen >=nmin){1}else{0} }else{0}}
moffenklak[,c("Adiffc")] <- sapply(moffenklak$Adiff,function(x) checkverschil(x,-30,70))                     #Kan asset toegevoegd zijn door storing?
moffenklak[,c("Rdiffc")] <- sapply(moffenklak$Rdiff,function(x) checkverschil(x,-30,70))                     #Kan asset verwijderd zijn door storing?
#Bekijk of Asset is vervangen door andere asset
moffenklak <-data.frame(moffenklak)
for(i in which(klaktabel$countremoved>0)){
  removed<-moffenklak[which(moffenklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & moffenklak$DateRemoved!=""),c("Coo_X","Coo_Y","DateRemoved")]
  added  <-moffenklak[which(moffenklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & moffenklak$DateAdded!=""),c("Coo_X","Coo_Y","DateAdded")]
  dist   <-t(sapply(removed$Coo_X,function(x){(x-added$Coo_X)^2})+sapply(removed$Coo_Y,function(x){(x-added$Coo_Y)^2}))
  tijdsd <-t(sapply(removed$DateRemoved,function(x){as.Date(paste0(x,"04"),format="%y%m%d")-as.Date(paste0(added$DateAdded,"04"),format="%y%m%d")}))
  vervc  <-sapply(apply(floor(dist/4)+floor(abs(tijdsd/45)),1,min),function(x)ifelse(x>0,0,1))
  moffenklak[which(moffenklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & moffenklak$DateRemoved!=""),c("vervc")]<-vervc
}

#Bepaal of spanningsniveau van kabel "MS", "Onbekend"of "Anders" is
#moffenklak$SPNV<-ifelse(moffenklak$Spanningsniveau %in% c("10 kV","10 kv","20kV","3kV","6kV"),"MS",ifelse(moffenklak$Spanningsniveau =="","NB","AN"))
#Selecteer alleen MS moffen
moffenklak <- moffenklak[which(moffenklak$Brontabel=="ms_moffen"),]

### Maak dataframe met mogelijk gevonden klakstoringen
klaktabel[,c("countadded")]   <-sapply(klaktabel$ID_KLAK_Melding,function(x) sum(moffenklak$Adiffc[which(moffenklak$ID_KLAK_Melding==x)]))   #aantal toegevoegde moffen
klaktabel[,c("countremoved")] <-sapply(klaktabel$ID_KLAK_Melding,function(x) sum(moffenklak$Rdiffc[which(moffenklak$ID_KLAK_Melding==x)]))   #aantal weggehaalde moffen
#max(klaktabel$countremoved)
klaktabel[,c("storing")]      <-sapply(klaktabel$countadded, function (x) if(x>0){1}else{0})*sapply(klaktabel$countremoved, function (x) if(x>0){1}else{0})


klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-NA

tabel<-sapply(klaktabel$ID_KLAK_Melding, function(x) moffenklak$ID_Bron[which(moffenklak$ID_KLAK_Melding==x & moffenklak$Rdiffc==1 & moffenklak$verv==1)])
klakteveel<-which(unlist(lapply(tabel,length))>5)
tabel<-tabel[-klakteveel]
Reduce("+",lapply(tabel,length))
max(unlist(lapply(tabel,length)))
tabel<-t(data.frame(lapply(tabel,function(x)x[1:5])))

klaktabel[-klakteveel,c("asset1","asset2","asset3","asset4","asset5")]<-tabel

klaktabelmoffenMS   <-klaktabel[which(klaktabel$storing==1),]

#save(klaktabel,file="gestoordemoffen.Rda")

# Verbeterde proxy MS kabels ----------------------------------------------------
# KLAK_MS<-KLAK_MS[which(KLAK_LS$Assetgroep=="Kabels. lijnen & garnituren"),]

###aanmaken tabel met kabels + bijbehorende KLAK-melding
klaktabel    <- KLAK_MS[c("ID_KLAK_Melding", "Component","Tijdstip_begin_storing", "PC_6", "ID_Groep")]   #aanmaken tabel met klakmeldingen
kabelsklak   <- kabels[0,]
ll<-0
i=1
for(i in 1:nrow(klaktabel))){
  klakmelders     <-KLAKMELDERS   [list(klaktabel$ID_Groep[i]),]                                    #Koppel alle klakmelders aan melding
  klakmelders     <-klakmelders   [which(complete.cases(klakmelders$PC6)),]
  klakmelders$PC4 <-substr(klakmelders$PC6,1,4)
  kabelsklakadd   <-kabels        [list(klakmelders$PC6),]
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