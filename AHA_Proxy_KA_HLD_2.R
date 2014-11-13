# Inladen data
load("C:/Data/AHAdata/2. Input Datasets/AHA_Proxy_partial_data.Rda")
library(data.table)

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

# Koppelen EAN aan KLAKMELDER, aanmaken datatables -------------------------------------------
KLAKMELDERS                                            <-  merge(KLAKMELDERS,CARXYPC,by.x=c("PC6","Huisnr"),by.y=c("PC_6","Huisnr"),all.x=TRUE)
names(KLAKMELDERS)[names(KLAKMELDERS) == "ID_EAN.x"]   <-  "ID_EAN"
KLAKMELDERS                                            <-  data.table(KLAKMELDERS[,c("MELDING","ID_Groep","ST_Groep_eerste","PC6","Huisnr","ID_EAN","Straat","Klacht","SubKlacht")])
setkey(KLAKMELDERS,ID_Groep)                                                    #Instellen van Groepsid als key, om snel te zoeken
EANtoHFD                                               <-  data.table(EANtoHFD)
setkey(EANtoHFD,ID_EAN)                                                         #Instellen van EAN-nummer als key, om snel te zoeken
kabels                                                 <-  data.table(kabels)
setkey(kabels,ID_Verbinding)                                                    #Instellen van EAN-nummer als key, om snel te zoeken

#sum(ifelse(is.na(KLAKMELDERS2$ID_EAN),1,0))
#table(table(KLAKMELDERS2$MELDING))

# proxy LS kabels via HLD ----------------------------------------------------
Rprof(filename="proxyhld.out")
klaktabel    <- KLAK_LS[c("ID_KLAK_Melding", "Component","Tijdstip_begin_storing", "PC_6", "ID_Groep")]    #aanmaken tabel met klakmeldingen
kabelsklak   <- kabels [0,]                                                                                #aanmaken tabel met kabels + bijbehorende KLAK-melding
kabelsklak[,c("ID_KLAK_Melding","Component","Tijdstip_begin_storing","PC_6")]<-c(NA,NA,NA)
ll<-0                                                                                                     #teller om # gekoppelde storingen bij te houden
for(i in 1:nrow(klaktabel)){
  klakmelders     <-KLAKMELDERS   [list(klaktabel$ID_Groep[i]),]                                    #Koppel alle klakmelders aan melding
  klakmelders     <-klakmelders   [complete.cases(klakmelders$ID_EAN),]
  HFD             <-EANtoHFD      [list(unique(klakmelders$ID_EAN)),]               #Koppel alle LS hoofdleidingen aan de ean's van de melders
         #VBD     <-masterdataset [which(masterdataset$ID_Verbinding %in% HFD$ID_Hoofdleiding_LS),] #Koppel overige verbindingsID's
  kabelsklakadd   <-kabels        [list(as.character(unique(HFD$ID_Hoofdleiding_LS))),]               #aanmaken deeltabel voor kabels gekoppeld aan storing
  kabelsklakadd   <-kabelsklakadd [complete.cases(kabelsklakadd$ID_Bron)]
  countremoved    <-sum(kabelsklakadd$DateRemoved != "")                                            #Teller hoeveel kabels zijn verwijderd
    if(countremoved>0){                                                                             #neem assets mee indien in dezelfde hoofdleiding assets verwijderd zijn
    kabelsklakadd[,c("ID_KLAK_Melding","Component","Tijdstip_begin_storing","PC_6")]<-klaktabel[i,c("ID_KLAK_Melding","Component","Tijdstip_begin_storing","PC_6")]
    kabelsklak<-rbind(kabelsklak,kabelsklakadd)
    ll<-ll+1}
}

i=1
for(i in 1:nrow(klaktabel)){
  removed<-kabelsklak[which(kabelsklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & kabelsklak$DateRemoved!=""),c("Coo_X","Coo_Y","DateRemoved")]
  added  <-kabelsklak[which(kabelsklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & kabelsklak$DateAdded!=""),c("Coo_X","Coo_Y","DateAdded")]
  dist   <-t(sapply(removed$Coo_X,function(x){(x-added$Coo_X)^2})+sapply(removed$Coo_Y,function(x){(x-added$Coo_Y)^2}))
  tijdsd <-t(sapply(removed$DateRemoved,function(x){as.Date(paste0(x,"04"),format="%y%m%d")-as.Date(paste0(added$DateAdded,"04"),format="%y%m%d")}))
  vervc  <-sapply(apply(floor(dist/4)+floor(abs(tijdsd/45)),1,min),function(x)ifelse(x>0,0,1))
  kabelsklak[which(kabelsklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & kabelsklak$DateRemoved!=""),c("vervc")]<-vervc
}


###voeg datumverschillen toe
kabelsklak[,c("Adiff")]  <-as.Date(paste0(kabelsklak$DateAdded,"04"),format="%y%m%d")-as.Date(kabelsklak$Tijdstip_begin_storing,format="%d-%m-%Y")
kabelsklak[,c("Rdiff")]  <-as.Date(paste0(kabelsklak$DateRemoved,"04"),format="%y%m%d")-as.Date(kabelsklak$Tijdstip_begin_storing,format="%d-%m-%Y")
checkverschil            <-function(dagen,nmin,nmax){if(!is.na(dagen)){if(dagen <= nmax & dagen >=nmin){1}else{0} }else{0}}
kabelsklak[,c("Adiffc")] <-sapply(kabelsklak$Adiff,function(x) checkverschil(x,-30,70))                     #Kan asset verwijderd zijn door storing?
kabelsklak[,c("Rdiffc")] <-sapply(kabelsklak$Rdiff,function(x) checkverschil(x,-30,70))                     #Kan asset toegevoegd zijn door storing?

### Maak dataframe met mogelijk gevonden klakstoringen
klaktabel[,c("countadded")]   <-sapply(klaktabel$ID_KLAK_Melding,function(x) sum(kabelsklak$Adiffc[which(kabelsklak$ID_KLAK_Melding==x)]))     #aantal toegevoegde moffen
klaktabel[,c("countremoved")] <-sapply(klaktabel$ID_KLAK_Melding,function(x) sum(kabelsklak$Rdiffc[which(kabelsklak$ID_KLAK_Melding==x)]))   #aantal weggehaalde moffen
#max(klaktabel$countremoved)
klaktabel[,c("storing")]      <-ifelse(klaktabel$countadded>0 & klaktabel$countremoved>0,1,0)

klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-NA

tabel                         <-sapply(klaktabel$ID_KLAK_Melding,function(x) kabelsklak$ID_Bron[which(kabelsklak$ID_KLAK_Melding==x)])
tabel                         <-t(data.frame(lapply(tabel,function(x) x[1:5])))

klaktabel[,c("asset1","asset2","asset3","asset4","asset5")]<-tabel
klaktabel<-klaktabel[which(klaktabel$storing==1),]

klaktabelkabelsLS             <-klaktabel

Rprof()
summaryRprof("proxyhld.out")