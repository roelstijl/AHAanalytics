# Inladen data
load("C:/Data/AHAdata/2. Input Datasets/AHA_Proxy_partial_data.Rda")
load("C:/Data/AHAdata/2. Input Datasets/AHA_Proxy_partial_data_storingen.Rda")
library(data.table)
library(plyr)
mymin <- function(...,def=0,na.rm=FALSE)
  {if(!is.infinite(x<-suppressWarnings(min(...,na.rm=na.rm)))) x else def}


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
storingen$KLAKMELDERS                                  <-  merge(data.frame(storingen$KLAKMELDERS),data.frame(nettopo$EAN_koppel),by.x=c("PC6","Huisnr"),by.y=c("PC_6","Huisnr"),all.x=TRUE)
storingen$KLAKMELDERS                                  <- data.table(storingen$KLAKMELDERS)
setkey(storingen$KLAKMELDERS,ID_Groep)                                           #Instellen van Groepsid als key, om snel te zoeken
setkey(assets$kabels,ID_Verbinding)                                              #Instellen van EAN-nummer als key, om snel te zoeken

#sum(ifelse(is.na(KLAKMELDERS2$ID_EAN),1,0))
#table(table(KLAKMELDERS2$MELDING))

#Proxy LS-kabels koppelen via hoofdleiding --------------------------------------
kabelsklak<-assets$kabels[0,]
kabelsklak[,ID_KLAK_Melding:=NA]         ; kabelsklak[,ID_KLAK_Melding:=as.integer(ID_KLAK_Melding)]
kabelsklak[,Netcomponent:=NA]            ; kabelsklak[,Netcomponent:=as.character(Netcomponent)]
kabelsklak[,Tijdstip_begin_storing:=NA]  ; kabelsklak[,Tijdstip_begin_storing:=as.character(Tijdstip_begin_storing)]
kabelsklak[,Gmu_Verwerking_Gereed:=NA]   ; kabelsklak[,Gmu_Verwerking_Gereed:=as.character(Gmu_Verwerking_Gereed)]
kabelsklak[,PC_6:=NA]                    ; kabelsklak[,PC_6:=as.character(PC_6)]
kabelsklak[,ID_Groep:=NA]                ; kabelsklak[,ID_Groep:=as.double(ID_Groep)]


ll<-0
for(i in 1:nrow(klaktabel)){
  ifelse(is.na(klaktabel$ID_Groep[i]),
         klakmelders         <- storingen$KLAKMELDERS[0,],                                                                   #Indien ID_groep onebekend is, kijk alleen naar KLAK-melding
        {klakmelders         <- storingen$KLAKMELDERS[klaktabel$ID_Groep[i],] #Koppel alle klakmelders aan melding
         klakmelders         <- klakmelders   [which(complete.cases(klakmelders$ID_Hoofdleiding)),]})                            #Verwijder NA's
if(nrow(klakmelders)>0){
   kabelsklakadd   <- assets$kabels [list(as.character(unique(klakmelders$ID_Hoofdleiding))),]
   kabelsklakadd   <- kabelsklakadd [complete.cases(kabelsklakadd$ID_unique)]
   countremoved    <- ifelse(is.na(sum(kabelsklakadd$DateRemoved != "")),0,sum(kabelsklakadd$DateRemoved != ""))
   klaktabel$countremoved[i] <-  countremoved     
   lengthchanged   <- mymin(kabelsklakadd$Length_ch,na.rm=T)
   if(countremoved>0 | lengthchanged<0){
      kabelsklakadd[,c("ID_KLAK_Melding", "Netcomponent","Tijdstip_begin_storing","Gmu_Verwerking_Gereed","PC_6", "ID_Groep")
                 :=klaktabel[i,c("ID_KLAK_Melding", "Netcomponent","Tijdstip_begin_storing","Gmu_Verwerking_Gereed","PC_6", "ID_Groep"),with=F]]
      kabelsklak    <-rbind(kabelsklak,kabelsklakadd)
      ll<-ll+1
    }
  }
}

#bereken datumverschillen
kabelsklak[,c("Adiff")]<-as.Date(paste0(kabelsklak$DateAdded,"04"),format="%y%m%d")-as.Date(kabelsklak$Tijdstip_begin_storing,format="%d-%m-%Y")
kabelsklak[,c("Rdiff")]<-as.Date(paste0(kabelsklak$DateRemoved,"04"),format="%y%m%d")-as.Date(kabelsklak$Tijdstip_begin_storing,format="%d-%m-%Y")
kabelsklak[,c("Ldiff")]<-as.Date(paste0(kabelsklak$Date_Length_ch,"04"),format="%y%m%d")-as.Date(kabelsklak$Tijdstip_begin_storing,format="%d-%m-%Y")

kabelsklak[,c("Adiff")]  = as.Date(paste0(kabelsklak$DateAdded,"04"),format="%y%m%d")-as.Date(kabelsklak$Gmu_Verwerking_Gereed,format="%d-%m-%Y")
kabelsklak[,c("Rdiff")]  = as.Date(paste0(kabelsklak$DateRemoved,"04"),format="%y%m%d")-as.Date(kabelsklak$Gmu_Verwerking_Gereed,format="%d-%m-%Y")
kabelsklak[,c("Ldiff")]<-as.Date(paste0(kabelsklak$Date_Length_ch,"04"),format="%y%m%d")-as.Date(kabelsklak$Tijdstip_begin_storing,format="%d-%m-%Y")

checkverschil<-function(dagen,nmin,nmax){if(!is.na(dagen)){if(dagen <= nmax & dagen >=nmin){1}else{0} }else{0}}
kabelsklak[,c("Adiffc")]<-sapply(kabelsklak$Adiff,function(x) checkverschil(x,-30,70))                     #Kan asset verwijderd zijn door storing?
kabelsklak[,c("Rdiffc")]<-sapply(kabelsklak$Rdiff,function(x) checkverschil(x,-30,70))                     #Kan asset toegevoegd zijn door storing?
kabelsklak[,c("Ldiffc")]<-sapply(kabelsklak$Ldiff,function(x) checkverschil(x,-30,70))                     #Kan asset toegevoegd zijn door storing?

#Bekijk of Asset is vervangen door andere asset
kabelsklak <-data.frame(kabelsklak)
for(i in which(klaktabel$countremoved>0)){
  removed<-kabelsklak[which(kabelsklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & kabelsklak$DateRemoved!=""),c("Coo_X","Coo_Y","DateRemoved")]
  added  <-kabelsklak[which(kabelsklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & kabelsklak$DateAdded!=""),c("Coo_X","Coo_Y","DateAdded")]
  dist   <-t(sapply(removed$Coo_X,function(x){(x-added$Coo_X)^2})+sapply(removed$Coo_Y,function(x){(x-added$Coo_Y)^2}))
  tijdsd <-t(sapply(removed$DateRemoved,function(x){as.Date(paste0(x,"04"),format="%y%m%d")-as.Date(paste0(added$DateAdded,"04"),format="%y%m%d")}))
  vervc  <-sapply(apply(floor(dist/4)+floor(abs(tijdsd/45)),1,min),function(x)ifelse(x>0,0,1))
  kabelsklak[which(kabelsklak$ID_KLAK_Melding==klaktabel$ID_KLAK_Melding[i] & kabelsklak$DateRemoved!=""),c("vervc")]<-vervc
}

#Selecteer alleen LS kabels
kabelsklak        <- kabelsklak[which(kabelsklak$Voltage %in% c("400V","Onbekend")),]

#Selecteer verwijderde moffen die in de rondom de storing vervangen zijn
LSkabelsklakhld <- kabelsklak[which((kabelsklak$Rdiffc==1 & kabelsklak$vervc ==1)|kabelsklak$Ldiffc ==1),]
LSkabelsklakhld <- transform(LSkabelsklakhld, freq.KLAKmelding=ave(seq(nrow(LSkabelsklakhld)),ID_KLAK_Melding,FUN=length))
LSkabelsklakhld <- LSkabelsklakhld[which(LSkabelsklakhld$freq.KLAKmelding<6),]
table(table(LSkabelsklakhld$ID_KLAK_Melding))
table(LSkabelsklakhld$Netcomponent)

save(LSkabelsklakhld, file="N:/Multivariate Analyse/AHAdata/3. Analyse Datasets/LSkabelsklakhld.Rda")

nm =merge(data.frame(table(storingen$LS$Netcomponent)),
          data.frame(table(LSkabelsklakhld$Netcomponent)),
          by="Var1",all=T)
nm <- data.frame(nm[,-1], row.names=nm[,1])
nm <- data.frame(mapply(`*`,nm,apply(nm,2,function(x) 1/sum(x,na.rm=T))), row.names=row.names(nm))
par(mar=c(4, 12, 2, 0.5))
barplot(t(nm),col=c("darkblue","red"),horiz=TRUE, beside=TRUE,las=1)

# proxy LS kabels via HLD ----------------------------------------------------
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