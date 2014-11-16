load("N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_LS.Rda")
KLAK_LS=data.frame(mindataset)
load("N:/Multivariate Analyse/AHAdata/1. Ruwe Datasets/4. KLAK/KLAK_MS.Rda")
KLAK_MS=data.frame(mindataset)

gismutaties<-read.csv("N:/Multivariate Analyse/AHAdata/0. Ongebruikte en brondata/21. KLAK GIS-mutaties/gis mutaties alles.csv",sep=";", header=TRUE)
colnames(gismutaties)
nrow(gismutaties)
table(table(gismutaties$GMU_MELDING))

colnames(KLAK_LS)
nrow(KLAK_LS)
table(table(KLAK_LS$ID_KLAK_Melding))                                                                                                       
nrow(KLAK_MS)
table(table(KLAK_MS$ID_KLAK_Melding)) 

klakls<-merge(KLAK_LS,gismutaties,by.x="ID_KLAK_Melding",by.y="GMU_MELDING",all.x=TRUE)
nrow(klakls)
View(klakls)

klakms<-merge(KLAK_MS,gismutaties,by.x="ID_KLAK_Melding",by.y="GMU_MELDING",all.x=TRUE)
nrow(klakms)

