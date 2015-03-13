#This script is meant AS AN EXAMPLE to merge the proxy to the NOR. 

#ADJUST THIS SCRIPT INTO A FUNCTION FOR THE COUPLING SCRIPT LATER

load(paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/Proxylijsten/Proxy_koppellijst_2015-03-10 17.17.34.Rda"))
load("F:/1. Alliander Datasets/3. Base Dataset (09-03-2015)/AHA_Proxy_partial_data_assets_NOR.Rda")

#First preprocess the koppellijst to select only each ID_unique with the highest punten
LSkabelsKoppel=koppellijst$LSkabels

#check whether ID_unique in the koppelijst has PC6 by making it numeric, if so: cut off the last 4 characters because we couple to PC2
if (sum(is.na(as.numeric(LSkabelsKoppel$ID_unique)))>100){
  LSkabelsKoppel$ID_unique=substr(LSkabelsKoppel$ID_unique,1,nchar(LSkabelsKoppel$ID_unique)-4)
}

#Add a column with the maximum punten for later comparison
LSkabelsKoppel[,maximum := max(punten),by=ID_unique]
LSkabelsKoppel=LSkabelsKoppel[maximum == punten,]

#select one of the ID_uniques at random when punten is equal
setkey(LSkabelsKoppel,ID_unique)
LSkabelsKoppel=unique(LSkabelsKoppel)

#clean the LSkabelsKoppel table to only include the useful fields
LSkabelsKoppel=LSkabelsKoppel[,list(ID_unique,punten)]

#Now do the coupling to the nor set, first load the LSkabels from the NOR
LSkabels=assets$LSkabels

#set the keys to couple on
setkey(LSkabels,ID_unique)
setkey(LSkabelsKoppel,ID_unique)

#perform the coupling and set anything that doesn't couple to 0 (because there is no failure in that case)
coupledNOR=LSkabelsKoppel[LSkabels]
coupledNOR[is.na(punten),punten:=0]

threshold=0.1
coupledNOR[punten>threshold,gestoord:=1]
coupledNOR[!(punten>threshold),gestoord:=0]

#correct missing PC6 values
coupledNOR[is.na(PC_6_van),PC_6_van:=i.PC_6_van]
coupledNOR[is.na(PC_6_naar),PC_6_naar:=i.PC_6_naar]

#remove rows with missing X,Y coordinates
coupledNOR=coupledNOR[is.na(Coo_X_van)==F | is.na(Coo_Y_van)==F,]

save(coupledNOR,file=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/ProxylistLSpartial.Rda"),compress=F)


#sampling by criterion:
#coupledNORsample_LS=coupledNOR[,.SD[sample(.N,5000)],by = gestoord]
Ngestoord=5000
Nnietgestoord=15000
coupledNORsample_LS=coupledNOR[gestoord==1,.SD[sample(.N,Ngestoord)],]
coupledNORsample_LS=rbind(coupledNORsample_LS,coupledNOR[gestoord==0,.SD[sample(.N,Nnietgestoord)],])

save(coupledNORsample_LS,file=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/ProxylistSampleLSkabels.Rda"),compress=F)
