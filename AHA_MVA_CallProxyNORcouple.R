rm(list = setdiff(ls(), lsf.str()))
.First()

#settings for the NORfile and proxylist
Settype="LSmoffen"
NORfile=paste0(settings$Analyse_Datasets,"/2. CoupledWithoutProxy/MVA_Coupled_AnalysisSet_LSmoffen.Rda")
Proxyfile=paste0(settings$Analyse_Datasets,"/4. KA Proxy samengevoegd/Proxy_koppellijst_2015-03-27 21.20.07.Rda")

#load the files and clean some memory
load(NORfile)
load(Proxyfile)
gc(reset=T)

#Select only outages where less then 6 assets will be coupled
setkey(koppellijst[[Settype]],ID_KLAK_Melding)
koppellijst[[Settype]] = koppellijst[[Settype]][(koppellijst[[Settype]][,list(too_many=(sum(punten>=0.3)>5)), by=ID_KLAK_Melding])][
  ,c(names(koppellijst[[Settype]]),"too_many"),with=F]
koppellijst[[Settype]] = koppellijst[[Settype]][!koppellijst[[Settype]]$too_many,]

#set filename to write to
finalSetTargetOutFileName=paste0(settings$Analyse_Datasets,"/3. CoupledWithProxy/MVA_Coupled_AnalysisSet_",Settype,"_withTarget_inclOorzaak.Rda")

#couple the punten into the set
coupledSet=AHA_MVA_CoupleNORproxy(NORset=coupledSet,ProxySet=koppellijst,Settype=Settype)


coupledSet$gestoordAsset = ifelse(coupledSet$punten>=0.3,1,0)
coupledSet[,Oorzaak:=NULL]
coupledSet[,punten:=NULL]
sum(coupledSet$gestoordAsset)

#couple the target variables into the set for the list of thresholds
#coupledSet=AHA_MVA_TargetVariables(coupledNOR=coupledSet,threshold=list(0.3,0.5,1.0))

#save the set and clean memory again
save(coupledSet,file=finalSetTargetOutFileName,compress=F)
gc()


