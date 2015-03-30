rm(list = setdiff(ls(), lsf.str()))
.First()

#settings for the NORfile and proxylist
Settype="LSmoffen"
NORfile=paste0(settings$Analyse_Datasets,"/2. CoupledWithoutProxy/MVA_Coupled_AnalysisSet_LSmoffen.Rda")
Proxyfile=paste0(settings$Analyse_Datasets,"/1. Proxylijsten/Proxy_koppellijst_2015-03-25 14.00.00.Rda")

#load the files and clean some memory
load(NORfile)
load(Proxyfile)
gc()

#set filename to write to
finalSetTargetOutFileName=paste0(settings$Analyse_Datasets,"/3. CoupledWithProxy/MVA_Coupled_AnalysisSet_",Settype,"_withTarget_inclOorzaak.Rda")

#couple the punten into the set
coupledSet=AHA_MVA_CoupleNORproxy(NORset=coupledSet,ProxySet=koppellijst,Settype=Settype)

#couple the target variables into the set for the list of thresholds
coupledSet=AHA_MVA_TargetVariables(coupledNOR=coupledSet,threshold=list(0.3,0.5,1.0))

#save the set and clean memory again
save(coupledSet,file=finalSetTargetOutFileName,compress=F)
gc()


