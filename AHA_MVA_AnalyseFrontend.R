rm(list = setdiff(ls(), lsf.str()))
.First()
gc()
cat("Note that the alldata set and the metadata already have to exist for this script to run, output them first with the shiny preprocessing tool \n")

#############################
#Settings                   #
#############################
threshold=0.3
settype="LSkabels"
Ntrain=150000
Ntest=2900000
PercTrain=1
Target_val="T"
Target_var="gestoordAsset_th0.3"


#############################
#Load the dataset           #
#############################
ptmOrg=proc.time()[3]
ptm=ptmOrg
filename=paste0("MVA_Coupled_AnalysisSet_",settype,"_withTarget_inclOorzaak")
load(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_alldata.Rda"))
cat("Alldata loaded in ",proc.time()[3]-ptm," s \n")

#########################################
#Remove NAs by imputing mean or onbekend#
#########################################
ptm=proc.time()[3]
if (exists("mindataset")==T){alldata=mindataset
                          rm(mindataset)}

gc()
for (i in names(alldata))
{
  #karakters imputeren en omzetten naar factoren
  if (is.character(alldata[,get(i)]))
  {
    alldata[is.na(get(i)),eval(i):= "Onbekend"]
    alldata[,eval(i):=factor(alldata[,get(i)])]
  }
  #factoren imputeren (daarvoor moeten ze eerst karakter worden...)
  if (is.factor(alldata[,get(i)]))
  {
    alldata[,eval(i):= as.character(alldata[,get(i)])]
    alldata[is.na(get(i)),eval(i):= "Onbekend"]
    alldata[,eval(i):=factor(alldata[,get(i)])]
  }
  if (is.numeric(alldata[,get(i)]))
  {
    alldata[is.na(get(i)),eval(i):= mean(alldata[,get(i)], na.rm=TRUE)]
  }
}

save(alldata,metadata,cfg,file = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_alldataImputed.Rda"),compress=F)
cat("Imputation performed and file saved in ",proc.time()[3]-ptm," s \n")

############################
#Create test and train sets#
############################

#set settings
ptm=proc.time()[3]
st= data.table(Tr_size=as.numeric(Ntrain),
               Tr_tgt=as.numeric(PercTrain)/100,
               tst_size=as.numeric(Ntest),
               rnd_seed=as.numeric(10),
               Target_Value=Target_val, 
               Target_Variable=Target_var)

#Create test and trainset from the already imputed and corrected set
testrows = rep(F,nrow(alldata))
testrows[sample(1:nrow(alldata),st$tst_size)] = T
trainrow = c(sample(which(alldata[[st$Target_Variable]]==st$Target_Value & !testrows),st$Tr_size*(st$Tr_tgt)),
             sample(which(alldata[[st$Target_Variable]]!=st$Target_Value & !testrows),st$Tr_size*(1-st$Tr_tgt)))
testrows = which(testrows)

testset  = alldata[testrows,metadata$names[metadata$selected],with=F]
trainset = alldata[trainrow,metadata$names[metadata$selected],with=F]

save(testset,trainset,metadata,cfg,file = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_test_train_set.Rda"),compress=F)
write.table(testset,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_testset.csv"),sep = ";",na="",row.names = F);
write.table(trainset,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_trainset.csv"),sep = ";",na="",row.names = F);
cat("Test and trainsets created and saved in ",proc.time()[3]-ptm," s \n")

############################
#Run random forest analysis
############################
ptm=proc.time()[3]
paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_test_train_set.Rda")
uitkomst=AHA_MVA_Analyse(aantalimp=1,inputfilename=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_test_train_set.Rda"))
cat("Random forest run done in ",proc.time()[3]-ptm," s --- for trainsize: ",st$Tr_size,"\n")

#calculate failure probabilities 
probs=data.table(predict(uitkomst$resultaat[[1]],alldata,"prob"));
setnames(probs,names(probs),c("P_Nietgestoord","P_Gestoord"))
gc()

###########################################################
#Load the original NOR dataset and couple P_fail into it  #
###########################################################
setname=load(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/",filename,".Rda"))
fullSet=get(setname)

Pfailname=paste0("P_faal",Target_var)

fullSet[,eval(Pfailname):=probs$P_Gestoord]

cat("Total time taken (in minutes): ",(proc.time()[3]-ptmOrg)/60,"\n")
save(fullSet,file=paste0(settings$Analyse_Datasets,"/6. MVA output/",filename,Pfailname,".Rda"),compress=T)
save(uitkomst,file=paste0(settings$Analyse_Datasets,"/6. MVA output/",filename,Pfailname,"_model.Rda"),compress=T)
