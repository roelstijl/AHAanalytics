AHA_Data_Batch_Processing = function(range)
{

  folder="NOR"
  setfolder     = list.files(settings$Bron_Datasets,pattern=folder)[1]; 
  dir.create(paste0(settings$Ruwe_Datasets,"/",setfolder), showWarnings = FALSE)

  a=list.files(paste0(settings$Bron_Datasets,"/",setfolder))

  all = substr(a[1:95], 17, 20)
  all = substr(a[range], 17, 20)

  for (n in all) {
  AHA_Data_Import("NOR",paste0("ELCVERBINDINGEN_",n),"ELCVERBINDINGEN","save")
   AHA_Data_Import("NOR",paste0("ELCVERBINDINGSDELEN_",n),"ELCVERBINDINGSDELEN","save")
   AHA_Data_Import("NOR",paste0("ELCVERBINDINGSKNOOPPUNTEN_",n),"ELCVERBINDINGSKNOOPPUNTEN","save")
  }
  }
# library(foreach)
# library(doParallel)
# 
# #setup parallel backend to use 8 processors
# cl<-makeCluster(4)
# registerDoParallel(cl)
# 
# stopCluster(cl)