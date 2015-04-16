# Created by Jacco Heres, Alliander
# for project Asset Health Analytics, IT Digitale Netten, Liander
# This script processes the proxy method for all sets and methods, please run AHA_Data_Batch_Processing
AHA_Proxy_Batch_Processing = function(){
  for(set in c("NOR","BAR")){
    for(method in c("PC","XY","TOPO")){
      AHA_Proxy_KA_BAR_NOR(method,set,meldersproc=T,checkverv=T)
    }
  } 
  AHA_Proxy_Merge(global=F,set)
}