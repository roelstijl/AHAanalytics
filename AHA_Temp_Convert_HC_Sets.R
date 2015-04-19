Convert_HC = function(){
  dir   = choose.dir()
files = list.files(dir,"*csv")

sets = list()
for (file in files)  
  sets[[substr(file, 11, 24)]] = fread(paste0(dir,"\\",file))

figures = ldply(sets,function(x) data.table(doel=sum(x$gestoordAsset_th0.3=="T"),totaal=length(x$gestoordAsset_th0.3),variables = ncol(x)))

for (name in names(sets)){
  sets[[name]][,Datum_Inbedrijf := NULL]
  sets[[name]][,ID_NAN := NULL]
  sets[[name]][,ID_NAN_present := NULL]
  sets[[name]][,ID_unique := NULL]
  sets[[name]][,ID_unique_present := NULL]
  sets[[name]][,Datum_Inbedrijf := NULL]
  sets[[name]][,Coo_X := NULL]
  sets[[name]][,Coo_Y := NULL]
  sets[[name]][,Coo_X_van := NULL]
  sets[[name]][,Coo_Y_van := NULL]
  sets[[name]][,Aantal_Droge_Dagen := NULL]
  if(grepl("kabels",name))
  sets[[name]][Mean_Load_Fraction>1,Mean_Load_Fraction := NA]  
}

for (name in names(sets))
setorder(sets[[name]],Aantal_Inwoners_Buurt,Datum_Inbedrijf_Jaar,
         Oppervlakte_Totaal_Buurt,Personenauto.s_Totaal_Buurt,na.last=T)

for (name in names(sets)){
  setnames(sets[[name]],"gestoordAsset_th0.3","Gestoorde_Asset")
  sets[[name]][Gestoorde_Asset=="T",Gestoorde_Asset:="Gestoord"]
  sets[[name]][Gestoorde_Asset=="F",Gestoorde_Asset:="Niet Gestoord"]  
}

for (name in names(sets))
write.csv(sets[[name]],row.names = F,na = "",file=paste0(dir,"\\",name,".csv"))
}

