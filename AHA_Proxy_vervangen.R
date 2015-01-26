# Analyse vervangen ja/nee
config=list()
config$vervdiff$min = -45 # Aantal dagen tussen verwijderde en toegevoegde asset
config$vervdiff$max =  45 # Aantal dagen tussen verwijderde en toegevoegde asset
config$sdiff$max    =   2 # Afstand tussen verwijderde en toegevoegde asset


#  assets$LSkabels$Coo_X_van  <- as.numeric(assets$LSkabels$Coo_X_van)
#  assets$LSkabels$Coo_X_naar <- as.numeric(assets$LSkabels$Coo_X_naar)
#  assets$LSkabels$Coo_Y_van <- as.numeric(assets$LSkabels$Coo_Y_van)
#  assets$LSkabels$Coo_Y_naar <- as.numeric(assets$LSkabels$Coo_Y_naar)
#  
#  assets$MSkabels$Coo_X_van  <- as.numeric(assets$MSkabels$Coo_X_van)
#  assets$MSkabels$Coo_X_naar <- as.numeric(assets$MSkabels$Coo_X_naar)
#  assets$MSkabels$Coo_Y_van <- as.numeric(assets$MSkabels$Coo_Y_van)
#  assets$MSkabels$Coo_Y_naar <- as.numeric(assets$MSkabels$Coo_Y_naar)






kabel_vervangen = function(kabelset, kabel,config){
  #print(kabel$ID_unique)
  if (kabel$Status_ID == "Length_changed"){TRUE}
  else if (kabel$Status_ID =="Active"    ){FALSE}
  else {
    welkekabelsidbuurt=which(
      ((kabelset$Coo_X_van - kabel$Coo_X_van)^2 + (kabelset$Coo_Y_van - kabel$Coo_Y_van)^2)<config$sdiff$max |
      ((kabelset$Coo_X_naar - kabel$Coo_X_van)^2 + (kabelset$Coo_Y_naar - kabel$Coo_Y_van)^2)<config$sdiff$max |
      ((kabelset$Coo_X_van - kabel$Coo_X_naar)^2 + (kabelset$Coo_Y_van - kabel$Coo_Y_naar)^2)<config$sdiff$max |
      ((kabelset$Coo_X_naar - kabel$Coo_X_naar)^2 + (kabelset$Coo_Y_naar - kabel$Coo_Y_naar)^2)<config$sdiff$max 
      )
    kabelsidbuurt=kabelset[welkekabelsidbuurt,]
  if(sum(
      ((kabelsidbuurt$DateAdded-kabel$DateRemoved)>config$vervdiff$min) &
      ((kabelsidbuurt$DateAdded-kabel$DateRemoved)<config$vervdiff$max)
      )>0){TRUE}else{FALSE}
  }
}

#MS
system.time(
test <- ddply(assets$MSkabels,.(ID_unique),function(x){kabel_vervangen(assets$MSkabels,x,config)})
)


test      <-    data.table(test,key="Index")
setnames(test,"V1","is.verv")
setkey(assets$MSkabels,Index)
assets$MSkabels <- assets$MSkabels[test]

#LS
library(doParallel)
library(foreach)
pb = txtProgressBar(min = 0, max = 1)
registerDoParallel(makeCluster(3))
system.time({
  test2 <- ddply(assets$LSkabels,.(Index),function(x){kabel_vervangen(assets$LSkabels,x,config)})
})

test2      <-    data.table(test2,key="Index")
setnames(test2,"V1","is.verv")
setkey(assets$MSkabels,Index)
assets$MSkabels <- assets$MSkabels[test2]


write.csv(test2, file="C:/Data/AHAdata/vervangen2.csv")
write.csv(test, file="C:/Data/AHAdata/vervangenMSkabels.csv")
