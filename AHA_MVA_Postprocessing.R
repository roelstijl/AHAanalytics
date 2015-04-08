AHA_MVA_postprocessing = function(){
# Roel Stijl (Bearingpoint) 2015
# Some postprocessing functions for the MVA data
dataset  = LoadWrap()
gestoord = table(dataset$gestoordAsset_th0.3)
setsize  = 100000

dataset2=rbind(dataset[gestoordAsset_th0.3 == "T"],dataset[gestoordAsset_th0.3 == "F"][sample(1:gestoord[["F"]],setsize-gestoord[["T"]])])
SaveWrap(dataset2,settings$Last_Load)
}