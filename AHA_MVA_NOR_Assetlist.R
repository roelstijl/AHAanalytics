datums = as.Date("2014-02-01")
cutoff = 0.5
sample.size = 25000

koppeltbl=rbindlist(koppellijst)[punten>=0.5]

rel_kabels = assets$kabels[DateRemoved > datums |is.na(DateRemoved)]
rel_moffen = assets$moffen[DateRemoved > datums |is.na(DateRemoved)]

 = (rel_kabels$ID_unique %in% koppeltbl$ID_unique)
 = (rel_moffen$ID_unique %in% koppeltbl$ID_unique)

sample((1:nrow(rel_kabels))[!sample_kabels],sample.size)
sample((1:nrow(rel_moffen))[!sample_moffen],sample.size)

sample_kabels = rbind(rel_kabels[ID_unique %in% koppeltbl$ID_unique,Gestoord := "Gestoord"],
                      rel_kabels[sample((1:nrow(rel_kabels))[!sample_kabels],sample.size),Gestoord := "Niet_gestoord"]) 

sample_moffen = rbind(rel_moffen$ID_unique %in% koppeltbl$ID_unique,rel_moffen[Gestoord := "Gestoord"],
                      rel_moffen[sample((1:nrow(rel_moffen))[!sample_moffen],sample.size),Gestoord := "Niet_gestoord"])  

id_sample_set <-c(a$ID_unique,row.sample(masterdataset$ID_unique[!(masterdataset$ID_unique %in% a$ID_unique)],20000)
mva_sample_set <- assets$kabels[id_sample_set]
mva_sample_set$gestoord <- sapply(mva_sample_set$ID_unique,function(x){x %in% a$ID_unique})
