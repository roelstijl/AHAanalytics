# Take original BAR set
load("E:/1. Alliander/3. Asset Health Analytics/1. Ruwe Datasets/1. BARlog/MH_NRG_MS_KABELS_XY_PC6.Rda")
BAR_LS_Kabels = mindataset

load("E:/1. Alliander/3. Asset Health Analytics/1. Ruwe Datasets/6. NOR/ELCVERBINDINGSDELEN_1501.Rda")
NOR_source = mindataset

load("E:/1. Alliander/3. Asset Health Analytics/2. Input Datasets/6. NOR/masterdataset_ELCVERBINDINGSDELEN_XY_PC6.Rda")
NOR_PC = masterdataset

load("E:/1. Alliander/3. Asset Health Analytics/2. Input Datasets/6. NOR/masterdataset_ELCVERBINDINGSDELEN.Rda")
NOR_old = masterdataset

NOR_source = NOR_source[!is.na(ID_NAN)]
NOR_old = NOR_old[!is.na(ID_NAN)]
NOR_PC  = NOR_PC[!is.na(ID_NAN)]

setkey(NOR_source,ID_NAN)
setkey(NOR_PC,ID_NAN)
setkey(BAR_LS_Kabels,ID_NAN)
setkey(NOR_old,ID_NAN)

NOR_source    = unique(NOR_source)
NOR_old       = unique(NOR_old)
NOR_PC        = unique(NOR_PC)
BAR_LS_Kabels = unique(BAR_LS_Kabels)

setnames(NOR_PC,c("PC_6_van","Coo_X_van","Coo_Y_van"),c("PC_6_x","Coo_X_x","Coo_Y_x"))

setnames(NOR_PC,c("PC_6_naar","Coo_X_naar","Coo_Y_naar"),c("PC_6_van","Coo_X_van","Coo_Y_van"))

colren = c("PC_6_van","Coo_X_van","Coo_Y_van")
setnames(NOR_source,colren,paste0(colren,"_","NOR_source"))
setnames(NOR_old,colren,paste0(colren,"_","NOR_old"))
setnames(NOR_PC,colren,paste0(colren,"_","NOR_PC"))
setnames(BAR_LS_Kabels,colren,paste0(colren,"_","BAR_LS_Kabels"))

PCXY = BAR_LS_Kabels[,list(ID_NAN,PC_6_van_BAR_LS_Kabels,Coo_X_van_BAR_LS_Kabels,
                           Coo_Y_van_BAR_LS_Kabels)][NOR_old[,list(PC_6_van_NOR_old,Coo_X_van_NOR_old,
                                                     Coo_Y_van_NOR_old,ID_NAN)]][NOR_PC[,list(PC_6_van_NOR_PC,Coo_X_van_NOR_PC,
                                                                                 Coo_Y_van_NOR_PC,ID_NAN)]][NOR_source[,list(PC_6_van_NOR_source,Coo_X_van_NOR_source,
                                                                                                                       Coo_Y_van_NOR_source,ID_NAN)]]
setcolorder(PCXY, cn(PCXY))

row.sample(PCXY[,list(PC_6_van_BAR_LS_Kabels,PC_6_van_NOR_old, PC_6_van_NOR_PC, PC_6_van_NOR_source)],100)

write.csv(row.sample(PCXY,1000),file="Postcode_tests.csv")


