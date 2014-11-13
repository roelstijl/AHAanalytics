require(data.table)
haskey(moffen)
haskey(kabels)

setnames(kabels,c("Coo_X_van","Coo_Y_van"),c("Coo_X","Coo_Y"))
kabels = unique(kabels,by=c("Coo_X","Coo_Y"))
a=merge(moffen,kabels[,c("Coo_X","Coo_Y","ID_Verbinding"),with=FALSE],by=c("Coo_X","Coo_Y"),all.x=TRUE)

setnames(a,"ID_Verbinding","ID_Verbinding_van")
setnames(kabels,c("Coo_X","Coo_Y"),c("Coo_X_van","Coo_Y_van"))
setnames(kabels,c("Coo_X_naar","Coo_Y_naar"),c("Coo_X","Coo_Y"))

kabels = unique(kabels,by=c("Coo_X","Coo_Y"))

a=merge(a,kabels[,c("Coo_X","Coo_Y","ID_Verbinding"),with=FALSE],by=c("Coo_X","Coo_Y"),all.x=TRUE)
setnames(a,"ID_Verbinding","ID_Verbinding_naar")

load("C:/Datasets/AHAdata/2. Input Datasets/6. NOR/masterdataset_ELCVERBINDINGEN.Rda")


setnames(a,"ID_Verbinding_van","ID_Verbinding")
a=merge(a,masterdataset[,c("ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by="ID_Verbinding",all.x=TRUE)
setnames(a,"ID_Hoofdleiding","ID_Hoofdleiding_van")
setnames(a,"ID_Verbinding","ID_Verbinding_van")

setnames(a,"ID_Verbinding_naar","ID_Verbinding")
a=merge(a,masterdataset[,c("ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by="ID_Verbinding",all.x=TRUE)
setnames(a,"ID_Hoofdleiding","ID_Hoofdleiding_naar")
setnames(a,"ID_Verbinding","ID_Verbinding_naar")

data1 = data[logical,]
data2 = data[!logical,c("NR_Behuizing","Datum","Belasting_component","Min_or_Max","EV","WP","PV","Belasting"),with=FALSE]
setnames(data2,"Belasting","Overbelast")

data3 = merge(
  data1[!duplicated(data1[,c("NR_Behuizing","Datum","Belasting_component","Min_or_Max","EV","WP","PV"),with=FALSE])],
  data2[!duplicated(data2[,c("NR_Behuizing","Datum","Belasting_component","Min_or_Max","EV","WP","PV"),with=FALSE])],
  all.x=TRUE,
  by=c("NR_Behuizing","Datum","Belasting_component","Min_or_Max","EV","WP","PV")
)

