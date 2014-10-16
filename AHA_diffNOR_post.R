require(data.table)

########### Just some random functions to correct mistakes in the NOR and prep the data ################
duprow = function (changes,colname)
{  subdata = changes[,c("ID_unique","Date",colname),with=FALSE]
   !(duplicated(subdata)| duplicated(subdata,fromLast=TRUE))}

changes$key=1:nrow(changes)
setkey(changes,key)
changes = changes[order(changes$ID_unique)]
changes$key=1:nrow(changes)
setkey(changes,key)
haschanged = duprow(changes,"Lengte")

# length
lchanged   = changes[haschanged] 
ldelta1 = lchanged[1:(nrow(lchanged)/2)*2-1,Lengte]
ldelta1[is.na(ldelta1)] = 0
ldelta2 = lchanged[1:(nrow(lchanged)/2)*2,Lengte]
ldelta2[is.na(ldelta2)] = 0
ldelta  = ldelta2 - ldelta1 

VERBINDINGSDELEN.length.changed = cbind(lchanged[1:nrow(lchanged)*2-1,c("ID_unique","Date"),with=FALSE],ldelta)

a=ldelta[ldelta!=0]
b= cbind(table(a[a>0 & a<50]),rev(table(a[a<0 & a>(-50)])))
barplot(t(b),beside=TRUE)

barplot(table(lengthchanged$Date))

as.numeric(sapply(KLAK_LS$Co_X,fixnumber))
as.numeric(sapply(KLAK_LS$Co_X,fixnumber))

####################### Merge moffen with hoofdleidingen ##################
require(data.table)
haskey(moffen)
haskey(kabels)

setnames(kabels,"Lengtenum","Lengte")
kabels[,Lengte:=as.integer(kabels$Lengtenum)]

moffen= unique(verbindingen[,c("ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by=c("ID_Verbinding"))
kabels=merge(kabels,a,by=c("ID_Verbinding"),all.x=TRUE)

##############

setnames(kabels,c("Coo_X_van","Coo_Y_van"),c("Coo_X","Coo_Y"))
kabels = unique(kabels,by=c("Coo_X","Coo_Y"))
moffen=merge(moffen,kabels[,c("Coo_X","Coo_Y","ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by=c("Coo_X","Coo_Y"),all.x=TRUE)

setnames(moffen,"ID_Verbinding","ID_Verbinding_van")
setnames(moffen,"ID_Hoofdleiding","ID_Hoofdleiding_van")

setnames(kabels,c("Coo_X","Coo_Y"),c("Coo_X_van","Coo_Y_van"))
setnames(kabels,c("Coo_X_naar","Coo_Y_naar"),c("Coo_X","Coo_Y"))

kabels = unique(kabels,by=c("Coo_X","Coo_Y"))

moffen=merge(moffen,kabels[,c("Coo_X","Coo_Y","ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by=c("Coo_X","Coo_Y"),all.x=TRUE)
setnames(moffen,"ID_Verbinding","ID_Verbinding_naar")
setnames(moffen,"ID_Hoofdleiding","ID_Hoofdleiding_naar")
setnames(kabels,c("Coo_X","Coo_Y"),c("Coo_X_naar","Coo_Y_naar"))


load("C:/Datasets/AHAdata/2. Input Datasets/6. NOR/masterdataset_ELCVERBINDINGEN.Rda")

masterdataset = unique(masterdataset,by="ID_Verbinding")
setnames(a,"ID_Verbinding_van","ID_Verbinding")
a=merge(a,masterdataset[,c("ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by="ID_Verbinding",all.x=TRUE)
setnames(a,"ID_Hoofdleiding","ID_Hoofdleiding_van")
setnames(a,"ID_Verbinding","ID_Verbinding_van")

setnames(a,"ID_Verbinding_naar","ID_Verbinding")
a=merge(a,masterdataset[,c("ID_Hoofdleiding","ID_Verbinding"),with=FALSE],by="ID_Verbinding",all.x=TRUE)
setnames(a,"ID_Hoofdleiding","ID_Hoofdleiding_naar")
setnames(a,"ID_Verbinding","ID_Verbinding_naar")

sum(!(a$ID_Hoofdleiding_van %in% a$ID_Hoofdleiding_naar))

################## Load some functions #######################
fixnumber = function(x) {
  val= strsplit(x,",")[[1]];
  
  if (suppressWarnings(!is.na(as.numeric(val[1])))){
    len=length(val); cor=ifelse(nchar(tail(val,1))==2,100,1000)
    if(len==1) {a=val[1]
    } else if(len==2) {
      a=(as.numeric(val[1])+as.numeric(val[2])/cor)
    } else if(len==3) {
      a=(as.numeric(val[1])*1000+as.numeric(val[2])+as.numeric(val[3])/cor)
    }
  }
  else{
    a=NA
  }
  #cat(paste0(a,", "))
  return(as.numeric(a))
}

