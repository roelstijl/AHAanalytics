
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

duprow = function (changes,colname)
{
  subdata = changes[,c("ID_unique","Date",colname),with=FALSE]
  !(duplicated(subdata)| duplicated(subdata,fromLast=TRUE))
}

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

