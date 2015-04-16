# Containts lots of extra functions for the AHA project
cNA = function(dataset){
# Roel Stijl (Bearingpoint) 2015
# Produces a simple bar chart with your NA and empty values
par(mfrow=c(1, 1), mar=c(2, 10, 0, 2))
  
#   for (x in which(!laply(dataset,is.character))){
#     suppressMessages( set(dataset,i=NULL,j=x,as.character(dataset[,x,with=F])))
#   }
  setcolorder(dataset, cn(dataset))

  barplot(t(cbind(sapply(dataset,function(x) sum(as.character(x)=="",na.rm=T)),
                  sapply(dataset,function(x) sum(is.na(x))),
                  sapply(dataset,function(x) sum(!is.na(x))-sum(as.character(x)=="",na.rm=T)))), horiz=TRUE,las=1,cex.names=0.7,legend = c("empty","NA","Not NA"))
}

LoadWrap = function(filename="NULL",filetype = "",filepath=settings$Analyse_Datasets,cfg=list()){
# Roel Stijl (Bearingpoint) 2015
# A little wrapper to make the loading easier and easier to modify
# filename: is the name of the file you wish to load, filetype detected based on extention
# filetype: legacy
# filepath: where to start the prompt is filename == NULL
# cfg: input the cfg to 
  if(filename=="NULL") filename= choose.files(default = paste0(filepath,"/",filetype,"*"))
  settings$Last_Load <<- filename
  
  switch(file_ext(filename),
         ssv = {return(data.table(fread(filename)))},
         csv = {return(data.table(fread(filename)))},
         Rda = {SetName  = load(filename)
                return(get(SetName))}
)

}

SaveWrap = function(mindataset,filename)
{
  dir.create(dirname(filename), showWarnings = FALSE)
  
  switch(file_ext(filename),
           csv = {write.csv(mindataset,file=filename,row.names = F)},
          Rda = {save(mindataset,file=filename,compress=F)}
          )
  
}

vector_in_DT = function(vector,DT){
  # Compares all values in the seperate DT cols and returns number of matches without NA
  # Removed cases spaces e.d. for fuzzier comparison
  toremove = " |,|-|\\.|\\'"
  vector2   = na.omit(gsub(toremove,"",tolower(vector)))
  
  a=data.table(laply(names(DT),function(x) 
    cbind(x,sum((vector2  %in% na.omit(gsub(toremove,"",tolower(DT[[x]])))))))); 
  
  setnames(a,c("Name","Matches"))
  a$Matches = as.numeric(a$Matches)
  setorder(a,Matches);
  cat(paste0("Results\nMaximum: ", length(vector), ", with NAs: ", 
             sum(is.na(vector)),", best match: ",max(a$Matches,na.rm = T)," = ",
             round(max(a$Matches,na.rm = T)/length(vector2)*100),"% (",
             round(max(a$Matches,na.rm = T)/length(vector)*100),"% w NA)", "\n\n"))
  return(a)
}



substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

riverplot = function(){
nodes <- c("B","A","BB","C")
edges <- list( A= list( C= 0.7*0.2*75000*100,BB=1000000), B= list( C= 0.1*0.3*75000*100 ),BB= list( C= 0.3*0.3*75000*100 ))
r <- makeRiver( nodes, edges, node_xpos= c( 2,2,1,3 ),node_styles= list( A= list( col= "yellow" )) )
plot( r )

na=100000
plot(makeRiver(c("A","B","C","D","E"),list(A=list(B=0.7*na,C=0.3*na),B=list(D=0.8*0.7*na,E=0.2*0.7*na),C=list(D=0.9*0.3*na,E=0.1*0.3*na)),
               node_xpos=c(1,2,2,3,3),
               node_labels= c( A= "Auction churners (2013-2014)", B= "Don't contact save desk", C= "Contact save desk",D="Auction Churners",E= "Retained with HC rules"),
               default_style= list( col="#FF8080" ),
               node_styles= list( E= list( col= "red" ))),lty=1)
}



dup = function(data,by=NULL) duplicated(data,by=by)

cDUB = function(dataset)
{
  par(mfrow=c(1, 1), mar=c(2, 10, 0, 2))
  barplot(t(cbind(sapply(dataset,function(x) sum(duplicated(x))),
                  sapply(dataset,function(x) sum(!duplicated(x))))), horiz=TRUE,las=1,cex.names=0.7,legend = c("Duplicated","Not duplicated"))
}

invlogical = function (logic1,logic2)
{
  # Inverse logical operation maps logic 2 on logic 1
  output = logic1
  output[logic1] = logic2
  return(output)
}

row.sample <- function(dta, rep = 20) {
  dta <- as.data.frame(dta) # for single variables
  dta[sample(1:nrow(dta), rep, replace=FALSE), ] 
} 

cn = function (x){
  sort(names(x))
}

importldr = function(sourcefile,colclass){ 
  a = readLines(sourcefile)
  b = paste(a, collapse = "")
  c = gsub("\\{EOL\\}","\n",b)
  d = fread(c,header=TRUE,colClasses=colclass,sep="|")
}

loadObj <- function(file.name){
  library(foreach)
  filesize <- file.info(file.name)$size
  chunksize <- ceiling(filesize / 100)
  pb <- txtProgressBar(min = 0, max = 100, style=3)
  infile <- file(file.name, "rb")
  data <- foreach(it = icount(100), .combine = c) %do% {
    setTxtProgressBar(pb, it)
    readBin(infile, "raw", chunksize)
  }
  close(infile)
  close(pb)
  return(unserialize(data))
}

saveObj <- function(object, file.name){
  outfile <- file(file.name, "wb")
  serialize(object, outfile)
  close(outfile)
}

savewrapper = function(..., file, compress=F)
{
  save()
}

pbarwrapper = function(title="PlaceHolder", label = "Starting...", max = 1,min = 0,initial = 0,width = 450){
  # Roel Stijl, Bearingpoint 2015
  # Used for the progressbar generation
  # use setpbwrapper to set steps of progress
  # title, label and max are needed, remainder is there for historic reasons
  
  cat("\n")
  gltitle <<- title
  label     = paste0(title, ": ",label)
  pb        = txtProgressBar2(title = title, label = label, min = 0, max = max, initial = 0, style = 3)
  glpc    <<-  0
  glpb    <<- pb
  return(pb)
}

# Used for the progressbar
setpbarwrapper = function(pb=T,index=-1,label="title",title=""){
  if (title!=""){
    gltitle <<-title
  }
  if (is.logical(pb)){
    pb=glpb
  }
  if (index==-1){
    glpc <<- glpc+1; 
    index=glpc;
  }
  label = paste0(gltitle, ": ", label,"                                         ")
  setTxtProgressBar2 (pb, index,label = label)
}

# Corrects corrupted NRG data ---------------------
AHA_DATA_Correct_NRG_Corruption= function(){
  require(data.table)
  MS_stations <- fread(paste0(settings$Bron_Datasets,"/11. Nettopologie/MS_Stations.txt"),sep="\t") 
  MS_hld <- read.csv(paste0(settings$Bron_Datasets,"/11. Nettopologie/MS_hoofdleidingen.txt"),sep="\t",colClasses="character") 
  
  setnames(MS_stations,as.character(MS_stations[1,])) #set names first row (headers) to characters 
  MS_stations <- MS_stations[-1,] #delete first row 
  
  write.csv(MS_stations, file =paste0(settings$Bron_Datasets,"/11. Nettopologie/MS_Stations.csv"))
  write.csv(MS_hld, file =paste0(settings$Bron_Datasets,"/11. Nettopologie/MS_hoofdleidingen.csv"))
  
}

shorten= function(checkboxes,amount) 
{names(checkboxes) = paste0(substring(checkboxes,1,amount),"...");return(checkboxes)}

txtProgressBar2 <- function (min = 0, max = 1, initial = 0, char = "=", width = NA, 
                             title="", label="", style = 1) 
{
  force(label);force(title)
  if (!style %in% 1L:3L) 
    style <- 1
  .val <- initial
  .killed <- FALSE
  .nb <- 0L
  .pc <- 0L
  nw <- nchar(char, "w")
  if (is.na(width)) {
    width <- getOption("width")
    if (style == 3L) 
      width <- width - 10L
    width <- trunc(width/nw)
  }
  up1 <- function(value) {
    if (!is.finite(value) || value < min || value > max) 
      return()
    .val <<- value
    nb <- round(width * (value - min)/(max - min))
    if (.nb < nb) {
      cat(paste(rep.int(char, nb - .nb), collapse = ""))
      flush.console()
    }
    else if (.nb > nb) {
      cat(paste(c("\r", rep.int(" ", .nb * nw)), collapse = ""))
      cat(paste(c("\r", rep.int(char, nb)), collapse = ""))
      flush.console()
    }
    .nb <<- nb
  }
  up2 <- function(value) {
    if (!is.finite(value) || value < min || value > max) 
      return()
    .val <<- value
    nb <- round(width * (value - min)/(max - min))
    if (.nb <= nb) {
      cat(paste("\r", rep.int(char, nb), collapse = ""))
      flush.console()
    }
    else {
      cat(paste(c("\r", rep.int(" ", .nb * nw)), collapse = ""))
      cat(paste(c("\r", rep.int(char, nb)), collapse = ""))
      flush.console()
    }
    .nb <<- nb
  }
  up3 <- function(value) {
    if (!is.finite(value) || value < min || value > max) 
      return()
    .val <<- value
    nb <- round(width * (value - min)/(max - min))
    pc <- round(100 * (value - min)/(max - min))
    if (nb == .nb && pc == .pc) 
      return()
    cat(paste(c("\r  |", rep.int(" ", nw * width + 6)), collapse = ""))
    cat(paste(c("\r  |", rep.int(char, nb), rep.int(" ", 
                                                    nw * (width - nb)), sprintf("| %3d%%", pc)), collapse = ""))
    if(nchar(label))cat(" : ",label,sep="")
    flush.console()
    .nb <<- nb
    .pc <<- pc
  }
  lab <- function(label){label<<-label}
  getVal <- function() .val
  kill <- function() if (!.killed) {
    cat("\n")
    flush.console()
    .killed <<- TRUE
  }
  up <- switch(style, up1, up2, up3)
  if (initial > min) 
    up(initial)
  structure(list(getVal = getVal, up = up, kill = kill, label=lab), class = "txtProgressBar")
}


setTxtProgressBar2 <- function (pb, value, title = NULL, label = NULL) 
{
  if (!inherits(pb, "txtProgressBar")) 
    stop("'pb' is not from class \"txtProgressBar\"")
  oldval <- pb$getVal()
  if(!is.null(label)){
    pb$label(label)
  }
  pb$up(value)
  invisible(oldval)
}

# Little tool to make parallel stuff a bit more efficient
goparallel = function(cores=6,stop=F){
  if(!stop){
    cat("Starting parallel cluster\n")
    settings$cl <<- makeCluster(cores)
    registerDoParallel(settings$cl)
    settings$parallel <<-T
  } else{
    cat("Stopping parallel cluster\n")
    stopCluster(cl = settings$cl)
    settings$parallel <<-F
  }
}


fixnumber = function(x) {
  val= strsplit(x,",")[[1]];
  
  if (suppressWarnings(!is.na(as.numeric(val[1])))){
    len=length(val); 
    cor=switch(nchar(val[len]),"1"=10,"2"=100,"3"=1000)
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
  return(as.numeric(a))
}

invwhich = function(indices, totlength) {
  is.element(seq_len(totlength), indices)
}

onlynum = function(ll){
  as.numeric(sapply(strsplit(ll, "[^0-9]+"),function(x) x[2]))
}

adrsplit = function (Adres){
  
  as.integer(laply(strsplit(Adres," "),function(x) ifelse(length(x)>1,x[[2]],NA)))
}


firstFri = function(initialdate)
{
# Determine first sunday following first friday for nor creation date -------------------------------

#   Aproximate date of NOR generation, first friday + 2 days
date = as.Date(paste0(initialdate,"01"), "%y%m%d")
dow = sapply(seq(0,6),function(x) wday(date+days(x)))
firstFriday = date + days(which(dow==5)-1)+2
return(firstFriday)
}

#Check whether old names are in the colnames of a datatable, if true then change this name
namechange <- function(datatable,oldnames,newnames){
  if(length(oldnames)!=length(newnames)){print("Unequal length of name vectors")}else{
    lengthnames = length(oldnames)
    for(i in 1:lengthnames){
      if(sum(grepl(oldnames[i],names(datatable)))){
        setnames(datatable,oldnames[i],newnames[i])
      }
    }
  }
}