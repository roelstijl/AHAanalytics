AHA_Data_Import= function(folder="automatic",dataname,headername,mode="save",override="no"){
  
  # Asset health analytics import script, 
  # Load project first
  # (c) Roel Stijl (Bearingpoint), Jacco Heres (Alliander), 2014
  
  # This file imports select collumns from raw files and modifies the headers based on an xlsx file
  # The xlsx file is <filename>_header.xlsx, and is automatically created importing a CSV
  # Default for the xlsx file is all collumns and default headers, can be modified using shiny GUI or excel
  # The functions operates in several modes these can be be shiny, save or load
  # - shiny provides a GUI for editing the header files
  # - load provides a mode to load a file to workspace (e.g. output = AHA_impot(a,b,c))
  # - save will save the data to file <filename>.Rda
  # - header will just dump the header xlsx
  # Multiple files is supported, initial input dataname is a partial search
  
  # Do all the loading and modifying of files -------------------------------
  # Define the location of your data based on the system used

  if (folder == "automatic"){
    filechooser= (file.choose());
    datafiles  = basename(filechooser)
    folder     = strsplit(tail(strsplit(dirname(filechooser),"/")[[1]],1)," ")[[1]][2]
    curdataname = substring(datafiles[[1]],1,nchar(datafiles[[1]])-4)
    setfolder  = tail(strsplit(dirname(filechooser),"/")[[1]],1)
    headerfile = paste0(settings$Ruwe_Datasets,"/",
                        setfolder,"/",
                        substring(datafiles[[1]],1,nchar(datafiles[[1]])-4),"_headers.xlsx")
  } 
  else{
    setfolder     = list.files(settings$Bron_Datasets,pattern=folder)[1]; 
    datafiles     = list.files(paste0(settings$Bron_Datasets,"/",setfolder),pattern=paste0(dataname,".{0,}\\.csv|",dataname,".{0,}\\.ssv|",dataname,".{0,}\\.tsv|",dataname,".{0,}\\.xlsx|",dataname,".{0,}\\.shp"))
    headerfile    = paste0(settings$Ruwe_Datasets, "/", setfolder,"/",headername,"_headers.xlsx")
  }
  dir.create(paste0(settings$Ruwe_Datasets,"/",setfolder), showWarnings = FALSE)
  
  shinyfolder   = "x. Shiny"
  pb = tkProgressBar (title = "Import", label = "Starting...", min = 0, max = length(datafiles)*3, initial = 0, width = 450); pc=0;
  
  # Import data and rename cols
  for (filenumber in 1:length(datafiles))
  {
    sourcefile  = paste0(settings$Bron_Datasets,"/",setfolder,"/",datafiles[filenumber])   
    curdataname = substring(datafiles[filenumber],1,nchar(datafiles[filenumber])-4);
    curdataext  = substring(datafiles[filenumber],nchar(datafiles[filenumber])-2,nchar(datafiles[filenumber]));
    
    setTkProgressBar (pb, pc, title = paste0("AHA_Data_Import,file: ",datafiles[filenumber]), label = "Starting import"); pc=pc+1
    
    # Choose the correct import method
    if(mode!="header"){colclass=rep("character",1)} else {colclass = switch (override,yes=NA,no=NULL)}
    mindataset  = switch (curdataext,
                          csv = {if(folder =="NOR" & !any(pmatch(paste0("ELCVERBINDINGEN_140",1:8), curdataname,dup = TRUE,nomatch=0)>0) & any(pmatch("ELCVERBINDINGEN",curdataname,dup = TRUE,nomatch=0)>0))
                          {mindataset = data.frame(read.csv(sourcefile,row.names=NULL,colClasses=colclass));
                           names(mindataset)[1:(length(names(mindataset))-1)]= names(mindataset)[2:length(names(mindataset))]; 
                           names(mindataset)[(length(names(mindataset)))]="DUPLICATE";
                           mindataset}
                          else 
                          {data.frame(fread(sourcefile,sep=",",header=TRUE,colClasses=colclass))}},
                          
                          tsv = {switch(override,
                                        no=data.frame(fread(sourcefile,header=TRUE,sep="\t",colClasses=colclass)),
                                        yes=data.frame(read.csv(sourcefile,sep="\t",colClasses=colclass)))},
                          
                          ldr = {data.frame(importldr(sourcefile,colclass))},
                          
                          ssv = {switch(override,
                                        no=data.frame(fread(sourcefile,header=TRUE,sep=";",colClasses=colclass)),
                                        yes=data.frame(read.csv(sourcefile,header=TRUE,sep=";",colClasses=colclass)))},
                          
                          lsx= {data.frame(read.xlsx(sourcefile,1))}
    )
    
    
    # Convert header into the same format as the xlsx file --------------------------------
    #     cat("converting header. \n");
    setTkProgressBar (pb, pc, label = paste0("Converting header")); pc=pc+1
    
    header       = data.frame(matrix(0,length(colnames(mindataset)),4))
    header[,1]   = data.frame(colnames(mindataset))
    header[,2]   = (colnames(mindataset))
    header[,3]   = matrix(0,length(colnames(mindataset)))
    header[,4]   = matrix("comment",length(colnames(mindataset)))
    header[,5]   = sapply(mindataset, class,simplify=TRUE);
    colnames(header) = c(curdataname,"Original name","Meenemen","Notities","Class")
    
    # Load the xlsx file or create it if non existing
    if (file.exists(headerfile)) {
      savedheader   = read.xlsx(headerfile,1, as.data.frame=TRUE)
    }else{ cat("Creating new header file.\n")
           header[,3]   = matrix(1,length(colnames(mindataset)))
           write.xlsx(header,file=headerfile,row.names=FALSE)
           savedheader  = header}
    
    # Check if the headers saved and actual are equal and correct where not
    pat    = pmatch(header[,2], savedheader[,2], dup = TRUE,nomatch=0)
    header[pat>0,] = savedheader[pat[pat>0],]
    setnames(mindataset,colnames(mindataset), t(header[1]))
    
# Set colclasses to the desired (excel sheet)
for(i in header[header[,5]=="numeric",1]) {mindataset[,i] = as.numeric(gsub(",",".",mindataset[,i]))}
for(i in header[header[,5]=="date",1])    {mindataset[,i] = dmy(mindataset[,i])} #Timezone note taken into account for perforamnce
for(i in header[header[,5]=="dateymd",1]) {mindataset[,i] = ymd(mindataset[,i])} #Timezone note taken into account for perforamnce
for(i in header[header[,5]=="datetime",1]){mindataset[,i] = as.Date(my_hms(mindataset[,i]))}
for(i in header[header[,5]=="datetimeYDM",1]){mindataset[,i] = as.Date(ymd_hms(mindataset[,i]))}
for(i in header[header[,5]=="datetimeM",1]){mindataset[,i] = as.Date(dmy_hm(mindataset[,i]))}
for(i in header[header[,5]=="integer",1]) {mindataset[,i] = as.integer(mindataset[,i])}

    
# Choose what output to generate depending on user selection ---------------------------
if(mode=="shiny"){
  
  # Shiny visualisation
  shinyfolder  = "x. Shiny"
  dataset   <<- mindataset[sample(nrow(mindataset),min(nrow(mindataset),10000)),]
  remove ("mindataset")
  header    <<- header  
  cat("Starting shiny .\n")  
  header = runApp(shinyfolder)
  cat("Closing shiny .\n")
  file.rename(paste0(shinyfolder ,"/header.xlsx"),paste0(shinyfolder,"/",headerfile));
  
  cat("Copy file\n"); 
  file.copy(paste0(shinyfolder,"/",headerfile),paste0(settings$Ruwe_Datasets, "/", setfolder,"/",headerfile),overwrite=TRUE);      
  cat("Done\n")} 
else if(mode=="load") {
  
  # Load to memory
  cat("Done\n");    return(mindataset[,header[header[,3]==1,1]])
} else if(mode=="save") {
  setTkProgressBar (pb, pc, label = paste0("Saving to file, rows: ", nrow(mindataset)," cols: ",ncol(mindataset)) ); pc=pc+1
  
  dataclasses= sapply(mindataset, class)
  mindataset = data.table(mindataset[,header[header[,3]==1,1]])
  setkeyv(mindataset, colnames(mindataset)[1])
  
  # Save to file
  savefile = paste0(settings$Ruwe_Datasets, "/", setfolder,"/",curdataname,".Rda")
  save(mindataset,dataclasses,file=savefile)
  setTkProgressBar (pb, pc, title = paste0("AHA_Data_Import,file: ",datafiles[filenumber]), label = "Done!");
  
}else if(mode=="header"){
  cat("Saved header file to xlsx\n")
}else{
  cat("Wrong mode selected, load, save or shiny\n")
}  
}
}

cNA = function(dataset)
{
  par(mfrow=c(1, 1), mar=c(2, 15, 0, 2))
  barplot(t(cbind(sapply(dataset,function(x) sum(is.na(x))),
                  sapply(dataset,function(x) sum(!is.na(x))))), horiz=TRUE,las=1,cex.names=0.5)
}

row.sample <- function(dta, rep = 20) {
  dta <- as.data.frame(dta) # for single variables
  dta[sample(1:nrow(dta), rep, replace=FALSE), ] 
} 

importldr = function(sourcefile,colclass){ 
  a = readLines(sourcefile)
  b = paste(a, collapse = "")
  c = gsub("\\{EOL\\}","\n",b)
  d = fread(c,header=TRUE,colClasses=colclass)
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

pbarwrapper = function(title="PlaceHolder", label = "Starting...", max = noloops)
  {
  pb        = tkProgressBar(title = title, label = label, min = 0, max = max, initial = 1, width = 450)
  return(pb)
}

setRrogressBar = function(pb){
setTkProgressBar (pb, loopy,label = "Converting to geospatial output");
}
