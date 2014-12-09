AHA_Data_Import= function(folder,dataname,headername,mode="save",override="no"){
  
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
  setfolder     = list.files(settings$Bron_Datasets,pattern=folder)[1]; dir.create(paste0(settings$Ruwe_Datasets,"/",setfolder), showWarnings = FALSE)
  datafiles     = list.files(paste0(settings$Bron_Datasets,"/",setfolder),pattern=paste0(dataname,".{0,}\\.csv|",dataname,".{0,}\\.ssv|",dataname,".{0,}\\.tsv|",dataname,".{0,}\\.xlsx|",dataname,".{0,}\\.shp"))
  headerfile    = paste0(headername,"_headers.xlsx");
  shinyfolder   = "x. Shiny"
  if (is.na(datafiles[1]) | is.na(setfolder[1])) stop("Wrong file, check file name")
  
  
  # Import data and rename cols
  for (filenumber in 1:length(datafiles))
  {
  curdataname = substring(datafiles[filenumber],1,nchar(datafiles[filenumber])-4);
  curdataext  = substring(datafiles[filenumber],nchar(datafiles[filenumber])-2,nchar(datafiles[filenumber]));
  cat(paste0("Start data-import of file : ",datafiles[filenumber]),".\n" ); tic()  
  
  # Choose the correct import method
  if(mode!="header"){colclass=rep("character",1)} else {colclass = switch (override,yes=NA,no=NULL)}
  mindataset  = switch (curdataext,
          csv = {if(folder =="NOR" & !any(pmatch(paste0("ELCVERBINDINGEN_140",1:8), curdataname,dup = TRUE,nomatch=0)>0) & any(pmatch("ELCVERBINDINGEN",curdataname,dup = TRUE,nomatch=0)>0))
                  {data.frame(read.csv(paste0(settings$Bron_Datasets,"/",setfolder,"/",datafiles[filenumber]),row.names=NULL,colClasses=colclass));names(mindataset)[1:(length(names(mindataset))-1)]= names(mindataset)[2:length(names(mindataset))]; names(mindataset)[(length(names(mindataset)))]="DUPLICATE"}
                  else {data.frame(fread(paste0(settings$Bron_Datasets, "/", setfolder,"/",datafiles[filenumber]),sep=",",colClasses=colclass))}},
          tsv = {switch(override,no=data.frame(fread(paste0(settings$Bron_Datasets, "/", setfolder,"/",datafiles[filenumber]),sep="\t",colClasses=colclass)),
                        yes=data.frame(read.csv(paste0(settings$Bron_Datasets, "/", setfolder,"/",datafiles[filenumber]),sep="\t",colClasses=colclass)))},
          ssv = {switch(override,no=data.frame(fread(paste0(settings$Bron_Datasets, "/", setfolder,"/",datafiles[filenumber]),sep=";",colClasses=colclass)),
                        yes=data.frame(read.csv(paste0(settings$Bron_Datasets, "/", setfolder,"/",datafiles[filenumber]),sep=";",colClasses=colclass)))},
          lsx= {data.frame(read.xlsx(paste0(settings$Bron_Datasets, "/", setfolder,"/",datafiles[filenumber]),1))}
         )

  
# Convert header into the same format as the xlsx file --------------------------------
    toc(); cat("converting header. \n"); tic();
    header       = data.frame(matrix(0,length(colnames(mindataset)),4))
    header[,1]   = data.frame(colnames(mindataset))
    header[,2]   = (colnames(mindataset))
    header[,3]   = matrix(0,length(colnames(mindataset)))
    header[,4]   = matrix("comment",length(colnames(mindataset)))
    header[,5]   = sapply(mindataset, class,simplify=TRUE);
    colnames(header) = c(curdataname,"Original name","Meenemen","Notities","Class")
  
  # Load the xlsx file or create it if non existing
  if (file.exists(paste0(settings$Ruwe_Datasets, "/", setfolder,"/",headerfile))) {
    savedheader   = read.xlsx(paste0(settings$Ruwe_Datasets, "/", setfolder,"/",headerfile),1, as.data.frame=TRUE)
  }else{ cat("Creating new header file.\n")
    header[,3]   = matrix(1,length(colnames(mindataset)))
    write.xlsx(header,file=paste0(settings$Ruwe_Datasets, "/", setfolder,"/",headerfile),row.names=FALSE)
    savedheader  = header}

  # Check if the headers saved and actual are equal and correct where not
  pat    = pmatch(header[,2], savedheader[,2], dup = TRUE,nomatch=0)
  header[pat>0,] = savedheader[pat[pat>0],]
  setnames(mindataset,colnames(mindataset), t(header[1]))

  # Set colclasses to the desired (excel sheet)
  for(i in header[header[,5]=="numeric",1]) {mindataset[,i] = as.numeric(mindataset[,i])}
  for(i in header[header[,5]=="date",1])    {mindataset[,i] = dmy(mindataset[,i])} #Timezone note taken into account for perforamnce
  for(i in header[header[,5]=="dateymd",1])    {mindataset[,i] = ymd(mindataset[,i])} #Timezone note taken into account for perforamnce
  for(i in header[header[,5]=="datetime",1]){mindataset[,i] = dmy_hms(mindataset[,i])}
  for(i in header[header[,5]=="datetimeM",1]){mindataset[,i] = dmy_hm(mindataset[,i])}
  for(i in header[header[,5]=="integer",1]) {mindataset[,i] = as.integer(mindataset[,i])}


# Choose what output to generate (last element of input) ---------------------------
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
    cat("Done\n") ;    return()} 
  else if(mode=="load") {
    
    # Load to memory
    cat("Done\n");    return(mindataset[,header[header[,3]==1,1]])
  } else if(mode=="save") {
    toc();cat(paste0("Saving to file, rows: ", nrow(mindataset)," cols: ",ncol(mindataset), "\n")); tic()
    dataclasses= sapply(mindataset, class)
    mindataset = data.table(mindataset[,header[header[,3]==1,1]])
    setkeyv(mindataset, colnames(mindataset)[1])
    
    save(mindataset,dataclasses,file=paste0(settings$Ruwe_Datasets, "/", setfolder,"/",curdataname,".Rda"))
    cat(paste0("Saved: ",paste0(settings$Ruwe_Datasets, "/", setfolder,"/",curdataname,".Rda"),"\n"));toc();
    }else if(mode=="header"){
    cat("Saved header file to xlsx\n")
    return()
    }else{
    cat("Wrong mode selected, load, save or shiny\n")
  }  
  }
}
