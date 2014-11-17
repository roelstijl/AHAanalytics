AHA_Data_NOR_Log = function(NORtable, source="file")
  # Used to derive the monthly change version of the NOR using first month as a basis
  # Source can be backup or file
{
# Load functions and settings ----------------------------------------

  #   source = "file"
  #   NORtable = "ELCVERBINDINGSDELEN"
  datafolder    = paste0(settings$Ruwe_Datasets,"/6. NOR");
  outputfolder  = paste0(settings$Input_Datasets,"/6. NOR");
  
  par(mfrow=c(1,1))

  files = list.files(pattern=paste0(NORtable,".*\\.Rda"), path=datafolder,full.names=TRUE)
  filesshort = list.files(pattern=paste0(NORtable,".*\\.Rda"), path=datafolder)
  files=files[!grepl("masterdataset_backup",filesshort)]
  filesshort=filesshort[!grepl("masterdataset_backup",filesshort)]
  
  # Select which collumns to use for the unique identifiers
  ID_unique = switch (NORtable, ELCVERBINDINGSKNOOPPUNTEN=c("ID_Bron","PC_XY"),
                        ELCVERBINDINGSDELEN=c("ID_Kabel","PC_XY_van"),
                        ELCVERBINDINGEN=c("ID_Verbinding","ID_Hoofdleiding"),
                        cat("Please add headers to compute\n\n"))

  # Select which collumns to compare
  comparecols = switch (NORtable,ELCVERBINDINGSKNOOPPUNTEN=c("ID_unique","ID_NAN","Spanningsniveau", "Soort",	"Constructie",	"Isolatiemedium",	"Fabrikant"),
                        ELCVERBINDINGSDELEN=c("ID_unique","Lengte","ID_NAN","Status","Geleidermateriaal","Spanningsniveau","Diameter","Netverbinding"),
                        ELCVERBINDINGEN=c("ID_unique","Beheerder","Lengte", "ID_Hoofdleiding",	"SpanningsNiveau",	"SOORT",	"SOORTNET"),
                        cat("Please add headers to compute\n\n"))
  plot(file.info(files)$size)

# Create the master dataset from the first file ---------------------------
  cat("Loading master set\n");tic()
  
  switch(source,
         file={
           
           #Load stuff
           load(files[1])
           
           # Add some stuff
           mindataset$file = 1; 
           mindataset$DateAdded = "0701"; 
           mindataset$DateRemoved = ""
           mindataset$Status_ID = "Active"
           
           # Create the NAN number or Verbindingen if not present already
           if(!any(colnames(mindataset)=="ID_NAN")) 
           {switch (NORtable,
                    ELCVERBINDINGSKNOOPPUNTEN={mindataset$ID_NAN=NA},
                    ELCVERBINDINGSDELEN      ={mindataset$ID_NAN=NA})}
           
           if(!any(colnames(mindataset)=="ID_Verbinding")) 
           {switch (NORtable,
                    ELCVERBINDINGSKNOOPPUNTEN={mindataset$ID_Verbinding=NA}
           )}
           
           
           # Prep data table
           mindataset$ID_unique    = 
             paste0(as.character(mindataset[,ID_unique[1]]),
                    as.character(mindataset[,ID_unique[2]]))
           mindataset = data.table(mindataset)
           setkey(mindataset,ID_unique)
           
           # Initiate master set
           masterdataset = unique(mindataset)
           firstfile = 2     
           
           # Load some variables for later
           dataclasses = as.data.frame(t(as.data.frame(sapply(masterdataset, class))))
           colnames(dataclasses)= colnames(masterdataset)
           changes     = data.table(matrix(1,0,length(comparecols)+1));setnames(changes,c(comparecols,"Date"))
           
         },
         
         # Or load from a backup
         backup ={
           backups = list.files(pattern=paste0("masterdataset_backup",".*\\.Rda"), path=outputfolder,full.names=TRUE);
           print(backups)
           filenumber <- readline(prompt="Select a backup file: ")
           load(paste0(backups[as.numeric(filenumber)]))
           print(filesshort)
           firstfile <- readline(prompt= "Continue from what file?: "); firstfile = as.numeric(firstfile)
         });
  toc();par(mfrow=c(2,1))  

# Calculate the difference between the master dataset and each file -------

  for (n in firstfile:length(files))
  {
    # Load some data
    toc(); curdate = gsub("[^0-9]","",filesshort[n]);cat(paste0("Starting import of dataset: ",filesshort[n],"\n"));tic()
    load(files[n]) 

    # Prepare the data set
    if(NORtable == "ELCVERBINDINGSDELEN" & class(mindataset$Lengte)=="character")  {  cat("Correctig character lengths \n")
      mindataset$Lengte = as.numeric(sapply(mindataset$Lengte,fixnumber))}
    toc();cat("Preparing sets\n");tic()
    mindataset$ID_unique    = 
      paste0(as.character(mindataset[,ID_unique[1]]),
             as.character(mindataset[,ID_unique[2]]))
    mindataset$file = n;  mindataset$DateAdded = curdate; mindataset$DateRemoved = ""; mindataset$Status_ID = "Active"
    if(any(!(colnames(masterdataset) %in% colnames(mindataset)))){mindataset[,colnames(masterdataset)[!(colnames(masterdataset) %in% colnames(mindataset))]]=NA}
    
    # Create the NAN number or Verbindingen if not present already
    if(!any(colnames(mindataset)=="ID_NAN")) 
    {switch (NORtable,
             ELCVERBINDINGSKNOOPPUNTEN={mindataset$ID_NAN=NA},
             ELCVERBINDINGSDELEN      ={mindataset$ID_NAN=NA})}
    
    if(!any(colnames(mindataset)=="ID_Verbinding")) 
    {switch (NORtable,
             ELCVERBINDINGSKNOOPPUNTEN={mindataset$ID_Verbinding=NA}
    )}
    
    # Convert to data table for speed
    toc(); cat("Converting to data table\n"); tic()    
    mindataset = data.table(mindataset[,colnames(masterdataset)])
    setkey(mindataset,ID_unique) 
    mindataset = unique(mindataset)
    
    toc(); cat("Calculating classes\n"); tic()
    dataclasses = rbind(dataclasses,as.data.frame(t(as.data.frame(sapply(mindataset[,colnames(masterdataset)], class)))));
    row.names(dataclasses) <- NULL 

    # Check which IDs have been removed
    toc(); cat("Checking added and removed\n"); tic()    
    Added    = !(mindataset$ID_unique %in% masterdataset$ID_unique)
    Removed  = !(masterdataset$ID_unique %in% mindataset$ID_unique)

    # Merge the old and new IDs for comparison
    toc();cat("Starting comparison of sets (duplicated)\n");tic()
    
    combinedset  = rbind(masterdataset[which(!Removed),comparecols,with=FALSE],mindataset[which(!Added),comparecols,with=FALSE])
    differences = !(duplicated(combinedset) | duplicated(combinedset,fromLast=TRUE))
    
    changed = combinedset[differences]
    set(changed,i=NULL,"Date",curdate)
    changes = rbind(changes, changed)
    
    # Write the result in a changelog    
    toc(); cat("Correcting to new values\n"); tic()
    updatedmstr = 1:nrow(masterdataset)==0; updatedmstr[!Removed]= differences[1:sum(!Removed)]
    updatedmind = 1:nrow(mindataset)==0   ; updatedmind[!Added]  =differences[(sum(!Removed)+1):(sum(!Removed)+sum(!Added))]
    set(masterdataset,which(updatedmstr),comparecols,mindataset[which(updatedmind),comparecols,with=FALSE])   
    setkey(masterdataset,ID_unique)  
    
    # Apply the removed sets
    set(masterdataset,which(Removed & masterdataset$DateRemoved==""),"DateRemoved",curdate)
    set(masterdataset,which(Removed),"Status_ID","Removed")
    masterdataset = rbind(masterdataset,mindataset[which(Added),])
    setkey(masterdataset,ID_unique) 
    
    # Save backups every 6 cycles
    if (n%%6 == 0) {
    cat("Saving backup\n");tic(); 
    save(masterdataset,changes,dataclasses,file=paste0(outputfolder,"/masterdataset_backup_",filesshort[n])); toc()    
    cat("Plotting\n"); tic()
    barplot(rbind(table(masterdataset$DateRemoved)[2:n],table(masterdataset$DateAdded)[2:n]),beside=TRUE);  
    barplot(table(changes$Date))
    
    toc()}
  }
  cat("Finished!! Saving to files\n"); save(masterdataset,changes,dataclasses,file=paste0(outputfolder,"/masterdataset_",NORtable,".Rda"))   
}

###########################################################################################################################################

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
  #cat(paste0(a,", "))
  return(as.numeric(a))
}

invwhich = function(indices, totlength) is.element(seq_len(totlength), indices)
