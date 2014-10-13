AHA_diffNOR = function(NORtable, source)
  # Used to derive the monthly change version of the NOR using first month as a basis
{
  library(compare)
  library(pracma)
  library(foreach)
#   source = "file"
#   NORtable = "ELCVERBINDINGSDELEN"
  datafolder    = paste0(settings$Ruwe_Datasets,"/6. NOR");
  outputfolder  = paste0(settings$Input_Datasets,"/6. NOR");
  
  par(mfrow=c(2,1))
  

  files = list.files(pattern=paste0(NORtable,".*\\.Rda"), path=datafolder,full.names=TRUE)
  filesshort = list.files(pattern=paste0(NORtable,".*\\.Rda"), path=datafolder)
  files=files[!grepl("masterset_backup",filesshort)]
  filesshort=filesshort[!grepl("masterset_backup",filesshort)]
  
  # Select which collumns to use for the unique identifiers
  uniqueidcols = switch (NORtable,
                        ELCVERBINDINGSKNOOPPUNTEN=c("ID_Bron","PC_XY"),
                        ELCVERBINDINGSDELEN=c("ID_Kabel","PC_XY_van"),
                        ELCVERBINDINGEN=c("ID_Verbinding","ID_Hoofdleiding"),
                        cat("Please add headers to compute\n\n"))

  # Select which collumns to compare
  comparecols = switch (NORtable,
                        ELCVERBINDINGSKNOOPPUNTEN=c("ID_unique","Spanningsniveau", "Soort",	"Constructie",	"Isolatiemedium",	"Fabrikant"),
                        ELCVERBINDINGSDELEN=c("ID_unique","Lengte","Status","Geleidermateriaal","Spanningsniveau","Diameter","Netverbinding"),
                        ELCVERBINDINGEN=c("ID_unique","Beheerder", "ID_Hoofdleiding",	"SpanningsNiveau",	"SOORT",	"SOORTNET"),
                        cat("Please add headers to compute\n\n"))
  dataset = list()
  plot(file.info(files)$size)
  
  cat("Loading master set\n");tic()
  switch(source,
         # Create the master dataset from the first file
         file={
           load(files[1])
           info = data.frame()  
           mindataset$file = 1; 
           mindataset$DateAdded = "0701"; 
           mindataset$DateRemoved = ""
           mindataset$Status_ID = "Active"
           # Create the NAN number
           if(!any(colnames(mindataset)=="ID_NAN")) 
             {switch (NORtable,
             ELCVERBINDINGSKNOOPPUNTEN={mindataset$ID_NAN=NA},
             ELCVERBINDINGSDELEN      ={mindataset$ID_NAN=NA})}
   
           masterset = mindataset[!duplicated(mindataset[c(uniqueidcols,"file")]),]
           masterset$ID_unique = as.character(1:length(masterset[,1]))
           firstfile = 2
                     
         },
         
         # Or load from a backup
         backup ={
           backups = list.files(pattern=paste0("masterset_backup",".*\\.Rda"), path=outputfolder,full.names=TRUE);
           print(backups)
           filenumber <- readline(prompt="Select a backup file: ")
           load(paste0(backups[as.numeric(filenumber)]))
           print(filesshort)
           firstfile <- readline(prompt= "Continue from what file?: "); firstfile = as.numeric(firstfile)
         });
  toc()
  
  # Load some variables for later
  dataclasses = as.data.frame(t(as.data.frame(sapply(masterset, class))))

  changes = data.frame(matrix(1,0,length(comparecols)+1));
  colnames(changes)=c(colnames(masterset[,comparecols]),"Date")
  changeslogical = changes;
  
  # Calculate the difference between the master dataset and each file
  
  for (n in firstfile:length(files))
  {
    tic()
    cat(paste0("Starting import of dataset: ",filesshort[n],"\n"));tic()
    load(files[n])  ;toc()
    
    # Prepare the master set
    cat("Preparing sets\n");tic()
    mindataset$file = n
    mindataset$DateAdded = gsub("[^0-9]","",filesshort[n])
    mindataset$DateRemoved = ""
    mindataset$Status_ID = "Active"
    mindataset = mindataset[!duplicated(mindataset[c(uniqueidcols,"file")]),]
    if(!any(colnames(mindataset)=="ID_NAN")) 
    {switch (NORtable,
       ELCVERBINDINGEN          ={if (n>84) {mindataset$Lengte = as.integer(sapply(mindataset$Lengte,fixnumber))}},
       ELCVERBINDINGSKNOOPPUNTEN={mindataset$ID_NAN=NA},
       ELCVERBINDINGSDELEN      ={mindataset$ID_NAN=NA})}

    # Check if there are any new unique IDs
    cat("Starting comparison of sets (merge)\n"); tic()
    mindataset = merge(mindataset, masterset[,c("ID_unique",uniqueidcols)], by = uniqueidcols, all.x = TRUE)
    if(any(!(colnames(masterset) %in% colnames(mindataset)))){
      mindataset[,colnames(masterset)[!(colnames(masterset) %in% colnames(mindataset))]]=NA
    }
    mindataset = mindataset[,colnames(masterset)]
    
    
    # Add the new IDs to the master set
    lastid   = length(masterset$ID_unique)
    newIDpos = is.na(mindataset$ID_unique)
    newIDs   = as.character( lastid+ (1:sum(is.na(mindataset$ID_unique))))
    mindataset[newIDpos,"ID_unique"] = newIDs    
    masterset = rbind(masterset,mindataset[newIDpos,])
    
    # Check which IDs have been removed
    IDsRemoved = masterset[!(masterset$ID_unique %in% mindataset$ID_unique),"ID_unique"]
    masterset[! (masterset$ID_unique %in% mindataset$ID_unique) & masterset$DateRemoved=="","DateRemoved"] = gsub("[^0-9]","",filesshort[n])
    masterset[! (masterset$ID_unique %in% mindataset$ID_unique) ,"Status_ID"] = "Removed"
  
    # Merge the old and new IDs for comparison
    toc();cat("Starting comparison of sets (duplicated)\n");tic()
    combinedset = rbind(masterset,mindataset) 
    datadups    = 
      !(duplicated(combinedset[,comparecols])|duplicated(combinedset[,comparecols], fromLast = TRUE)) & 
      combinedset$Status_ID !=  "Removed" &
      combinedset$DateAdded != gsub("[^0-9]","",filesshort[n])
    changedIDs  = (combinedset[datadups,"ID_unique"]) 
    toc()
    
    # Write the result in a changelog    
    cat("Writing to changelog\n"); tic()
    
    {changedset = combinedset[combinedset$ID_unique %in% changedIDs,comparecols]
    changedset$Date = gsub("[^0-9]","",filesshort[n])}
    
    changes = rbind(changes, changedset)
    changes = changes[order(changes$ID_unique),]
    
    masterset[match(changedIDs,masterset$ID_unique),] = mindataset[match(changedIDs,mindataset$ID_unique),]
        
    dataclasses =rbind(dataclasses,as.data.frame(t(as.data.frame(sapply(mindataset[,colnames(dataclasses)], class)))));
    row.names(dataclasses) <- NULL
    toc();  
    
    # Save backups every 6 cycles
  if (n%%6 == 0) {
    cat("Saving backup\n");  
    tic(); 
    save(masterset,changes,dataclasses,file=paste0(outputfolder,"/masterset_backup_",filesshort[n])); toc()    
    cat("Plotting\n"); tic()
    barplot(rbind(table(masterset[,"DateRemoved"])[2:n],table(masterset[,"DateAdded"])[2:n]),beside=TRUE);  
    legend("topleft",c("DateRemoved","DateAdded"))
    toc()}
    
  }

  cat("Saving backup\n"); save(masterset,changes,dataclasses,file=paste0(outputfolder,"/masterset_",NORtable,".Rda"))  
  
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

invwhich = function(indices, totlength) is.element(seq_len(totlength), indices)


#     pm = pmatch(colnames(masterset),colnames(mindataset), dup = TRUE,nomatch=0)
#     if (sum(grepl(0,pm))>0) mindataset[,colnames(masterset)[pm == 0]] = rep(rownames(mindataset),2)
#     
#     mindataset = mindataset[,colnames(masterset)]
#       
#     unique1 = !duplicated(masterset[c(comparecols,"file")]); u1 = sum(unique1);
#     unique2 = !duplicated(mindataset[c(comparecols,"file")]); u2 = sum(unique2);

#     if(class(mindataset$Lengte)=="integer" & class(masterset$Lengte)== "numeric") {
#       # Correct for when a file is integer but the set is numeric
#       temp = masterset[unique1,c(comparecols,"file")]
#       temp$Lengte = as.integer(temp$Lengte)
#       combinedset = rbind(temp[unique1,c(comparecols,"file")],mindataset[unique2,c(comparecols,"file")])
#     } else{
#       combinedset = rbind(masterset[unique1,c(comparecols,"file")],mindataset[unique2,c(comparecols,"file")])
#     }
#     
#     datadups = cbind(combinedset,duplicated(combinedset[,comparecols])|duplicated(combinedset[,comparecols], fromLast = TRUE))
#     colnames(datadups)[length(datadups)]="dup";
#     
#     #     datadups = cbind(combinedset, dup=dupsBetweenGroups(combinedset,"file"))
#     
#     masterset = rbind(masterset,mindataset[unique2,][!datadups$dup[(u1+1):(u1+u2)],])
#     masterset[1:u1,][!datadups$dup[1:u1]&masterset[1:u1,"DateRemoved"]=="","DateRemoved"]= gsub("[^0-9]","",filesshort[n])
#     
#   # Select which collumns have numbers in a character format (wrong read)
#   comparecols = switch (NORtable,
#                         ELCVERBINDINGSKNOOPPUNTEN="ID_Bron",
#                         ELCVERBINDINGSDELEN="Length",
#                         ELCVERBINDINGEN=c("ID_Verbinding","Lengte"),
#                         cat("Please add headers to compute\n\n"))