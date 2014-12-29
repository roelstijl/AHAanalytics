AHA_Data_NOR_Log = function(NORtable, source="file")
  # Used to derive the monthly change version of the NOR using first month as a basis
  # Source can be backup or file
{
  # Load functions and settings ----------------------------------------
  #   source = "file"
  #   NORtable = "ELCVERBINDINGSDELEN"
  datafolder    = paste0(settings$Ruwe_Datasets,"/6. NOR");
  outputfolder  = paste0(settings$Input_Datasets,"/6. NOR");
  firstfile = 1
  
  par(mfrow=c(1,1))
  files = list.files(pattern=paste0(NORtable,".*\\.Rda"), path=datafolder,full.names=TRUE)
  filesshort = list.files(pattern=paste0(NORtable,".*\\.Rda"), path=datafolder)
  files=files[!grepl("masterdataset_backup",filesshort)]
  filesshort=filesshort[!grepl("masterdataset_backup",filesshort)]
  
  pb = tkProgressBar(title = "AHA_Data_NOR_Log start", label = "Start", min = 0, max = length(filesshort)*3-1, initial = 0, width = 450); pc=0;
  
  # Select which collumns to compare
  comparecols = switch (NORtable,ELCVERBINDINGSKNOOPPUNTEN=c("ID_unique","ID_NAN","Bronsysteem","Spanningsniveau", "Soort",  "Constructie",	"Isolatiemedium",	"Fabrikant"),
                        ELCVERBINDINGSDELEN=c("ID_unique","Lengte","Bronsysteem","ID_NAN","Status","Geleidermateriaal","Spanningsniveau","Diameter","Netverbinding"),
                        ELCVERBINDINGEN=c("ID_unique","Beheerder","Lengte", "Bronsysteem",	"SpanningsNiveau",	"Soort",	"Soortnet"),
                        cat("Please add headers to compute\n\n"))
  plot(file.info(files)$size)
  
  if (source == "backup") {
    backups = list.files(pattern=paste0("masterdataset_backup",".*\\.Rda"), path=paste0(outputfolder,"/backup"),full.names=TRUE);
    print(backups)
    filenumber <- readline(prompt="Select a backup file: ")
    load(paste0(backups[as.numeric(filenumber)]))
    print(filesshort)
    firstfile <- readline(prompt= "Continue from what file?: "); firstfile = as.numeric(firstfile)-1
  };
  
  for (n in firstfile:length(files))
  {
    
    # Load some data -------------------------------------
    curdate = firstFri(gsub("[^0-9]","",filesshort[n]));
    
    setTkProgressBar(pb, pc, title = paste0("AHA_Data_NOR_Log, file: ",filesshort[n]), label = "Starting import"); 
    setTkProgressBar(pb, pc, label = "Starting import"); pc=pc+1
    
    load(files[n]) 
    
    # Prepare the data set
    if(NORtable == "ELCVERBINDINGSDELEN" & class(mindataset$Lengte)=="character")  
    {cat("Correcting character lengths \n")
     mindataset$Lengte = as.numeric(sapply(mindataset$Lengte,fixnumber))}
    
    # Define the unique ID composition
    ID_unique = switch (NORtable,
                        ELCVERBINDINGSDELEN       = mindataset[,ID_unique:=paste0(ID_Kabel,PC_6_van)],
                        ELCVERBINDINGEN           = mindataset[,ID_unique:=paste0(ID_Verbinding,ID_Hoofdleiding)],
                        ELCVERBINDINGSKNOOPPUNTEN = mindataset[,ID_unique:=paste0(ID_Bron,PC_6)],
                        cat("Please add headers to compute\n\n"))
    setkey(mindataset,ID_unique) 
    
    mindataset = unique(mindataset)
    
    mindataset$file         = n;  
    mindataset$DateAdded    = curdate; 
    mindataset$DateRemoved  = as.Date(NA); 
    mindataset$Status_ID    = "Active"
    
    if(n!=1) {if(any(!(colnames(masterdataset) %in% colnames(mindataset))))
    {set(mindataset,,colnames(masterdataset)[!(colnames(masterdataset) %in% colnames(mindataset))],(NA))}}
    
    # Create the NAN number or Verbindingen if not present already
    if(!any(colnames(mindataset)=="ID_NAN")){mindataset$ID_NAN=as.character(NA)}
    if(!any(colnames(mindataset)=="Bronsysteem")){mindataset$Bronsysteem=as.character(NA)}
    
    if(!any(colnames(mindataset)=="ID_Verbinding")) 
    {switch (NORtable,
             ELCVERBINDINGSKNOOPPUNTEN={mindataset$ID_Verbinding=as.character(NA)}
    )}
    
    
    # Create the master dataset from the first file ---------------------------
    if (n<=firstfile){
      if (source=="file"){                  
        # Load some variables for later
        masterdataset = mindataset
      }
      par(mfrow=c(2,1))  
    }
    # Calculate the difference between the master dataset and each file -------
    if(n>firstfile){
      # Convert to data table for speed
      mindataset = mindataset[,colnames(masterdataset),with=FALSE]
      
      # Check which IDs have been removed
      setTkProgressBar(pb, pc, label = "Checking Added Removed"); pc=pc+1
      
      Added    = !(mindataset$ID_unique %in% masterdataset$ID_unique)
      Removed  = !(masterdataset$ID_unique %in% mindataset$ID_unique)
      
      # Merge the old and new IDs for comparison    
      combinedset  = rbind(masterdataset[which(!Removed),comparecols,with=FALSE],mindataset[which(!Added),comparecols,with=FALSE])
      differences = !(duplicated(combinedset,by=comparecols) | duplicated(combinedset,by=comparecols,fromLast=TRUE))
      
      
      if (!exists("changes")){
        changes = combinedset[differences];  
        changes[,Date:=curdate]
      } else {
        temp    = combinedset[differences]
        temp[,Date:=curdate]
        changes = rbind(changes, temp)
      }
      
      # Write the result in a changelog    
      setTkProgressBar(pb, pc, label = "Writing added removed"); pc=pc+1
      updatedmstr = logical(nrow(masterdataset));
      updatedmstr[!Removed]= differences[1:sum(!Removed)];
      updatedmind = logical(nrow(mindataset)); 
      updatedmind[!Added]  = differences[(sum(!Removed)+1):(sum(!Removed)+sum(!Added))]
      
      set(masterdataset,which(updatedmstr),comparecols,mindataset[updatedmind,comparecols,with=FALSE])   
      
      # Apply the removed sets
      set(masterdataset,which(Removed & is.na(masterdataset$DateRemoved)),"DateRemoved",curdate)
      set(masterdataset,which(Removed),"Status_ID","Removed")
      masterdataset = rbind(masterdataset,mindataset[which(Added),])
      
      # Save backups every 6 cycles
      if (n%%6 == 0) {
        cat("Saving backup\n"); 
        setTkProgressBar(pb, pc, label = "Saving backup");
        save(masterdataset,changes,dataclasses,file=paste0(outputfolder,"/backup/masterdataset_backup_",filesshort[n]));   

        setTkProgressBar(pb, pc, label = "Plotting");
        try({barplot(rbind(table(masterdataset$DateRemoved)[2:n],table(masterdataset$DateAdded)[2:n]),beside=TRUE);  
        barplot(table(changes$Date))}
        )}
    }
  }
  cat("Finished!! Saving to file\n"); save(changes,file=paste0(outputfolder,"/changes_",NORtable,".Rda"))   
  save(masterdataset,file=paste0(outputfolder,"/masterdataset_",NORtable,".Rda"))   
  
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

firstFri = function(initialdate)
{
  #   Aproximate date of NOR generation, first friday + 2 days
  date = as.Date(paste0(initialdate,"01"), "%y%m%d")
  dow = sapply(seq(0,6),function(x) wday(date+days(x)))
  firstFriday = date + days(which(dow==5)-1)+2
  return(firstFriday)
}

