# Used to derive the monthly change version of the NOR using first month as a basis
# Source can be backup or file, backups will be created every 6 months unless backup=FALSE

AHA_Data_NOR_Log = function(NORtable, source="file",backups=TRUE){
  # Load functions and settings ----------------------------------------
  #   source = "file"
  #   NORtable = "ELCVERBINDINGSDELEN"
  datafolder    = paste0(settings$Ruwe_Datasets ,"/6. NOR");
  outputfolder  = paste0(settings$Input_Datasets,"/6. NOR");
  firstfile = 1
  
  # File settings ----------------------------------------------------------------
  par(mfrow=c(1,1))
  files = list.files(pattern=paste0(NORtable,".*\\.Rda"), path=datafolder,full.names=TRUE)
  filesshort = list.files(pattern=paste0(NORtable,".*\\.Rda"), path=datafolder)
  files=files[!grepl("masterdataset_backup",filesshort)]
  filesshort=filesshort[!grepl("masterdataset_backup",filesshort)]
  
  pb = pbarwrapper(title = "AHA_Data_NOR_Log start", label = "Start", min = 0, max = length(filesshort)*3+1+backups*length(filesshort)/3, initial = 0, width = 450); pc=0;
  
  # Select which collumns to compare
  comparecols = switch (NORtable,ELCVERBINDINGSKNOOPPUNTEN=c("ID_unique","ID_NAN","Bronsysteem","Spanningsniveau","Beheerder", "Soort",  "Constructie",  "Isolatiemedium",	"Fabrikant","Coo_X","Coo_Y"),
                        ELCVERBINDINGSDELEN=c("ID_unique","Lengte","Bronsysteem","ID_NAN","Status","Beheerder","Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar","Spanningsniveau","Diameter","Netverbinding"),
                        ELCVERBINDINGEN=c("ID_unique","Lengte", "Bronsysteem",	"SpanningsNiveau","Beheerder",	"Soort",	"Soortnet"),
                        cat("Please add headers to compute\n\n"))
  # Plot to check for anomolies in file sizes
  plot(file.info(files)$size)
  
  # Load from backup functions --------------------------------------------
  if (source == "backup") {
    #   backups = list.files(pattern=paste0("masterdataset_backup",".*\\.Rda"), path=paste0(outputfolder,"/backup"),full.names=TRUE);
    #   filenumber <- readline(prompt="Select a backup file: ")
    cat("Select backup file to continue from\n")
    bfile = file.choose()  
    cat("Select file to start import from\n")
    ffile = file.choose()  
    firstfile = which(filesshort==basename(ffile))
    
    ifelse (length(firstfile)==1,
            load(bfile),
            error("Wrong file selected"))
    
  };
  
  # Loop over the files to be imported importing them one at a time --------------------------
  for (n in firstfile:length(files))
  {
    # Determine date at which the file was created
    curdate = firstFri(gsub("[^0-9]","",filesshort[n]));
    setpbarwrapper(pb,  title = paste0("AHA_Data_NOR_Log, file: ",filesshort[n]), label = "Starting import"); 
    
    # Load some data 
    load(files[n]) 
    
    # Prepare the data set
    if(NORtable == "ELCVERBINDINGSDELEN" & class(mindataset$Lengte)=="character")  
    {cat("Correcting character lengths \n")
     mindataset$Lengte = as.numeric(sapply(mindataset$Lengte,fixnumber))}
    
    # Define the unique ID composition
    ID_unique = switch (NORtable,
                        ELCVERBINDINGSDELEN       = mindataset[,ID_unique:=paste0(ID_Kabel,substr(PC_6_van,1,2))],
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
        masterdataset = mindataset}
      par(mfrow=c(2,1))  # For double plotting
    }
    # Calculate the difference between the master dataset and each file -----------
    if(n>firstfile){
      # Convert to data table for speed
      mindataset = mindataset[,colnames(masterdataset),with=FALSE]
      
      # Check which IDs have been removed and which added
      setpbarwrapper(pb,  label = "Checking Added Removed"); 
      
      Added    = !(mindataset$ID_unique %in% masterdataset$ID_unique)
      Removed  = !(masterdataset$ID_unique %in% mindataset$ID_unique)
      
      # Merge the old and new IDs for comparison    
      combinedset  = rbind(masterdataset[which(!Removed),comparecols,with=FALSE],mindataset[which(!Added),comparecols,with=FALSE])
      differences = !(duplicated(combinedset,by=comparecols) | duplicated(combinedset,by=comparecols,fromLast=TRUE))
      
      # Collect the changes in a data table
      if (!exists("changes")){
        changes = combinedset[differences];  
        changes[,Date:=curdate]
      } else {
        temp    = combinedset[differences]
        temp[,Date:=curdate]
        changes = rbind(changes, temp)
      }
      
      # Write the result in a changelog    
      setpbarwrapper(pb, label = "Writing added removed");
      updatedmstr = logical(nrow(masterdataset));
      updatedmstr[!Removed]= differences[1:sum(!Removed)];
      updatedmind = logical(nrow(mindataset)); 
      updatedmind[!Added]  = differences[(sum(!Removed)+1):(sum(!Removed)+sum(!Added))]
      
      set(masterdataset,which(updatedmstr),comparecols,mindataset[updatedmind,comparecols,with=FALSE])   
      
      # Apply the removed sets
      set(masterdataset,which(Removed & is.na(masterdataset$DateRemoved)),"DateRemoved",curdate)
      set(masterdataset,which(Removed),"Status_ID","Removed")
      masterdataset = rbind(masterdataset,mindataset[which(Added),])
      
      # Save backups every 6 cycles if on
      if (n%%6 == 0 & backups) {
        setpbarwrapper(pb,  label = "Saving backup");
        save(changes,dataclasses,file=paste0(outputfolder,"/backup/masterdataset_backup_changes_",filesshort[n]));   
        save(masterdataset,file=paste0(outputfolder,"/backup/masterdataset_backup_masterdataset_",filesshort[n]));   
        
        setpbarwrapper(pb,  label = "Plotting");
        try({barplot(rbind(table(masterdataset$DateRemoved)[2:n],table(masterdataset$DateAdded)[2:n]),beside=TRUE);  
             barplot(table(changes$Date))}
        )}
    }
  }
  
  # Save to file ----------------------------------
  setpbarwrapper(pb,  label = "Finished!! Saving to file");
  
  save(changes,file=paste0(outputfolder,"/changes_",NORtable,".Rda"),compress=F)   
  save(masterdataset,file=paste0(outputfolder,"/masterdataset_",NORtable,".Rda"),compress=F)   
  try({barplot(rbind(table(masterdataset$DateRemoved)[2:n],table(masterdataset$DateAdded)[2:n]),beside=TRUE);  
       barplot(table(changes$Date))})
}

AHA_Data_NOR_Log_Postprocessing  = function(){
# Postprocesses the data and combines it into a single all assets file
# Settings -----------------------------------
cfg = list();
assets = list();
achanges = list();
cfg$recalculate_PC6_naar = F

pb = pbarwrapper(title = paste0("AHA_Data_NOR_Log_Postprocessing, ",as.character(Sys.time())), label = "Start", min = 0, max = 15, initial = 0, width = 450);

# Load the data --------------------------
# Load nettopology
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_hoofdleidingen.Rda"))
MS_Hoofdleidingen = mindataset
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_Stations.Rda"))
MS_Stations       = mindataset

# Load hoofdleidingen
setpbarwrapper(pb, label = "Loading verbindingen"); 
load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGEN.Rda"))
masterdataset[,ID_Object := (1:nrow(masterdataset))]
setorder(masterdataset, -DateAdded, na.last=TRUE)
setkey(masterdataset,ID_Object)
masterdataset[ID_Verbinding=="",ID_Verbinding:=NA]
vb = masterdataset

# Load kabels  
setpbarwrapper(pb,label = "Loading verbindingsdelen"); 
load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSDELEN.Rda"))
masterdataset[,ID_Object := (1:nrow(masterdataset))]
setorder(masterdataset, -DateAdded, na.last=TRUE)
setkey(masterdataset,ID_Object)
assets$kabels = masterdataset;

# Generate a file for missing PC6_naar in NOR. Has to be run once on a new load!!
setpbarwrapper(pb,label = paste0("Loading XY in PC, recalculate = ",cfg$recalculate_PC6_naar))
if(cfg$recalculate_PC6_naar)
{XYinPC = AHA_Data_Determine_PC(assets$kabels[,c("Coo_X_naar","Coo_Y_naar","ID_unique"),with=FALSE],"Coo_X_naar","Coo_Y_naar","PC_6_naar")
 save(XYinPC,file=paste0(settings$Input_Datasets,"/6. NOR/XYinPC.Rda"),compress=F)}else{
   load(paste0(settings$Input_Datasets,"/6. NOR/XYinPC.Rda"))}

setkeyv(XYinPC,c("Coo_X_naar","Coo_Y_naar")); 
setkeyv(assets$kabels,c("Coo_X_naar","Coo_Y_naar")); 
assets$kabels= merge(assets$kabels,unique(XYinPC)[,list(PC_6_naar,Coo_X_naar,Coo_Y_naar)]);

# Load moffen
setpbarwrapper(pb, label = "Loading assets moffen"); 
load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSKNOOPPUNTEN.Rda"))

masterdataset[,ID_Object := (1:nrow(masterdataset))]
setorder(masterdataset,-DateAdded, na.last=TRUE)
setkey(masterdataset,ID_Object)
assets$moffen = masterdataset;

# Load moffen changes
setpbarwrapper(pb,label = "Loading verbindingsknooppunten changes"); 
load(paste0(settings$Input_Datasets,"/6. NOR/changes_ELCVERBINDINGSKNOOPPUNTEN.Rda"))

changes[,ID_Object := (1:nrow(changes))]
setorder(changes,ID_unique,-Date, na.last=TRUE)
setkey(changes,ID_unique)
changes[,oldnew := rep(1:2,times=nrow(changes)/2)]
achanges$moffen = Transform_changes(changes)
achanges$moffen$Coo_XY = Merge_xy(achanges$moffen$Coo_X,achanges$moffen$Coo_Y,changes)

# Load kabels changes
setpbarwrapper(pb,label = "Loading verbindingsdelen changes"); 
load(paste0(settings$Input_Datasets,"/6. NOR/changes_ELCVERBINDINGSDELEN.Rda"))

changes[,ID_Object := (1:nrow(changes))]
setorder(changes,ID_unique,-Date, na.last=TRUE)
setkey(changes,ID_unique)

changes[,oldnew := rep(1:2,times=nrow(changes)/2)]
changes[,Lengte_2 := sqrt((Coo_X_van-Coo_X_naar)^2+(Coo_Y_van-Coo_Y_naar)^2)]
achanges$kabels = Transform_changes(changes)
achanges$kabels$Coo_XY_XY = Merge_xy_xy(achanges$kabels$Coo_X_van,achanges$kabels$Coo_Y_van,achanges$kabels$Coo_X_naar,achanges$kabels$Coo_Y_naar,changes)

# Remove the junk
rm("XYinPC")
rm("changes")
rm("masterdataset")
rm("mindataset")

# Add the required fields -------------------------------------------------
setpbarwrapper(pb,label = "Adding some more information to the assets"); 

# Recalculate what mof based on their XY coordinates --------------------
setpbarwrapper(pb, label = "Calculating historical Moffen"); 

setkey(achanges$moffen$Coo_XY,ID_unique)
setkey(assets$moffen,ID_unique)
setnames(achanges$moffen$Coo_XY,"Date","Date_Length_Change")
allXY = achanges$moffen$Coo_XY[,list(ID_unique,Coo_X_oud,Coo_Y_oud,Date_Length_Change)][assets$moffen[
  ,list(DateAdded,Coo_X,Coo_Y,ID_unique,ID_Object,ID_NAN,ID_Verbinding)],allow.cartesian=TRUE]

setnames(achanges$moffen$ID_NAN,"Date","Date_NAN_Change")
setorder(achanges$moffen$ID_NAN,-Date_NAN_Change)
setkey(achanges$moffen$ID_NAN,ID_unique)
allXY = unique(achanges$moffen$ID_NAN[,list(ID_unique,Date_NAN_Change)])[allXY]
allXY[!is.na(ID_NAN)&is.na(Date_NAN_Change),Date_NAN_Change:=as.Date("01-01-2014")]
allXY[,Date_Last:=pmax(DateAdded,Date_NAN_Change,Date_Length_Change, na.rm = T)]

# Transform the integer coordinates into 1 vector
allXY[is.na(Coo_X_oud),Coo_X_oud:=Coo_X]
allXY[is.na(Coo_Y_oud),Coo_Y_oud:=Coo_Y]

# Loop back in the dates and add the historical ID of each uniqiue XY place
assets$moffen = unique(historical_topo(allXY)[,list(ID_unique,ID_unique_present,ID_NAN_present,ID_Verbinding_present)])[assets$moffen]

# Recalculate what kabel based on their XY coordinates --------------------
setpbarwrapper(pb, label = "Calculating historical Kabels"); 

setkey(achanges$kabels$Coo_XY_XY,ID_unique)
setkey(assets$kabels,ID_unique)
setnames(achanges$kabels$Coo_XY_XY,"Date","Date_Length_Change")
allXY = achanges$kabels$Coo_XY_XY[,list(ID_unique,Coo_X_naar_oud,Coo_Y_naar_oud,Coo_X_van_oud,Coo_Y_van_oud,Date_Length_Change)][assets$kabels[
  ,list(DateAdded,Coo_X_van,Coo_Y_van,Coo_X_naar,Coo_Y_naar,ID_unique,ID_Object,ID_NAN,ID_Verbinding)],allow.cartesian=TRUE]

setnames(achanges$kabels$ID_NAN,"Date","Date_NAN_Change")
setorder(achanges$kabels$ID_NAN,-Date_NAN_Change)
setkey(achanges$kabels$ID_NAN,ID_unique)
allXY = unique(achanges$kabels$ID_NAN[,list(ID_unique,Date_NAN_Change)])[allXY]
allXY[!is.na(ID_NAN)&is.na(Date_NAN_Change),Date_NAN_Change:=as.Date("01-01-2014")]
allXY[,Date_Last:=pmax(DateAdded,Date_NAN_Change,Date_Length_Change, na.rm = T)]

# Transform the integer coordinates into 1 vector
allXY[is.na(Coo_X_van_oud),Coo_X_van_oud:=Coo_X_van]
allXY[is.na(Coo_Y_van_oud),Coo_Y_van_oud:=Coo_Y_van]
allXY[is.na(Coo_X_naar_oud),Coo_X_naar_oud:=Coo_X_naar]
allXY[is.na(Coo_Y_naar_oud),Coo_Y_naar_oud:=Coo_Y_naar]

# Loop back in the dates and add the historical ID of each uniqiue XY place
assets$kabels = unique(historical_topo(allXY,byvars=c("Coo_X_naar","Coo_Y_naar","Coo_X_van","Coo_Y_van"))[,list(ID_unique,ID_unique_present,ID_NAN_present,ID_Verbinding_present)])[assets$kabels]

# Add the length changes -------------------
setpbarwrapper(pb, label = "Calculating length changes Kabels"); 

mergeset = rbind(achanges$kabels$Lengte[,lch := nieuw-oud],achanges$kabels$Lengte_2[,lch :=nieuw-oud])
setkey(mergeset,ID_unique,Date)
mergeset = mergeset[,max(lch),by=list(ID_unique,Date)]
setnames(mergeset,c("Date","V1"),c("DateLength_ch","Length_ch"))
setkey(mergeset,ID_unique)
setkey(assets$kabels,ID_unique)
mergeset = assets$kabels[mergeset]
mergeset[,Status_ID:="Length_changed"]
mergeset[,DateRemoved:=NA]
assets$kabels = rbind(assets$kabels,mergeset,fill=T)

# Add the Status changes to cables ----------------------------
setpbarwrapper(pb, label = "Calculating status changes Kabels"); 

setkey(achanges$kabels$Status,ID_unique,Date)
achanges$kabels$Status[,Status_ch:=paste0(oud,"->",nieuw)]
mergeset = unique(achanges$kabels$Status)[,list(Date,ID_unique,Status_ch)]
setnames(mergeset,c("Date"),c("Date_Status_ch"))
setkey(mergeset,ID_unique)
setkey(assets$kabels,ID_unique)
mergeset = assets$kabels[mergeset]
mergeset[,Status_ID:="Status_Change"]
mergeset[,DateRemoved:=NA]
mergeset[,DateLength_ch:=NA]
assets$kabels = rbind(assets$kabels,mergeset,fill=TRUE)

# Add the HLD to kabels ---------------------------
setpbarwrapper(pb, label = "Add the HLD and MSRings to kabels"); 
vb[,ID_Verbinding_present:=ID_Verbinding]

# First merge on verbinding and Beheerder
setkey(assets$kabels,ID_Verbinding_present,Beheerder); setkey(vb,ID_Verbinding_present,Beheerder)
assets$kabels[,ID_Hoofdleiding_present := unique(vb)[assets$kabels,ID_Hoofdleiding]]

# Next merge just on verbindingen
setkey(assets$kabels,ID_Verbinding_present);setkey(vb,ID_Verbinding_present)
assets$kabels[is.na(assets$kabels$ID_Hoofdleiding_present),
              ID_Hoofdleiding_present:= unique(vb)[assets$kabels,ID_Hoofdleiding][is.na(assets$kabels$ID_Hoofdleiding_present)]]

# Add the MSRing ----------------------------------------------------------
setpbarwrapper(pb, label = "Adding MSRing to Kabels");

setnames(MS_Hoofdleidingen,"Nummer","ID_Hoofdleiding_present")
setkey(MS_Hoofdleidingen,ID_Hoofdleiding_present); setkey(assets$kabels,ID_Hoofdleiding_present)
assets$kabels[,Routenaam_Present:=unique(MS_Hoofdleidingen[Routenaam !=""])[assets$kabels,Routenaam]]

# Second on position
nearest2 = nn2(
              assets$kabels[!is.na(Coo_X_naar)&!is.na(Routenaam_Present)&Brontabel=="ms_kabels",list(Coo_X_naar,Coo_Y_naar)],
              assets$kabels[!is.na(Coo_X_naar)&is.na(Routenaam_Present)&Brontabel=="ms_kabels",list(Coo_X_naar,Coo_Y_naar)],k=1)

assets$kabels[!is.na(Coo_X_naar)&is.na(Routenaam_Present)&Brontabel=="ms_kabels",
              Routenaam_Present:=
                assets$kabels$Routenaam_Present[
                  !is.na(assets$kabels$Coo_X_naar)&!is.na(assets$kabels$Routenaam_Present)&assets$kabels$Brontabel=="ms_kabels"
                  ][nearest2$nn.idx[,1]]]

# Attach the cables to moffen ------------------------------
setpbarwrapper(pb, label = "Adding the HLD and MSRings to moffen ")

# First with Beheerder and Verbinding
setkey(assets$moffen,ID_Verbinding,Beheerder); setkey(assets$kabels,ID_Verbinding,Beheerder)
assets$moffen[,Routenaam_Present:=unique(assets$kabels)[assets$moffen,Routenaam_Present]]
assets$moffen[,ID_Hoofdleiding_present:=unique(assets$kabels)[assets$moffen,ID_Hoofdleiding_present]]

# Second with  Verbinding
setkey(assets$moffen,ID_Verbinding,Beheerder); setkey(assets$kabels,ID_Verbinding,Beheerder)
assets$moffen[is.na(Routenaam_Present),
              Routenaam_Present:=unique(assets$kabels)[assets$moffen,Routenaam_Present][is.na(assets$moffen$Routenaam_Present)]]
assets$moffen[is.na(ID_Hoofdleiding_present),
              ID_Hoofdleiding_present:=unique(assets$kabels)[assets$moffen,ID_Hoofdleiding_present][is.na(assets$moffen$ID_Hoofdleiding_present)]]

# Third use NN for Routenaam
nearest = nn2(assets$kabels[!is.na(Coo_X_naar) & !is.na(Routenaam_Present),list(Coo_X_naar,Coo_Y_naar)],assets$moffen[!is.na(Coo_X),list(Coo_X,Coo_Y)],k=1)
nearest2= nn2(assets$kabels[!is.na(Coo_X_naar) & !is.na(Routenaam_Present),list(Coo_X_van,Coo_Y_van)],assets$moffen[!is.na(Coo_X),list(Coo_X,Coo_Y)],k=1)
nnd = (nearest$nn.dists[,1]>=nearest2$nn.dists[,1])
nni = nearest$nn.idx[,1]; nni[nnd] = nearest2$nn.idx[nnd,1]
nni[!is.na(assets$moffen$Coo_X)] = nni
nni[is.na(assets$moffen$Coo_X) ] = NA

assets$moffen[is.na(Routenaam_Present)&!is.na(nni),
              Routenaam_Present:=
                assets$kabels$Routenaam_Present[
                  !is.na(assets$kabels$Coo_X_naar)&
                    !is.na(assets$kabels$Routenaam_Present)][
                      nni[!is.na(nni) & is.na(assets$moffen$Routenaam_Present)]]]

# Fourth use NN for Hoofdleiding
nearest = nn2(assets$kabels[!is.na(Coo_X_naar) & !is.na(ID_Hoofdleiding_present),list(Coo_X_naar,Coo_Y_naar)],assets$moffen[!is.na(Coo_X),list(Coo_X,Coo_Y)],k=1)
nearest2= nn2(assets$kabels[!is.na(Coo_X_naar) & !is.na(ID_Hoofdleiding_present),list(Coo_X_van,Coo_Y_van)],assets$moffen[!is.na(Coo_X),list(Coo_X,Coo_Y)],k=1)
nnd = (nearest$nn.dists[,1]>=nearest2$nn.dists[,1])
nni = nearest$nn.idx[,1]; nni[nnd] = nearest2$nn.idx[nnd,1]
nni[!is.na(assets$moffen$Coo_X)] = nni
nni[is.na(assets$moffen$Coo_X) ] = NA

assets$moffen[is.na(ID_Hoofdleiding_present)&!is.na(nni),
              ID_Hoofdleiding_present:=
                assets$kabels$ID_Hoofdleiding_present[
                  !is.na(assets$kabels$Coo_X_naar)&
                    !is.na(assets$kabels$ID_Hoofdleiding_present)][
                      nni[!is.na(nni) & is.na(assets$moffen$ID_Hoofdleiding_present)]]]

# Add the object ID (Index)
assets$moffen[,ID_Object:=1:nrow(assets$moffen)]
assets$kabels[,ID_Object:=1:nrow(assets$kabels)]

# Save the data --------------------------------------------------
setpbarwrapper(pb, label = "Saving to file"); 

save(assets,file=paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))

all_ID_NAN = c(unique(assets$kabels$ID_NAN),unique(assets$moffen$ID_NAN))
save(all_ID_NAN,file=paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_all_ID_NAN.Rda"))

setpbarwrapper(pb, label = "Done"); 

}
# Other functions ----------------------------

Transform_changes= function(changes) {
  # Function that transforms the useless data structure of changes into something more managable
  metacols = c("Date","ID_Object","oldnew","ID_unique")
  nms      = names(changes)
  
  output = llply(nms[!(nms %in% metacols)],      
                 function(x){
                   a = (changes[oldnew==1,x,with=F] != changes[oldnew==2,x,with=F])
                   if (x!="ID_NAN"){
                     a = a & !(is.na(changes[oldnew==1,x,with=F]) | is.na(changes[oldnew==2,x,with=F]))
                   }
                   unique1 = changes$oldnew ==1 & rep(a,each=2)
                   unique2 = changes$oldnew ==2 & rep(a,each=2)
                   
                   temp = changes[unique1,metacols,with=FALSE]
                   temp[,oud    := changes[unique1,x ,with=FALSE]]
                   temp[,nieuw  := changes[unique2,x,with=FALSE]]
                   temp}) 
  names(output) = nms[!(nms %in% metacols)]
  return( output )
}

Merge_xy = function(datax,datay,changes){
  # Merge XY changes
  xynames = c("Coo_X","Coo_Y")
  metacols = c("Date","ID_Object","oldnew","ID_unique","ID_NAN")
  setnames(datax,c("oud","nieuw"),c(paste0(xynames[1],"_oud"),paste0(xynames[1],"_nieuw")))
  setnames(datay,c("oud","nieuw"),c(paste0(xynames[2],"_oud"),paste0(xynames[2],"_nieuw")))
  
  setkey(datax,ID_Object)
  setkey(datay,ID_Object)
  return(merge(datax,datay[,c(paste0(xynames[2],"_oud"),paste0(xynames[2],"_nieuw"),"ID_Object"),with=F]))
}

Merge_xy_xy = function(dataxvan,datayvan,dataxnaar,dataynaar,changes){
  # Merge XY changes
  xynames = c("Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar")
  metacols = c("Date","ID_Object","oldnew","ID_unique","ID_NAN")
  try(setnames(dataxvan,c("oud","nieuw"),c(paste0(xynames[1],"_oud"),paste0(xynames[1],"_nieuw"))))
  try(setnames(datayvan,c("oud","nieuw"),c(paste0(xynames[2],"_oud"),paste0(xynames[2],"_nieuw"))))
  try(setnames(dataxnaar,c("oud","nieuw"),c(paste0(xynames[3],"_oud"),paste0(xynames[3],"_nieuw"))))
  try(setnames(dataynaar,c("oud","nieuw"),c(paste0(xynames[4],"_oud"),paste0(xynames[4],"_nieuw"))))
  
  setkey(dataxvan,ID_Object)
  setkey(datayvan,ID_Object)
  setkey(dataxnaar,ID_Object)
  setkey(dataynaar,ID_Object)
  
  return(
    merge(
      merge(
        merge(
          dataxvan,datayvan[,c(paste0(xynames[2],"_oud"),paste0(xynames[2],"_nieuw"),"ID_Object"),with=F])
        ,dataxnaar[,c(paste0(xynames[3],"_oud"),paste0(xynames[3],"_nieuw"),"ID_Object"),with=F])
      ,dataynaar[,c(paste0(xynames[4],"_oud"),paste0(xynames[4],"_nieuw"),"ID_Object"),with=F])
  )
}

historical_topo = function(allXY, byvars=c("Coo_X","Coo_Y"))
{
# Used to calculate histortical nettopology
allXY = allXY[!is.na(allXY[[byvars[1]]])]
allXY[,ID_unique_present:=ID_unique]
allXY[,ID_NAN_present:=ID_NAN]
allXY[,ID_Verbinding_present:=ID_Verbinding]

# Loop back in the dates and add the historical ID of each uniqiue XY place
newXY = allXY[,c(paste0(byvars,"_oud"),"ID_unique_present","ID_NAN_present","ID_Verbinding_present","Date_Last"),with=F]

setorder(newXY,-Date_Last,na.last=T)
newXY   = unique(newXY[!is.na(ID_NAN_present)])
  
setnames(newXY,paste0(byvars,"_oud"),byvars)
setkeyv(newXY,byvars)
setkeyv(allXY,byvars)
allXY = unique(newXY)[allXY[,names(allXY)[!(names(allXY) %in% c("ID_unique_present","ID_NAN_present","ID_Verbinding_present","Date_Last"))],with=F],allow.cartesian=T]
cat(paste0("is NOT NA: NAN, ",sum(!is.na(allXY$ID_NAN_present))," of ",nrow(allXY)," = ",round(sum(!is.na(allXY$ID_NAN_present))/nrow(allXY)*100)," %\n"))

# If nothing found try using the nearest neighbours
nearest2 = nn2(
allXY[!is.na(ID_NAN_present),byvars[1:2],with=F],          
allXY[is.na(ID_NAN_present),byvars[1:2],with=F],
k=1)$nn.idx

allXY[is.na(ID_NAN_present),ID_Verbinding_present:=allXY$ID_Verbinding_present[!is.na(allXY$ID_NAN_present)][nearest2]]
allXY[is.na(ID_NAN_present),ID_unique_present:=allXY$ID_unique_present[!is.na(allXY$ID_NAN_present)][nearest2]]
allXY[is.na(ID_NAN_present),ID_NAN_present:=allXY$ID_NAN_present[!is.na(allXY$ID_NAN_present)][nearest2]]

cat(paste0("is NOT NA: NAN, ",sum(!is.na(allXY$ID_NAN_present))," of ",nrow(allXY)," = ",round(sum(!is.na(allXY$ID_NAN_present))/nrow(allXY)*100)," %\n"))

# Fill the remaining with original values
# allXY[is.na(ID_unique_present),ID_unique_present:=ID_unique]
# allXY[is.na(ID_NAN_present),ID_NAN_present:=ID_NAN]
# allXY[is.na(ID_Verbinding_present),ID_Verbinding_present:=ID_Verbinding]

setkey(allXY,ID_unique)
return(unique(allXY))

# Just some nice plots
a=allXY[,sum(!is.na(ID_NAN_present))/(sum(!is.na(ID_NAN_present))+sum(is.na(ID_NAN_present))),by=Date_Last]
setorder(a,by=Date_Last)
barplot(a$V1,names.arg=a$Date_Last)

}
