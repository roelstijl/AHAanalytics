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
comparecols = switch (NORtable,ELCVERBINDINGSKNOOPPUNTEN=c("ID_unique","ID_NAN","Bronsysteem","Spanningsniveau","Beheerder", "Soort",  "Constructie",	"Isolatiemedium",	"Fabrikant","Coo_X","Coo_Y"),
                      ELCVERBINDINGSDELEN=c("ID_unique","Lengte","Bronsysteem","ID_NAN","Status","Beheerder","Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar","Spanningsniveau","Diameter","Netverbinding"),
                      ELCVERBINDINGEN=c("ID_unique","Lengte", "Bronsysteem",	"SpanningsNiveau","Beheerder",	"Soort",	"Soortnet"),
                      cat("Please add headers to compute\n\n"))
# Plot to check for anomolies in file sizes
plot(file.info(files)$size)
  
# Load from backup functions --------------------------------------------
if (source == "backup") {
#   backups = list.files(pattern=paste0("masterdataset_backup",".*\\.Rda"), path=paste0(outputfolder,"/backup"),full.names=TRUE);
#   print(backups)
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
differences = !(dup(combinedset,by=comparecols) | dup(combinedset,by=comparecols,fromLast=TRUE))

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

save(changes,file=paste0(outputfolder,"/changes_",NORtable,".Rda"))   
save(masterdataset,file=paste0(outputfolder,"/masterdataset_",NORtable,".Rda"))   
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

pb = pbarwrapper(title = paste0("AHA_Data_NOR_Log_Postprocessing, ",as.character(Sys.time())), label = "Start", min = 0, max = 10, initial = 0, width = 450);
  
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
if(cfg$recalculate_PC6_naar){XYinPC = AHA_Data_Determine_PC(assets$kabels[,c("Coo_X_naar","Coo_Y_naar","ID_unique"),with=FALSE],"Coo_X_naar","Coo_Y_naar","PC_6_naar")
  save(XYinPC,file=paste0(settings$Input_Datasets,"/6. NOR/XYinPC.Rda"))}
else{load(paste0(settings$Input_Datasets,"/6. NOR/XYinPC.Rda"))}
setkeyv(XYinPC,c("Coo_X_naar","Coo_Y_naar")); setkeyv(assets$kabels,c("Coo_X_naar","Coo_Y_naar")); 
assets$kabels= merge(assets$kabels,unique(XYinPC)[,list(PC_6_naar,Coo_X_naar,Coo_Y_naar)]);remove("XYinPC")

# Load moffen
setpbarwrapper(pb, label = "Loading assets moffen"); 
load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSKNOOPPUNTEN.Rda"))
masterdataset[,ID_Object := (1:nrow(masterdataset))]
setorder(masterdataset,-DateAdded, na.last=TRUE)
setkey(masterdataset,ID_Object)
assets$moffen = masterdataset;

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

# Load moffen changes
setpbarwrapper(pb,label = "Loading verbindingsknooppunten changes"); 
load(paste0(settings$Input_Datasets,"/6. NOR/changes_ELCVERBINDINGSKNOOPPUNTEN.Rda"))
changes[,ID_Object := (1:nrow(changes))]
setorder(changes,ID_unique,-Date, na.last=TRUE)
setkey(changes,ID_unique)
changes[,oldnew := rep(1:2,times=nrow(changes)/2)]
achanges$moffen = Transform_changes(changes)
achanges$moffen$Coo_XY = Merge_xy(achanges$moffen$Coo_X,achanges$moffen$Coo_Y,changes)

# Remove the junk
rm("changes")
rm("masterdataset")

# Add the required fields -------------------------------------------------
setpbarwrapper(pb,label = "Adding some more information to the assets"); 

# Recalculate what mof based on their XY coordinates --------------------
setorder(achanges$moffen$Coo_XY,Date, na.last=TRUE)
setkey(achanges$moffen$Coo_XY,ID_unique)
setkey(assets$moffen,ID_unique)
allXY = unique(achanges$moffen$Coo_XY[,list(ID_unique,Coo_X_oud,Coo_Y_oud)])[assets$moffen[,list(DateAdded,Coo_X,Coo_Y,ID_unique,ID_Object,ID_NAN,ID_Verbinding)]]
allXY[is.na(Coo_X_oud),Coo_X_oud:=Coo_X]
allXY[is.na(Coo_Y_oud),Coo_Y_oud:=Coo_Y]
allXY = allXY[!is.na(Coo_X)]
allXY[,ID_unique_present:=ID_unique]
allXY[,ID_NAN_present:=ID_NAN]
allXY[,ID_Verbinding_present:=ID_Verbinding]

# Transform the integer coordinates into 1 vector
allXY[,Coo_XY_oud     := (Coo_X_oud*2)+(Coo_Y_oud*2-1)]
allXY[,Coo_XY_present := (Coo_X*2)+(Coo_Y*2-1)]

# Loop back in the dates and add the historical ID of each uniqiue XY place
setpbarwrapper(pb, label = "Calculating historical Moffen"); 
for (n in 1:3){
setorder(allXY,-DateAdded, na.last=TRUE)
setkey(allXY,Coo_XY_present)
newXY   = unique(allXY)[!is.na(ID_NAN),list(Coo_XY_oud,ID_unique_present,ID_NAN_present,ID_Verbinding_present,DateAdded)]
setnames(newXY,c("Coo_XY_oud"),c("Coo_XY_present"))
setkey(newXY,Coo_XY_present)
setkey(allXY,Coo_XY_present)
allXY = newXY[allXY[TRUE],allow.cartesian=T]
allXY[,i.ID_unique_present := NULL]
allXY[,i.ID_NAN_present    := NULL]
allXY[,i.ID_Verbinding_present := NULL]
allXY[,i.DateAdded := NULL]
}

# a=allXY[,sum(!is.na(ID_NAN_present))/(sum(!is.na(ID_NAN_present))+sum(is.na(ID_NAN_present))),by=DateAdded]
# setorder(a,by=DateAdded)
# barplot(a$V1,names.arg=a$DateAdded)

allXY[is.na(ID_unique_present),ID_unique_present:=ID_unique]
allXY[is.na(ID_NAN_present),ID_NAN_present:=ID_NAN]
allXY[is.na(ID_Verbinding_present),ID_Verbinding_present:=ID_Verbinding]
setkey(allXY,ID_unique)
setkey(assets$moffen,ID_unique)
assets$moffen = unique(allXY[,list(ID_unique,ID_unique_present,ID_NAN_present)])[assets$moffen]

# Recalculate what kabel based on their XY coordinates --------------------
setpbarwrapper(pb, label = "Calculating historical Kabels"); 

setorder(achanges$kabels$Coo_XY_XY,-Date, na.last=TRUE)
setkey(achanges$kabels$Coo_XY_XY,ID_unique)
setkey(assets$kabels,ID_unique)
allXY = unique(achanges$kabels$Coo_XY_XY[,list(ID_unique,Coo_X_van_oud,Coo_Y_van_oud,Coo_X_naar_oud,Coo_Y_naar_oud)])[assets$kabels[,list(ID_NAN,ID_Verbinding,Coo_X_van,Coo_Y_van,Coo_X_naar,Coo_Y_naar,ID_unique,ID_Object)]]

allXY[is.na(Coo_X_van_oud),Coo_X_van:=Coo_X_van]
allXY[is.na(Coo_Y_van_oud),Coo_Y_van_oud:=Coo_Y_van]
allXY[is.na(Coo_X_naar_oud),Coo_X_naar_oud:=Coo_X_naar]
allXY[is.na(Coo_Y_naar_oud),Coo_Y_naar_oud:=Coo_Y_naar]

allXY = allXY[!is.na(Coo_X_van)]
allXY[,ID_unique_present:=ID_unique]
allXY[,ID_NAN_present:=ID_NAN]
allXY[,ID_Verbinding_present:=ID_Verbinding]

allXY[,Coo_XY_oud := (Coo_X_van_oud*4)+(Coo_Y_van_oud*4-1)+(Coo_X_naar_oud*4-2)+(Coo_Y_naar_oud*4-3)]
allXY[,Coo_XY_present := (Coo_X_van*4)+(Coo_Y_van*4-1)+(Coo_X_naar*4-2)+(Coo_Y_naar*4-3)]

historical_topo = 

setkey(assets$kabels,ID_unique)
assets$kabels = unique(allXY[,list(ID_unique,ID_unique_present,ID_NAN_present,ID_Verbinding_present)])[assets$kabels]

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

# Add the Status changes to cables --------------------------
setpbarwrapper(pb, label = "Calculating status changes Kabels"); 

setkey(achanges$kabels$Status,ID_unique,Date)
achanges$kabels$Status[,Status_ch:=paste0(oud,"->",nieuw)]
mergeset = unique(achanges$kabels$Status[,list(Date,ID_unique,Status_ch)])
setnames(mergeset,c("Date"),c("Date_Status_ch"))
setkey(mergeset,ID_unique)
setkey(assets$kabels,ID_unique)
mergeset = assets$kabels[mergeset]
mergeset[,Status_ID:="Status_Change"]
mergeset[,DateRemoved:=NA]
mergeset[,DateLength_ch:=NA]
rbind(assets$kabels,mergeset,fill=TRUE)

# Add the HLD and MSRings to kabels ------------------------------
setpbarwrapper(pb, label = "Add the HLD and MSRings to kabels"); 

# Try 3 methods in order of accuracy
setkey(assets$kabels,ID_Verbinding,Beheerder); 
setkey(vb,ID_Verbinding,Beheerder)
dup = function(data) duplicated(data)

bronsystemen = assets$kabels$Beheerder
notcoupled = is.na(unique(vb[,list(ID_Hoofdleiding,ID_Verbinding,Beheerder)])[assets$kabels]$ID_Hoofdleiding)
assets$kabels[notcoupled,Beheerder:="Empty"]
ve = vb
ve$Beheerder="Empty"
ve = rbind(vb,ve)
setkey(ve,ID_Verbinding,Beheerder)
setkey(assets$kabels,ID_Verbinding,Beheerder)

# Make sure we get 5 of the Hoofdleidingen, to negate the ambiguity in verbindingen
z=rep(F,each=nrow(ve))
a=dup(ve)
b=z; b[a]=dup(ve[a])
c=z; c[b]=dup(ve[b])
d=z; d[c]=dup(ve[c])
setnames(ve,"ID_Hoofdleiding","ID_Hoofdleiding_1")
assets$kabels = ve[!a,list(ID_Hoofdleiding_1,ID_Verbinding,Beheerder)][assets$kabels]
setnames(ve,"ID_Hoofdleiding_1","ID_Hoofdleiding_2")
assets$kabels = ve[a&!b,list(ID_Hoofdleiding_2,ID_Verbinding,Beheerder)][assets$kabels]
setnames(ve,"ID_Hoofdleiding_2","ID_Hoofdleiding_3")
assets$kabels = ve[b&!c,list(ID_Hoofdleiding_3,ID_Verbinding,Beheerder)][assets$kabels]
setnames(ve,"ID_Hoofdleiding_3","ID_Hoofdleiding_4")
assets$kabels = ve[c&!d,list(ID_Hoofdleiding_4,ID_Verbinding,Beheerder)][assets$kabels]
setnames(ve,"ID_Hoofdleiding_4","ID_Hoofdleiding_5")
assets$kabels = ve[d,list(ID_Hoofdleiding_5,ID_Verbinding,Beheerder)][assets$kabels]

# Add the MSRing ----------------------------------------------------------
setpbarwrapper(pb, label = "Adding MSRing to Kabels"); 

setkey(MS_Hoofdleidingen,ID_Hoofdleiding)
setkey(assets$kabels,ID_Hoofdleiding)
nettopo_MSRing_hld = unique(nettopo_MSRing_hld[nettopo_MSRing_hld$Routenaam !=""])[,list(ID_Hoofdleiding,Routenaam)]
assets$kabels = nettopo_MSRing_hld[assets$kabels]

# Add the HLD and MSRings to moffen ------------------------------
# Try to find the matching cable, this will not always work
setpbarwrapper(pb, label = "Adding the HLD and MSRings to moffen ")

setkey(assets$moffen,ID_Verbinding,Beheerder)
setkey(assets$kabels,ID_Verbinding,Beheerder)
assets$moffen= unique(assets$kabels[,list(ID_Verbinding,Beheerder,ID_Hoofdleiding,Routenaam)])[assets$moffen] 

setnames(assets$kabels,c("Coo_X_naar","Coo_Y_naar"),c("Coo_X","Coo_Y"))
setkey(assets$moffen,Coo_X,Coo_Y)
setkey(assets$kabels,Coo_X,Coo_Y)
set(assets$moffen,is.na(assets$moffen$ID_Hoofdleiding),,unique(assets$kabels[,list(ID_Verbinding,Beheerder,ID_Hoofdleiding,Routenaam)])[assets$moffen][is.na(assets$moffen)])

setnames(assets$kabels,c("Coo_X","Coo_Y","Coo_X_van","Coo_Y_van"),c("Coo_X_naar","Coo_Y_naar","Coo_X","Coo_Y"))
setkey(assets$kabels,Coo_X,Coo_Y)
set(assets$moffen,is.na(assets$moffen$ID_Hoofdleiding),,unique(assets$kabels[,list(ID_Verbinding,Beheerder,ID_Hoofdleiding,Routenaam)])[assets$moffen][is.na(assets$moffen)])

setnames(assets$kabels,c("Coo_X","Coo_Y"),c("Coo_X_van","Coo_Y_van"))

# Save the data --------------------------------------------------
setpbarwrapper(pb, label = "Saving to file"); 

save(assets,file=paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))

all_ID_NAN = c(unique(assets$kabels$ID_NAN),unique(assets$moffen$ID_NAN))
save(all_ID_NAN,file=paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_all_ID_NAN.Rda"))

setpbarwrapper(pb, label = "Done"); 

}

Transform_changes= function(changes) {
  # Function that transforms the useless data structure of changes into something more managable
  metacols = c("Date","ID_Object","oldnew","ID_unique","ID_NAN")
  nms      = names(changes)

    output = llply(nms[!(nms %in% metacols)],      
     function(x){
       a = (changes[oldnew==1,x,with=F] != changes[oldnew==2,x,with=F])
       a = a & !(is.na(changes[oldnew==1,x,with=F]) | is.na(changes[oldnew==2,x,with=F]))
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

historical_topo = function()
{
  
  # Loop back in the dates and add the historical ID of each uniqiue XY place
  for (n in 1:3){
    setkey(allXY,Coo_XY_present)
    dupXY   = duplicated(allXY)
    newXY   = allXY[!dupXY,list(Coo_XY_oud,ID_unique_present,ID_NAN_present,ID_Verbinding_present)]
    setnames(newXY,c("Coo_XY_oud"),c("Coo_XY_present"))
    setkey(newXY,Coo_XY_present)
    allXY = newXY[allXY]
    allXY$i.ID_unique_present = NULL
    allXY$i.ID_NAN_present    = NULL
    allXY$i.ID_Verbinding_present=NULL
  }
  
  allXY[is.na(ID_unique_present),ID_unique_present:=ID_unique]
  allXY[is.na(ID_NAN_present),ID_NAN_present:=ID_NAN]
  allXY[is.na(ID_Verbinding_present),ID_Verbinding_present:=ID_Verbinding]
  setorder(allXY,ID)
  setkey(allXY,ID_unique)
  return(allXY)
}
  