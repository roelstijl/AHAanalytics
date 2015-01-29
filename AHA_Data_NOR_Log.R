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
comparecols = switch (NORtable,ELCVERBINDINGSKNOOPPUNTEN=c("ID_unique","ID_NAN","Bronsysteem","Spanningsniveau", "Soort",  "Constructie",	"Isolatiemedium",	"Fabrikant","Coo_X","Coo_Y"),
                      ELCVERBINDINGSDELEN=c("ID_unique","Lengte","Bronsysteem","ID_NAN","Status","Geleidermateriaal","Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar","Spanningsniveau","Diameter","Netverbinding"),
                      ELCVERBINDINGEN=c("ID_unique","Beheerder","Lengte", "Bronsysteem",	"SpanningsNiveau",	"Soort",	"Soortnet"),
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

save(changes,file=paste0(outputfolder,"/changes_",NORtable,".Rda"))   
save(masterdataset,file=paste0(outputfolder,"/masterdataset_",NORtable,".Rda"))   
try({barplot(rbind(table(masterdataset$DateRemoved)[2:n],table(masterdataset$DateAdded)[2:n]),beside=TRUE);  
     barplot(table(changes$Date))})
}

AHA_Data_NOR_Log_Postprocessing  = function(){
# Postprocesses the data and combines it into a single all assets file
# Load and prepare some data --------------------------------------------------
assets = list(); achanges = list();
pb = pbarwrapper(title = paste0("AHA_Data_NOR_Log_Postprocessing, ",as.character(Sys.time())), label = "Start", min = 0, max = 10, initial = 0, width = 450);

# Load the data --------------------------
# Load hoofdleidingen
setpbarwrapper(pb, label = "Loading verbindingen"); 
load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGEN.Rda"))
masterdataset[,ID_Object := (1:nrow(masterdataset))]
setkey(masterdataset,ID_Object)
setorder(masterdataset, -DateAdded, na.last=TRUE)
verbindingen = unique(masterdataset,by=c("ID_Verbinding","Beheerder","ID_NAN"))[!is.na(ID_Hoofdleiding)]

# Load kabels  
setpbarwrapper(pb,label = "Loading verbindingsdelen"); 
load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSDELEN.Rda"))
masterdataset[,ID_Object := (1:nrow(masterdataset))]
setorder(masterdataset, -DateAdded, na.last=TRUE)
setkey(masterdataset,ID_Object)
assets$kabels = masterdataset;

# Load moffen
setpbarwrapper(pb, label = "Loading assets moffen"); 
load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSKNOOPPUNTEN.Rda"))
masterdataset[,ID_Object := (1:nrow(masterdataset))]
setorder(masterdataset, -DateAdded, na.last=TRUE)
setkey(masterdataset,ID_Object)
assets$moffen = masterdataset;

# Load kabels changes
setpbarwrapper(pb,label = "Loading verbindingsdelen changes"); 
load(paste0(settings$Input_Datasets,"/6. NOR/changes_ELCVERBINDINGSDELEN.Rda"))
changes[,ID_Object := (1:nrow(changes))]
setorder(changes, ID_unique,Datum, na.last=TRUE)
setkey(changes(ID_Object))

setorder(achanges$kabels,ID_unique,-Date, na.last=TRUE)

achanges$kabels = changes

# Load moffen changes
setpbarwrapper(pb,label = "Loading verbindingsknooppunten changes"); 

# Remove the junk
rm("changes")
rm("masterdataset")

# Add the required fields -------------------------------------------------
setpbarwrapper(pb,label = "Assind some more information to the assets"); 

# Couple the newest NOR values for assets to the past ones basd on XY


# Recalculate the lengths and remove the NAs
achanges$kabels[,Lengte_2 := sqrt((Coo_X_van-Coo_X_naar)^2+(Coo_Y_van-Coo_Y_naar)^2)]


setkey(assets$kabels ,ID_NAN,Datum_Wijziging)
setorder(assets$kabels ,ID_NAN,Datum_Wijziging)
assets$kabels [,Lengte:= sqrt(abs(Coo_X_van-Coo_X_naar)^2+abs(Coo_Y_van-Coo_Y_naar)^2)]
lch = c(0,assets$kabels [2:nrow(assets$kabels ),Lengte] - assets$kabels [1:(nrow(assets$kabels )-1),Lengte])
logi = duplicated(assets$kabels ,by="ID_NAN")
assets$kabels [logi,Length_ch:=lch[logi]]
assets$kabels [Length_ch!=0,DateLength_ch:=(Datum_Wijziging)]
assets$kabels [!is.na(DateLength_ch),Status_ID:="Length_changed"]

# Add the length changes
setkey  (achanges$kabels,ID_unique,Date)
setkey  (assets$kabels,ID_unique)
setorder(achanges$kabels,ID_unique,Date)

achanges$kabels[,Length_ch:=Lengte[2]-Lengte[1],by=list(ID_unique,Date)]
achanges$kabels[Length_ch!=0,DateLength_ch:=Date]
achanges$kabels[!is.na(DateLength_ch),Status_ID:="Length_changed"]

mergeset = unique(achanges$kabels[Status_ID=="Length_changed",list(DateLength_ch,Status_ID,ID_unique,Length_ch)],fromLast = TRUE)
setkey(mergeset,ID_unique)
setkey(assets$kabels,ID_unique)
mergeset=mergeset[!is.na(Length_ch)]
mergeset[,DateRemoved:=NA]
assets$kabels = rbind(assets$kabels,assets$kabels[mergeset],fill=TRUE)

# Add the Status changes to cables
setkey(assets$kabels,Coo_X_van,Coo_Y_van,Coo_X_naar,Coo_Y_naar)
a=assets$kabels[unique(assets$kabels[list(Coo_X_van,Coo_Y_van,Coo_X_naar,Coo_Y_naar,ID_Hoofdleiding,ID_NAN,ID_Unique)])]
ggplot(assets$kabels,aes(x=DateAdded,fill=Bronsysteem)) + geom_bar(stat="bin")

# Generate a file for missing PC6_naar in NOR. Has to be run once on a new load!!
# Takes a long time to calculate ....
if(FALSE){
  XYinPC = AHA_Data_Determine_PC(
    assets$kabels[,c("Coo_X_naar","Coo_Y_naar","ID_unique"),with=FALSE],
    "Coo_X_naar","Coo_Y_naar","PC_6_naar")
  save(XYinPC,file=paste0(settings$Input_Datasets,"/6. NOR/XYinPC.Rda"))}
else{load(paste0(settings$Input_Datasets,"/6. NOR/XYinPC.Rda")); 
}

# Add missing PC_6_naar information
setpbarwrapper(pb, 4,label = "Loading XY in PC"); 
setkeyv(XYinPC,c("Coo_X_naar","Coo_Y_naar")); 
setkeyv(assets$kabels,c("Coo_X_naar","Coo_Y_naar"))
assets$kabels= merge(assets$kabels,unique(XYinPC)[,list(PC_6_naar,Coo_X_naar,Coo_Y_naar)])
remove("XYinPC")

# Add the HLD and MSRings to kabels ------------------------------
setpbarwrapper(pb, 6,label = "Add the HLD and MSRings to kabels"); 

assets$kabels$Index = (1:length(assets$kabels$ID_NAN)) # Some weird bug required this inefficient syntax

# Try 3 methods in order of accuracy
a=Add_HLD(c("ID_Verbinding","Bronsysteem"), assets$kabels, verbindingen)
b=Add_HLD(c("ID_NAN"), assets$kabels, verbindingen)
c=Add_HLD(c("ID_Verbinding"), assets$kabels, verbindingen)

# Combine with the asset data in order a,b,c
assets$kabels[!is.na(a$ID_Hoofdleiding),ID_Hoofdleiding:=a[!is.na(a$ID_Hoofdleiding),ID_Hoofdleiding]]
assets$kabels[!is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding:=b[!is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding]]
assets$kabels[is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding:=c[is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding]]
assets$kabels[assets$kabels$ID_Hoofdleiding=="",ID_Hoofdleiding:=NA]

setkey(verbindingen,ID_Hoofdleiding)
setkey(assets$kabels,ID_Hoofdleiding)
assets$kabels = unique(verbindingen[,list(ID_Hoofdleiding,MS_Route_NOR_IDTrace)])[assets$kabels]
assets$kabels[MS_Route_NOR_IDTrace=="null",MS_Route_NOR_IDTrace:=NA]

remove(a,b,c)

# Add the MSRing
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/nettopo_MSHLD_MSRing.Rda"))
try(setnames(nettopo_MSRing_hld,c("Nummer","NAN"),c("ID_Hoofdleiding","ID_NAN")))

setkey(nettopo_MSRing_hld,ID_Hoofdleiding)
setkey(assets$kabels,ID_Hoofdleiding)
nettopo_MSRing_hld = unique(nettopo_MSRing_hld[nettopo_MSRing_hld$Routenaam_MS !=""])[,list(ID_Hoofdleiding,Routenaam_MS)]
assets$kabels = nettopo_MSRing_hld[assets$kabels]

# Add the HLD and MSRings to moffen ------------------------------
# Try to find the matching cable, this will not always work
a= Add_HLD(c("ID_Verbinding","Bronsysteem"), assets$moffen, assets$kabels,"ID_Hoofdleiding")  

# Try to find the matching cable using XY
setpbarwrapper(pb, 8,label = "Add the HLD and MSRings to moffen "); 

setnames(assets$kabels,c("Coo_X_naar","Coo_Y_naar"),c("Coo_X","Coo_Y"))
b = Add_HLD(c("Coo_X","Coo_Y"), assets$moffen, assets$kabels,"ID_Hoofdleiding")  
setnames(assets$kabels,c("Coo_X","Coo_Y"),c("Coo_X_naar","Coo_Y_naar"))
setnames(assets$kabels,c("Coo_X_van","Coo_Y_van"),c("Coo_X","Coo_Y"))
c = Add_HLD(c("Coo_X","Coo_Y"), assets$moffen, assets$kabels,"ID_Hoofdleiding")  
setnames(assets$kabels,c("Coo_X","Coo_Y"),c("Coo_X_van","Coo_Y_van"))

# Add to original dataset
assets$moffen[!is.na(a$ID_Hoofdleiding),ID_Hoofdleiding:=a[!is.na(a$ID_Hoofdleiding),ID_Hoofdleiding]]
assets$moffen[!is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding:=b[!is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding]]
assets$moffen[assets$moffen$ID_Hoofdleiding=="",ID_Hoofdleiding:=NA]
assets$moffen[is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding:=c[is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding]]
assets$moffen[assets$moffen$ID_Hoofdleiding=="",ID_Hoofdleiding:=NA]

# Add the MS Ring
setkey(assets$moffen,ID_Hoofdleiding)
assets$moffen = nettopo_MSRing_hld[assets$moffen]
remove(a,b,c)

# MSRing from dataset verbindingen
setkey(verbindingen,ID_Hoofdleiding)
setkey(assets$moffen,ID_Hoofdleiding)
assets$moffen = unique(verbindingen[,list(ID_Hoofdleiding,MS_Route_NOR_IDTrace)])[assets$moffen]
assets$moffen[MS_Route_NOR_IDTrace=="null",MS_Route_NOR_IDTrace:=NA]

remove(a,b,c,verbindingen)

# Save the data --------------------------------------------------
setpbarwrapper(pb, 9,label = "Saving to file"); 

save(assets,file=paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))

all_ID_NAN = c(unique(assets$kabels$ID_NAN),unique(assets$moffen$ID_NAN))

save(all_ID_NAN,file=paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_all_ID_NAN.Rda"))

setpbarwrapper(pb, 10,label = "Done"); 

}

Add_HLD = function(usekey,asset,verbinding,Return = "ID_Hoofdleiding") {
# This function merges data tables, originally intended to save some space in merging HLD data
# Syntax of the 4th element is "col to return 1,col2,col3"
setkeyv(verbinding,usekey)  
setkeyv(asset,usekey)
eval(parse(text=paste0("temp= unique(verbinding)[asset,j=list(Index,", Return, ")]")))
setkey(temp,Index)
return(temp)
}