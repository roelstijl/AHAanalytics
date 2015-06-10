# Written by Roel Stijl (Bearingpoint) (2014-2015)
# Used to derive the monthly change version of the NOR using first month as a basis
# Source can be backup or file, backups will be created every 6 months unless backup=FALSE

AHA_Data_NOR_Log = function(NORtable, datasource="file",backups=F){
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Converts the raw data into a combined file with all changes
# NORTable - Which table to use?
# datasource - file or backup, backup prompts you to select a backup Rda from an earlier run
# backups - make backups? Makes it slower, but easier to debug

cfg = list()
cfg$NORtable      = NORtable
cfg$datafolder    = paste0(settings$Ruwe_Datasets ,"/6. NOR");
cfg$outputfolder  = paste0(settings$Input_Datasets,"/6. NOR");
cfg$datasource    = datasource
cfg$firstfile = 1

cfg$files = list.files(pattern=paste0(cfg$NORtable,".*\\.Rda"), path=cfg$datafolder,full.names=TRUE)
cfg$filesshort  = list.files(pattern=paste0(cfg$NORtable,".*\\.Rda"), path=cfg$datafolder)
cfg$files=cfg$files[!grepl("masterdataset_backup",cfg$filesshort )]
cfg$filesshort =cfg$filesshort [!grepl("masterdataset_backup",cfg$filesshort )]
cfg$curdate = llply(cfg$filesshort,function(x) firstFri(gsub("[^0-9]","",x)))

cfg$pb = pbarwrapper(title = "AHA_Data_NOR_Log start", label = "Start", max = length(cfg$filesshort )*3+1+backups*length(cfg$filesshort )/3);

cfg$comparecols = switch (cfg$NORtable,
    ELCVERBINDINGSKNOOPPUNTEN=c("ID_unique", "ID_NAN","Bronsysteem","SpanningsNiveau","Beheerder", "Soort",  "Constructie","Fabrikanttype","Isolatiemedium",  "Fabrikant","Coo_X","Coo_Y"),
    ELCVERBINDINGSDELEN=c("ID_unique","Lengte","BRONNAN","Bronsysteem","ID_NAN","Status","Beheerder","Coo_X_van","Coo_Y_van","Coo_X_naar","Coo_Y_naar","Fabrikanttype","SpanningsNiveau"),
    ELCVERBINDINGEN=c("ID_unique","Lengte","Bronsysteem",	"SpanningsNiveau","Beheerder",	"Soort",	"Soortnet"))

# Plot to check for anomolies in file sizes
plot(file.info(cfg$files)$size)

# Load from backup functions
if (cfg$datasource == "backup") load(AHA_NOR_Load_Backup(cfg))

for (n in cfg$firstfile:length(cfg$files))
{
# Loop over the cfg$files to be imported importing them one at a time --------------------------
if (n<=cfg$firstfile & cfg$datasource=="file") {

masterdataset = AHA_NOR_Load_File(cfg,n)

} else {
mindataset = AHA_NOR_Load_File(cfg,n,masterdataset)

# Check which IDs have been removed and which added
setpbarwrapper(cfg$pb,  label = "Checking unique IDs that have been added or removed"); 
masterdataset[!(ID_unique %in% mindataset$ID_unique) & Status_ID != "Removed",DateRemoved := cfg$curdate[[n]]]
masterdataset[!(ID_unique %in% mindataset$ID_unique) & Status_ID != "Removed",Status_ID   := "Removed"]
masterdataset[(ID_unique %in% mindataset$ID_unique), Status_ID   := "Active"]

masterdataset = rbind(masterdataset,mindataset[!(mindataset$ID_unique %in% masterdataset$ID_unique )])

setpbarwrapper(cfg$pb,  label = "Logging changes in select cols"); 
cfg$relcompcols = cfg$comparecols[which(laply(cfg$comparecols,function(x) sum(is.na(mindataset[[x]]))/length(mindataset[[x]]))<1)]

combinedset = rbind(
  masterdataset[Status_ID == "Active",c(cfg$comparecols,"file"),with=F],
  mindataset[,c(cfg$comparecols,"file"),with=F])

setkeyv(combinedset,cfg$relcompcols); 
doubles = combinedset[((!duplicated(combinedset,fromLast=F) & !duplicated(combinedset,fromLast=T)))]
doubles[,Date:=cfg$curdate[[n]]]
 
# Collect changes and write them back to the masterdataset if needed
ifelse(!exists("changes"),{changes = doubles},{changes = rbind(changes, doubles,fill=T)})

cfg$notcomparecols = c(names(masterdataset)[!names(masterdataset) %in% cfg$comparecols],"ID_unique")

setkey(masterdataset,ID_unique)
setkey(mindataset,ID_unique)
mindataset[,Date_Last_Change:=cfg$curdate[[n]]]
masterdataset = rbind(masterdataset[!(masterdataset$ID_unique %in% doubles[file==n,ID_unique])],
                          mindataset[doubles[file==n,ID_unique],cfg$comparecols,with=F][masterdataset[doubles[file==n,ID_unique],cfg$notcomparecols,with=F]]
                      ,fill=T)

# Save backups every 6 cycles if on
if (n%%6 == 0 & backups) {
setpbarwrapper(cfg$pb,  label = "Saving backup");
save(changes,file=paste0(cfg$outputfolder,"/backup/masterdataset_backup_changes_",cfg$filesshort [n]));   
save(masterdataset,file=paste0(cfg$outputfolder,"/backup/masterdataset_backup_masterdataset_",cfg$filesshort [n]));   

setpbarwrapper(cfg$pb,  label = "Plotting");
try({barplot(rbind(table(masterdataset$DateRemoved)[2:n],table(masterdataset$DateAdded)[2:n]),beside=TRUE);  
 barplot(table(changes$Date))}
)}
}}

# Save to file ----------------------------------
setpbarwrapper(cfg$pb,  label = "Saving to file");

save(changes,file=paste0(cfg$outputfolder,"/changes_",cfg$NORtable,".Rda"),compress=F)   
save(masterdataset,file=paste0(cfg$outputfolder,"/masterdataset_",cfg$NORtable,".Rda"),compress=F)   

try({barplot(rbind(table(masterdataset$DateRemoved)[2:n],table(masterdataset$DateAdded)[2:n]),beside=TRUE);  
barplot(table(changes$Date))})
setpbarwrapper(cfg$pb,  label = "Done");

}

AHA_NOR_Load_Backup=function (cfg){
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Loads the backups if requested
  setpbarwrapper("Select backup file to continue from")
  cfg$bfile = file.choose()  
  setpbarwrapper("Select file to start import from")
  cfg$ffile = file.choose()
  cfg$firstfile = which(cfg$filesshort ==basename(cfg$ffile))
  
  ifelse (length(cfg$firstfile)==1,
          return(cfg$bfile),
          error("Wrong file selected"))

}

AHA_NOR_Load_File = function(cfg,n,masterdataset=NULL){
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Loads the backups if requested
# Determine date at which the file was created
# cfg - the configuration parameters
# n - the loop number
# masterdataset - the masterdataset
  par(mfrow=c(2,1))
  load(cfg$files[n])
  
  # Prepare the data set
  if(class(mindataset$Lengte)=="character") mindataset$Lengte = as.numeric(gsub(",",".",mindataset$Lengte))
  switch (cfg$NORtable,ELCVERBINDINGSKNOOPPUNTEN = {mindataset[,PC_2:=substr(PC_6,1,2)]},
          ELCVERBINDINGSDELEN       = {mindataset[,PC_2:=substr(PC_6_van,1,2)]})    
  
  # Define the unique ID composition
  ID_unique = switch (cfg$NORtable,
                      ELCVERBINDINGSDELEN       = mindataset[,ID_unique:=paste0(ID_Kabel,PC_2)],
                      ELCVERBINDINGEN           = mindataset[,ID_unique:=paste0(ID_Verbinding,ID_Hoofdleiding)],
                      ELCVERBINDINGSKNOOPPUNTEN = mindataset[,ID_unique:=paste0(ID_Bron,PC_2)])
  
  setkey(mindataset,ID_unique) 
  mindataset = unique(mindataset)
  
  mindataset[,file         := n]
  mindataset[,DateAdded    := cfg$curdate[n]]
  mindataset[,DateRemoved  := as.Date(NA)]
  mindataset[,Status_ID    := "Active"]
  
  if(n!=1 & any(!(colnames(masterdataset) %in% colnames(mindataset))))
  {set(mindataset,,colnames(masterdataset)[!(colnames(masterdataset) %in% colnames(mindataset))],(NA))}
  
  # Create the NAN number or Verbindingen if not present already
  if(!any(colnames(mindataset)=="ID_NAN")){mindataset$ID_NAN=as.character(NA)}
  if(!any(colnames(mindataset)=="BRONNAN")){mindataset$BRONNAN=as.character(NA)}
  
  if(!any(colnames(mindataset)=="Bronsysteem")){mindataset$Bronsysteem=as.character(NA)}
  if(!any(colnames(mindataset)=="ID_Verbinding"))
  {switch (cfg$NORtable, ELCVERBINDINGSKNOOPPUNTEN={mindataset$ID_Verbinding=as.character(NA)})}
  ifelse(n==1,
         return(mindataset),
         return(mindataset[,colnames(masterdataset),with=FALSE]))
}

AHA_Data_NOR_Log_Postprocessing  = function(){
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Postprocesses the data and combines it into a single all assets file
# Settings
cfg = list();
assets = list();
achanges = list();
cfg$recalculate_PC6_naar = F

cfg$pb = pbarwrapper(title = paste0("AHA_Data_NOR_Log_Postprocessing, ",as.character(Sys.time())), label = "Start", min = 0, max = 15, initial = 0, width = 450);

# Load the data --------------------------

# Load transformations
load("E:/1. Alliander/3. Asset Health Analytics/1. Ruwe Datasets/6. NOR/Status_tabel.Rda")
Status_tabel           = mindataset

load("E:/1. Alliander/3. Asset Health Analytics/1. Ruwe Datasets/6. NOR/Conversion_of_voltages.Rda")
Conversion_of_voltages = mindataset

# Load nettopology
load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_hoofdleidingen.Rda"))
MS_Hoofdleidingen = mindataset

load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/MS_Stations.Rda"))
MS_Stations       = mindataset

# Load hoofdleidingen
setpbarwrapper(cfg$pb, label = "Loading verbindingen"); 
load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGEN.Rda"))
masterdataset[,ID_Object := (1:nrow(masterdataset))]
setorder(masterdataset, -DateAdded, na.last=TRUE)
setkey(masterdataset,ID_Object)
masterdataset[ID_Verbinding=="",ID_Verbinding:=NA]
vb = masterdataset

# Load kabels  
setpbarwrapper(cfg$pb,label = "Loading verbindingsdelen"); 
load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSDELEN_XY_PC6.Rda"))
mindataset = Add_Status_Voltage (mindataset,Conversion_of_voltages,Status_tabel)
mindataset[,ID_Object := (1:nrow(mindataset))]
setorder(mindataset, -DateAdded, na.last=TRUE)

setkey(mindataset,ID_Object)
assets$kabels = mindataset;

# Load moffen
setpbarwrapper(cfg$pb, label = "Loading assets moffen"); 
load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSKNOOPPUNTEN_XY_PC6.Rda"))
mindataset = Add_Status_Voltage (mindataset,Conversion_of_voltages,Status_tabel)

mindataset[,ID_Object := (1:nrow(mindataset))]
setorder(mindataset,-DateAdded, na.last=TRUE)
setkey(mindataset,ID_Object)
assets$moffen = mindataset;

# Load moffen changes

setpbarwrapper(cfg$pb,label = "Loading verbindingsknooppunten changes"); 
load(paste0(settings$Input_Datasets,"/6. NOR/changes_ELCVERBINDINGSKNOOPPUNTEN.Rda"))

changes = Add_Status_Voltage (changes,Conversion_of_voltages,Status_tabel)

changes[,ID_Object := (1:nrow(changes))]
achanges$moffen = Transform_changes(changes)
achanges$moffen$Coo_XY = Merge_xy(achanges$moffen$Coo_X,achanges$moffen$Coo_Y,changes)

# Load kabels changes
setpbarwrapper(cfg$pb,label = "Loading verbindingsdelen changes"); 
load(paste0(settings$Input_Datasets,"/6. NOR/changes_ELCVERBINDINGSDELEN.Rda"))

changes = Add_Status_Voltage (changes,Conversion_of_voltages,Status_tabel)

changes[,ID_Object := (1:nrow(changes))]

changes[,Lengte_2 := sqrt((Coo_X_van-Coo_X_naar)^2+(Coo_Y_van-Coo_Y_naar)^2)]
achanges$kabels = Transform_changes(changes)
achanges$kabels$Coo_XY_XY = Merge_xy_xy(achanges$kabels$Coo_X_van,achanges$kabels$Coo_Y_van,achanges$kabels$Coo_X_naar,achanges$kabels$Coo_Y_naar,changes)

# Load the minumum required from the BAR for Verbindingen and Hoofdleidingen
setpbarwrapper(cfg$pb,label = "Load the BAR for hoofdleidingen"); 

load("E:/1. Alliander/3. Asset Health Analytics/1. Ruwe Datasets/1. BARlog/MH_NRG_MS_KABELS_XY_PC6.Rda")
BAR_data = mindataset[,list(ID_Hoofdleiding,ID_Verbinding,ID_NAN)]
load("E:/1. Alliander/3. Asset Health Analytics/1. Ruwe Datasets/1. BARlog/MH_NRG_LS_KABELS_XY_PC6.Rda")
BAR_data = rbind(BAR_data,mindataset[,list(ID_Hoofdleiding,ID_Verbinding,ID_NAN)])

# Remove the junk
rm("changes")
rm("masterdataset")
rm("mindataset")

# Recalculate what mof based on their XY coordinates --------------------
setpbarwrapper(cfg$pb, label = "Calculating historical Moffen"); 

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

# Transform the integer coordinates into oud
allXY[is.na(Coo_X_oud),Coo_X_oud:=Coo_X]
allXY[is.na(Coo_Y_oud),Coo_Y_oud:=Coo_Y]

# Loop back in the dates and add the historical ID of each uniqiue XY place
assets$moffen = unique(historical_topo(allXY)[,list(ID_unique,ID_unique_present,ID_NAN_present,ID_Verbinding_present)])[assets$moffen]

# Recalculate kabel based on their XY coordinates --------------------
setpbarwrapper(cfg$pb, label = "Calculating historical Kabels"); 

setkey(achanges$kabels$Coo_XY_XY,ID_unique)
setkey(assets$kabels,ID_unique)
setnames(achanges$kabels$Coo_XY_XY,"Date","Date_Length_Change")
allXY = achanges$kabels$Coo_XY_XY[,list(ID_unique,Coo_X_naar_oud,Coo_Y_naar_oud,Coo_X_van_oud,Coo_Y_van_oud,Date_Length_Change)][assets$kabels[
,list(DateAdded,Coo_X_van,Coo_Y_van,Coo_X_naar,Coo_Y_naar,ID_unique,ID_Object,ID_NAN,ID_Verbinding)],allow.cartesian=TRUE]

setnames(achanges$kabels$ID_NAN,"Date","Date_NAN_Change")
setorder(achanges$kabels$ID_NAN,-Date_NAN_Change)
setkey(achanges$kabels$ID_NAN,ID_unique)
allXY = unique(achanges$kabels$ID_NAN[,list(ID_unique,Date_NAN_Change)])[allXY]
allXY[!is.na(ID_NAN)&is.na(Date_NAN_Change)]
allXY[,Date_Last:=pmax(DateAdded,Date_NAN_Change,Date_Length_Change, na.rm = T)]

# Transform the integer coordinates into 1 vector
allXY[is.na(Coo_X_van_oud),Coo_X_van_oud:=Coo_X_van]
allXY[is.na(Coo_Y_van_oud),Coo_Y_van_oud:=Coo_Y_van]
allXY[is.na(Coo_X_naar_oud),Coo_X_naar_oud:=Coo_X_naar]
allXY[is.na(Coo_Y_naar_oud),Coo_Y_naar_oud:=Coo_Y_naar]

# Loop back in the dates and add the historical ID of each uniqiue XY place
assets$kabels = unique(historical_topo(allXY,byvars=c("Coo_X_naar","Coo_Y_naar","Coo_X_van","Coo_Y_van"))[,list(ID_unique,ID_unique_present,ID_NAN_present,ID_Verbinding_present)])[assets$kabels]

# Add hoofdleidingen using verbindingen
setnames(vb,"ID_Verbinding","ID_Verbinding_present")
setkey(vb,ID_Verbinding_present); setkey(assets$kabels,ID_Verbinding_present)
assets$kabels[,ID_Hoofdleiding_present:=unique(vb)[assets$kabels,ID_Hoofdleiding]]

# Add hoofdleidingen based on BAR
setnames(BAR_data,c("ID_NAN","ID_Hoofdleiding","ID_Verbinding"),c("ID_NAN_present","ID_Hoofdleiding_present_BAR","ID_Verbinding_present_BAR"))
setkey(BAR_data,ID_NAN_present)
BAR_data = unique(BAR_data)
setkey(assets$kabels,ID_NAN_present)
assets$kabels[,ID_Hoofdleiding_present_BAR := BAR_data[assets$kabels,ID_Hoofdleiding_present_BAR]]
assets$kabels[,ID_Verbinding_present_BAR := BAR_data[assets$kabels,ID_Verbinding_present_BAR]]

# Add the length changes -------------------
setpbarwrapper(cfg$pb, label = "Calculating length changes Kabels"); 

mergeset = rbind(achanges$kabels$Lengte[,lch := nieuw-oud],achanges$kabels$Lengte_2[,lch :=nieuw-oud])
setkey(mergeset,ID_unique,Date)
mergeset = mergeset[,mean(lch),by=list(ID_unique,Date)]
setnames(mergeset,c("Date","V1"),c("DateLength_ch","Length_ch"))
setkey(mergeset,ID_unique)
setkey(assets$kabels,ID_unique)
mergeset = assets$kabels[mergeset]
mergeset[,Status_ID:="Length_changed"]
mergeset[,DateRemoved:=NA]
assets$kabels = rbind(assets$kabels,mergeset,fill=T)

# Add the Status changes to cables ----------------------------
setpbarwrapper(cfg$pb, label = "Calculating status changes Kabels"); 

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

# Add the Route ----------------------------------------------------------
setpbarwrapper(cfg$pb, label = "Adding MSRing to Kabels");

setnames(MS_Hoofdleidingen,"ID_Hoofdleiding","ID_Hoofdleiding_present_BAR")
setkey(MS_Hoofdleidingen,ID_Hoofdleiding_present_BAR); 
setkey(assets$kabels,ID_Hoofdleiding_present_BAR)
assets$kabels[,Routenaam_Present:=unique(MS_Hoofdleidingen[Routenaam !=""])[assets$kabels,Routenaam]]

# Second on position
assets$kabels[!is.na(Coo_X_naar)&is.na(Routenaam_Present),Routenaam_Present:=nnsearch_kabel(assets$kabels,"Routenaam_Present")]

# Attach the cables to moffen ------------------------------
setpbarwrapper(cfg$pb, label = "Adding the HLD and MSRings to moffen ")

# First with Beheerder and Verbinding
setkey(assets$moffen,ID_Verbinding,Beheerder); setkey(assets$kabels,ID_Verbinding,Beheerder)
assets$moffen[,Routenaam_Present:=unique(assets$kabels)[assets$moffen,Routenaam_Present]]
assets$moffen[,ID_Hoofdleiding_present:=unique(assets$kabels)[assets$moffen,ID_Hoofdleiding_present]]
assets$moffen[,ID_Hoofdleiding_present_BAR:=unique(assets$kabels)[assets$moffen,ID_Hoofdleiding_present_BAR]]
assets$moffen[,ID_Verbinding_present_BAR:=unique(assets$kabels)[assets$moffen,ID_Verbinding_present_BAR]]

# Second with Verbinding
setkey(assets$moffen,ID_Verbinding,Beheerder); setkey(assets$kabels,ID_Verbinding,Beheerder)
assets$moffen[is.na(Routenaam_Present),Routenaam_Present:=unique(assets$kabels)[assets$moffen,Routenaam_Present][is.na(assets$moffen$Routenaam_Present)]]
assets$moffen[is.na(ID_Hoofdleiding_present),ID_Hoofdleiding_present:=unique(assets$kabels)[assets$moffen,ID_Hoofdleiding_present][is.na(assets$moffen$ID_Hoofdleiding_present)]]
assets$moffen[is.na(ID_Hoofdleiding_present_BAR), ID_Hoofdleiding_present_BAR:=unique(assets$kabels)[assets$moffen,ID_Hoofdleiding_present_BAR][is.na(assets$moffen$ID_Hoofdleiding_present_BAR)]]
assets$moffen[is.na(ID_Verbinding_present_BAR), ID_Verbinding_present_BAR:=unique(assets$kabels)[assets$moffen,ID_Verbinding_present_BAR][is.na(assets$moffen$ID_Verbinding_present_BAR)]]

# Third use NN for Hoofdleiding
assets$moffen[!is.na(Coo_X)&is.na(ID_Verbinding_present),ID_Verbinding_present:=nnsearch_kabel_mof(assets$kabels,assets$moffen,"ID_Verbinding_present")]
assets$moffen[!is.na(Coo_X)&is.na(Routenaam_Present),Routenaam_Present:=nnsearch_kabel_mof(assets$kabels,assets$moffen,"Routenaam_Present")]
assets$moffen[!is.na(Coo_X)&is.na(ID_Hoofdleiding_present),ID_Hoofdleiding_present:=nnsearch_kabel_mof(assets$kabels,assets$moffen,"ID_Hoofdleiding_present")]
assets$moffen[!is.na(Coo_X)&is.na(ID_Hoofdleiding_present_BAR),ID_Hoofdleiding_present_BAR:=nnsearch_kabel_mof(assets$kabels,assets$moffen,"ID_Hoofdleiding_present_BAR")]
assets$moffen[!is.na(Coo_X)&is.na(ID_Verbinding_present_BAR),ID_Verbinding_present_BAR:=nnsearch_kabel_mof(assets$kabels,assets$moffen,"ID_Verbinding_present_BAR")]

# Add the object ID (Index)
assets$moffen[,ID_Object:=1:nrow(assets$moffen)]
assets$kabels[,ID_Object:=1:nrow(assets$kabels)]

# Make sure the XY are plausible
assets$moffen[Coo_X<60000 | Coo_X>260000 |Coo_Y<300000|Coo_Y>650000, Coo_X:=NA]
assets$moffen[Coo_X<60000 | Coo_X>260000 |Coo_Y<300000|Coo_Y>650000, Coo_Y:=NA]
assets$kabels[Coo_X_van<60000 | Coo_X_van>260000 |Coo_Y_van<300000|Coo_Y_van>650000, Coo_X_van:=NA]
assets$kabels[Coo_X_van<60000 | Coo_X_van>260000 |Coo_Y_van<300000|Coo_Y_van>650000, Coo_Y_van:=NA]
assets$kabels[Coo_X_naar<60000 | Coo_X_naar>260000 |Coo_Y_naar<300000|Coo_Y_naar>650000, Coo_X_naar:=NA]
assets$kabels[Coo_X_naar<60000 | Coo_X_naar>260000 |Coo_Y_naar<300000|Coo_Y_naar>650000, Coo_Y_naar:=NA]

# Save the data --------------------------------------------------
setpbarwrapper(cfg$pb, label = "Saving to file"); 

save(assets,file=paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"),compress=F)

setpbarwrapper(cfg$pb, label = "Done"); 

}

Transform_changes= function(changes) {
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Other functions
# Function that transforms the useless data structure of changes into something more managable
setkey(changes,Date,ID_unique)
changes[,ID_Object := 1:nrow(changes)]
changes = changes[duplicated(changes)|duplicated(changes,fromLast=T)]
changes[,oldnew := rep(1:2,times=nrow(changes)/2)]
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
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
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
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
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

historical_topo = function(allXY, byvars=c("Coo_X","Coo_Y")){
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Used to calculate histortical nettopology
# allXY - the dataset with all the information, in a changelog format
# byvars - the variables to base the history on 
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

nearest2 = nn2(
allXY[!is.na(ID_NAN_present),byvars[1:2],with=F],          
allXY[is.na(ID_NAN_present),byvars[1:2],with=F],
k=1)$nn.idx

allXY[is.na(ID_NAN_present),ID_Verbinding_present:=allXY$ID_Verbinding_present[!is.na(allXY$ID_NAN_present)][nearest2]]
allXY[is.na(ID_NAN_present),ID_unique_present:=allXY$ID_unique_present[!is.na(allXY$ID_NAN_present)][nearest2]]
allXY[is.na(ID_NAN_present),ID_NAN_present:=allXY$ID_NAN_present[!is.na(allXY$ID_NAN_present)][nearest2]]

cat(paste0("is NOT NA: NAN, ",sum(!is.na(allXY$ID_NAN_present))," of ",nrow(allXY)," = ",round(sum(!is.na(allXY$ID_NAN_present))/nrow(allXY)*100)," %\n"))

setkey(allXY,ID_unique)
return(unique(allXY))

# Just some nice plots
a=allXY[,sum(!is.na(ID_NAN_present))/(sum(!is.na(ID_NAN_present))+sum(is.na(ID_NAN_present))),by=Date_Last]
setorder(a,by=Date_Last)
barplot(a$V1,names.arg=a$Date_Last)

}

Add_Status_Voltage = function(dataset,Conversion_of_voltages,Status_tabel){
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Just a simple wrapper to prevent some clutter
# Adds spanningsniveau and status details
setkey(Conversion_of_voltages,SpanningsNiveau)
setkey(Status_tabel,Status_Detail)

try(setnames(dataset,"Status","Status_Detail"))
try(setkey(dataset,Status_Detail))
try(dataset[,Status:=unique(Status_tabel)[dataset,Status]])

setkey(dataset,SpanningsNiveau)
dataset[,Voltage:=unique(Conversion_of_voltages)[dataset,Voltage]]
dataset[,Netvlak:=unique(Conversion_of_voltages)[dataset,Netvlak]]

return(dataset)
}

nnsearch_kabel_mof = function(kabelsset,moffenset,variable){
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Third use NN for Routenaam
# Fill in every NA in the dataset with its nearest neighbour cable
# kabelset - the set of cables to compare to moffen
# moffenset - the set of moffen to compare to cables
# variable - the variable to fill the nearest neighbour for
if(!any(names(moffenset)==variable)) 
  eval(parse(text=paste0( "moffenset[,",variable, ":= as.",
       class(kabelsset[[variable]])
       ,"(NA)]")))
nearest = nn2(kabelsset[!is.na(Coo_X_naar) & !is.na(kabelsset[[variable]]),list(Coo_X_naar,Coo_Y_naar)],moffenset[!is.na(Coo_X)&is.na(moffenset[[variable]]),list(Coo_X,Coo_Y)],k=1)
nearest2= nn2(kabelsset[!is.na(Coo_X_naar) & !is.na(kabelsset[[variable]]),list(Coo_X_van,Coo_Y_van)],moffenset[!is.na(Coo_X)&is.na(moffenset[[variable]]),list(Coo_X,Coo_Y)],k=1)
nnd = (nearest$nn.dists[,1]>=nearest2$nn.dists[,1])
nni = nearest$nn.idx[,1]; nni[nnd] = nearest2$nn.idx[nnd,1]
nni[!is.na(moffenset$Coo_X)&is.na(moffenset[[variable]])] = nni

output = kabelsset[[variable]][!is.na(kabelsset$Coo_X_naar)&!is.na(kabelsset[[variable]])][nni[!is.na(nni) & is.na(moffenset[[variable]])]]


return(output)
}

nnsearch_kabel = function(kabelsset,variable){
# Created by Roel Stijl (Bearingpoint) 2015
# for project Asset Health Analytics, Alliander
# Looks for the nearest cable to couple variable to 
# Fill in every NA in the dataset with its nearest neighbour
# kabelset - the set of couples to look in
# variable - the variable to fill the nearest neighbour for
nearest2 = nn2(
kabelsset[!is.na(Coo_X_naar)&!is.na(kabelsset[[variable]]),list(Coo_X_naar,Coo_Y_naar)],
kabelsset[!is.na(Coo_X_naar)&is.na(kabelsset[[variable]]),list(Coo_X_naar,Coo_Y_naar)],k=1)

output = rep(NA, nrow(kabelsset))

output[!is.na(kabelsset$Coo_X_naar)&is.na(kabelsset[[variable]])]
output = kabelsset[[variable]][!is.na(kabelsset$Coo_X_naar)&!is.na(kabelsset[[variable]])][nearest2$nn.idx[,1]]

return(output)
}