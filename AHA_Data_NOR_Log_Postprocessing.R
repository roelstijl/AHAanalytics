AHA_Data_NOR_Log_Postprocessing  = function(){
# Load and prepare some data --------------------------------------------------
  
assets = list(); changes = list();
Conv_voltage = setkey(unique(data.table(read.xlsx(paste0(settings$Ruwe_Datasets,"/6. NOR/Conversion_of_voltages.xlsx"),1))),Spanningsniveau)  
pb = tkProgressBar(title = paste0("AHA_Data_NOR_Log_Postprocessing, ",as.character(Sys.time())), label = "Start", min = 0, max = 5, initial = 0, width = 450);
paste0("\nStarted at: ",as.character(Sys.time()),"\n" )


# hoofdleidingen koppel ----------------------
setTkProgressBar(pb, 1,label = "Loading verbindingen"); 

load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGEN.Rda"))
verbindingen = masterdataset[!is.na(masterdataset$ID_Hoofdleiding)]
verbindingen = unique(setorder(verbindingen, "DateAdded"),by=c("ID_Verbinding","Beheerder","ID_NAN"))

# kabels ---------------
# Load the data
setTkProgressBar(pb, 2,label = "Loading verbindingsdelen"); 

load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSDELEN.Rda"))
assets$kabels = masterdataset; rm("masterdataset")

# Add the correct voltage levels
setTkProgressBar(pb, 3,label = "Beginning calculations"); 

setkey(assets$kabels,Spanningsniveau);   
assets$kabels = Conv_voltage[assets$kabels]; 

# Generate a file for missing PC6_naar in NOR. Has to be run once on a new load!!
# Takes a long time to calculate ....
if(TRUE){
  XYinPC = AHA_Data_Determine_PC(
    assets$kabels[,c("Coo_X_naar","Coo_Y_naar","ID_unique","PC_6_van"),with=FALSE],
    "Coo_X_naar","Coo_Y_naar","PC_6_naar")
  save(XYinPC,file=paste0(settings$Input_Datasets,"/6. NOR/XYinPC.Rda"))}

# Load the PC6_Naar files for a new data
setTkProgressBar(pb, 4,label = "Loading XY in PC"); 

load(paste0(settings$Input_Datasets,"/6. NOR/XYinPC.Rda")); 
try(assets$kabels[,PC_6_naar:=NULL])
setnames(XYinPC,"PC_6","PC_6_naar"); 
setkeyv(XYinPC,c("Coo_X_naar","Coo_Y_naar")); 
setkeyv(assets$kabels,c("Coo_X_naar","Coo_Y_naar"))
assets$kabels= merge(assets$kabels,unique(XYinPC)[,list(PC_6_naar,Coo_X_naar,Coo_Y_naar)])
remove("XYinPC")

# Add the length changes
setTkProgressBar(pb, 5,label = "Loading verbindingsdelen changes"); 

load(paste0(settings$Input_Datasets,"/6. NOR/changes_ELCVERBINDINGSDELEN.Rda"))
setkey  (changes,ID_unique,Date)
setkey  (assets$kabels,ID_unique)
new.value= 2*(1:(nrow(changes)/2)); 
lengthch.vec = changes$Lengte[new.value-1]-changes$Lengte[new.value]; 
lengthch = assets$kabels[changes[new.value-1][lengthch.vec!=0,list(ID_unique,Date)][,Length_ch:=lengthch.vec[lengthch.vec!=0]]]

remove("changes");
setnames(lengthch,"Date","DateLength_ch")
lengthch$Status_ID = "Length_changed"
assets$kabels = rbind(assets$kabels,lengthch,fill=TRUE); remove("lengthch");
  
# Add the HLD and MSRings to kabels ------------------------------
setTkProgressBar(pb, 6,label = "Add the HLD and MSRings to kabels"); 

assets$kabels$Index = (1:length(assets$kabels$ID_NAN)) # Some weird bug required this inefficient syntax

# Try 3 methods in order of accuracy
a=Add_HLD(c("ID_Verbinding","Beheerder"), assets$kabels, verbindingen)
b=Add_HLD(c("ID_NAN"), assets$kabels, verbindingen)
c=Add_HLD(c("ID_Verbinding"), assets$kabels, verbindingen)

# Combine with the asset data in order a,b,c
assets$kabels[!is.na(a$ID_Hoofdleiding),ID_Hoofdleiding:=a[!is.na(a$ID_Hoofdleiding),ID_Hoofdleiding]]
assets$kabels[!is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding:=b[!is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding]]
assets$kabels[is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding:=c[is.na(b$ID_Hoofdleiding)&is.na(a$ID_Hoofdleiding),ID_Hoofdleiding]]
assets$kabels[assets$kabels$ID_Hoofdleiding=="",ID_Hoofdleiding:=NA]

remove(a,b,c,verbindingen)

# Add the MSRing

load(paste0(settings$Ruwe_Datasets,"/11. Nettopologie/nettopo_MSHLD_MSRing.Rda"))
try(setnames(nettopo_MSRing_hld,c("Routenaam","Nummer","NAN"),c("Routenaam_MS","ID_Hoofdleiding","ID_NAN_HLD")))

setkey(nettopo_MSRing_hld,ID_Hoofdleiding)
setkey(assets$kabels,ID_Hoofdleiding)
nettopo_MSRing_hld = unique(nettopo_MSRing_hld[nettopo_MSRing_hld$Routenaam_MS !=""])[,list(ID_Hoofdleiding,Routenaam_MS)]
assets$kabels = nettopo_MSRing_hld[assets$kabels]

# Moffen --------------------------
setTkProgressBar(pb, 7,label = "Loading assets moffen"); 

# Load the data
load(paste0(settings$Input_Datasets,"/6. NOR/masterdataset_ELCVERBINDINGSKNOOPPUNTEN.Rda"))
assets$moffen = masterdataset; rm("masterdataset")
assets$moffen$Index = (1:length(assets$moffen$ID_NAN)) 
  
# Add the HLD and MSRings to moffen ------------------------------
# Try to find the matching cable, this will not always work
a= Add_HLD(c("ID_Verbinding","Beheerder"), assets$moffen, assets$kabels,"ID_Hoofdleiding")  

# Try to find the matching cable using XY
setTkProgressBar(pb, 8,label = "Add the HLD and MSRings to moffen "); 

setnames(assets$kabels,c("Coo_X_naar","Coo_Y_naar"),c("Coo_X","Coo_Y"))
b = Add_HLD(c("Coo_X","Coo_Y"), assets$moffen, assets$kabels,"ID_Hoofdleiding")  
setnames(assets$kabels,c("Coo_X_van","Coo_Y_van","Coo_X","Coo_Y"),c("Coo_X","Coo_Y","Coo_X_naar","Coo_Y_naar"))
c = Add_HLD(c("Coo_X","Coo_Y"), assets$moffen, assets$kabels,"ID_Hoofdleiding")  

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
  
# Add the spanningsniveau information
setkey(assets$moffen,Spanningsniveau)
assets$moffen = Conv_voltage[assets$moffen]

# Save the data --------------------------------------------------
setTkProgressBar(pb, 9,label = "Saving to file"); 

save(assets,file=paste0(settings$Input_Datasets,"/2. All Assets/Asset_Data_NOR_assets.Rda"))
 
setTkProgressBar(pb, 10,label = "Done"); 

}

Add_HLD = function(usekey,asset,verbinding,Return = "ID_Hoofdleiding") {
  # This function merges data tables, originally intended to save some space in merging HLD data
  # Syntax of the 4th element is "col to return 1,col2,col3"
  setkeyv(verbinding,usekey)  
  setkeyv(asset,usekey)
  try(setnames(verbinding,"Index","NOT USED"))
  #   temp= unique(verbinding)[asset,j=list(Index,ID_Hoofdleiding)] 
  eval(parse(text=paste0("temp= unique(verbinding)[asset,j=list(Index,", Return, ")]")))
  setkey(temp,Index)
  return(temp)
}