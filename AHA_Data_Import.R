AHA_Data_Import= function(folder="automatic",dataname,headername=dataname,mode="save",override="no",ID_Object=F){
  
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
  
  # An attached excel sheet contains all the information for the import, the following date formats are allowed
  #  "date",         dmy
  #  "dateymd",      ymd
  #  "datetime",     my_hms
  #  "datetimeYDM",  ymd_hms
  #  "datetimeM",    dmy_hm
  #  "Hours minuts", hm
  
  # Do all the loading and modifying of files -------------------------------
  # Define the location of your data based on the system used
  cfg           = list()
  cfg$compress  = F
  cfg$started   = Sys.time()
  shinyfolder   = "Shiny"
  pb = pbarwrapper (title = "Import", label = "Starting...", min = 0, max = length(datafiles)*3, initial = 0, width = 450); pc=0;
  

  if (folder == "automatic"){
    filechooser= choose.files(default = paste0(settings$Bron_Datasets,"/*"))
    datafiles  = basename(filechooser)
    folder     = strsplit(tail(strsplit(dirname(filechooser),"/")[[1]],1)," ")[[1]][2]
    curdataname = file_path_sans_ext(datafiles[[1]])
    setfolder  = tail(strsplit(dirname(filechooser),"/")[[1]],1)
    headerfile = paste0(settings$Ruwe_Datasets,"/",setfolder,"/",curdataname,"_headers.xlsx")
  } 
  else{
    setfolder     = list.files(settings$Bron_Datasets,pattern=folder)[1]; 
    datafiles     = list.files(paste0(settings$Bron_Datasets,"/",setfolder),
                               pattern=paste0(dataname,".{0,}\\.csv|",
                                              dataname,".{0,}\\.ssv|",
                                              dataname,".{0,}\\.tsv|",
                                              dataname,".{0,}\\.xlsx|",
                                              dataname,".{0,}\\.ldr|",
                                              dataname,".{0,}\\.dbf|",
                                              dataname,".{0,}\\.shp"))
    headerfile    = paste0(settings$Ruwe_Datasets, "/", setfolder,"/",headername,"_headers.xlsx")
  }
  
  if(isempty(datafiles)){warning("File not found! \n")}
  if(isempty(setfolder)){warning("Source folder not found! \n")}
  
  dir.create(paste0(settings$Ruwe_Datasets,"/",setfolder), showWarnings = FALSE)
  
  # Import data and rename cols
  for (filenumber in 1:length(datafiles))
  {
    sourcefile  = paste0(settings$Bron_Datasets,"/",setfolder,"/",datafiles[filenumber])
    curdataname = file_path_sans_ext(datafiles[[filenumber]])
    curdataext  = file_ext(datafiles[[filenumber]])
    
    setpbarwrapper (pb, title = paste0("AHA_Data_Import, Started:",cfg$started," ,file: ",datafiles[filenumber]), label = "Starting import"); 
    
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
                          
                          xlsx= {data.frame(read.xlsx(sourcefile,1))},
                          
                          shp = {spatialset = readShapeSpatial(sourcefile)
                                 spatialsetdataframe = spatialset
                                 mindataset = spatialset@data
                                 #spatialset = SpatialPolygons(spatialset@polygons,proj4string=CRS("+init=epsg:28992"))
                                 mindataset},
                          
                          dbf = {read.dbf(sourcefile)}
    )
    
    # Convert header into the same format as the xlsx file --------------------------------
    setpbarwrapper (pb, label = paste0("Converting header")); 
    
    header       = data.frame(matrix(0,length(colnames(mindataset)),4))
    header[,1]   = data.frame(colnames(mindataset))
    header[,2]   = (colnames(mindataset))
    header[,3]   = matrix(0,ncol(mindataset))
    header[,4]   = matrix("comment",length(colnames(mindataset)))
    header[,5]   = sapply(mindataset, class,simplify=TRUE);
    header[,6:8] = t(mindataset[1:3,])
    colnames(header) = c(curdataname,"Original name","Meenemen","Notities","Class","Value 1","Value 2","Value 3")
    
    # Load the xlsx file or create it if non existing
    if (file.exists(headerfile)) {
      savedheader   = read.xlsx(headerfile,1, as.data.frame=TRUE)
    }else{   setpbarwrapper (pb, pc+2, label = "Creating new header file");

           header[,3]   = matrix(1,length(colnames(mindataset)))
           write.xlsx(header,file=headerfile,row.names=FALSE)
           savedheader  = header}
    
    # Check if the headers saved and actual are equal and correct where not
    pat    = pmatch(header[,2], savedheader[,2], dup = TRUE,nomatch=0)
    header[pat>0,] = savedheader[pat[pat>0],]
    setnames(mindataset,colnames(mindataset), t(header[1]))
    
# Set colclasses to the desired (excel sheet)
for(i in header[header[,5]=="numeric",1]) {mindataset[,i] = as.numeric(gsub(",",".",mindataset[,i]))}
for(i in header[header[,5]=="integer",1]) {mindataset[,i] = as.integer(gsub(",",".",mindataset[,i]))}
for(i in header[header[,5]=="logical",1]) {mindataset[,i] = as.logical(gsub(",",".",mindataset[,i]))}

# The different date formats
for(i in header[header[,5]=="date"|header[,5]=="dmy",1])           {mindataset[,i] = as.Date(dmy(mindataset[,i]))} #Timezone note taken into account for perforamnce
for(i in header[header[,5]=="dateymd"|header[,5]=="ymd",1])        {mindataset[,i] = as.Date(ymd(mindataset[,i]))} #Timezone note taken into account for perforamnce
for(i in header[header[,5]=="datetime"|header[,5]=="my_hms",1])    {mindataset[,i] = as.Date(my_hms(mindataset[,i]))}
for(i in header[header[,5]=="datetimeYDM"|header[,5]=="ymd_hms",1]){mindataset[,i] = as.Date(ymd_hms(mindataset[,i]))}
for(i in header[header[,5]=="datetimeM"|header[,5]=="dmy_hm",1])   {mindataset[,i] = as.Date(dmy_hm(mindataset[,i]))}

# Correct for missing information if 2 digit year in the 20th century
l_ply(names(mindataset[sapply(mindataset,class)=="Date"]),
  function(x) {mindataset[which(mindataset[,x] > "2015-06-01"),x] = mindataset[which(mindataset[,x] > "2015-06-01"),x] - years(100)}
      )    
# Choose what output to generate depending on user selection ---------------------------
if(mode=="shiny"){
  
  # Shiny visualisation
  shinyfolder  = "Shiny"
  dataset   <<- mindataset[sample(nrow(mindataset),min(nrow(mindataset),10000)),]
  remove ("mindataset")
  header    <<- header  
  setpbarwrapper (pb, label = paste0("Starting shiny" )); 
  header = runApp(shinyfolder)
  setpbarwrapper (pb, label = paste0("Saving to file")); 
  file.rename(paste0(shinyfolder ,"/header.xlsx"),paste0(shinyfolder,"/",basename(headerfile)));
  
  file.copy(paste0(shinyfolder,"/",basename(headerfile)),paste0(settings$Ruwe_Datasets, "/", setfolder,"/",basename(headerfile)),overwrite=TRUE);      
} 
else if(mode=="load") {
  
  # Load to memory
  cat("Done\n");    return(mindataset[,header[header[,3]==1,1]])
} else if(mode=="save") {
  setpbarwrapper (pb, label = paste0("Saving to file, rows: ", nrow(mindataset)," cols: ",ncol(mindataset)) ); 
  
  dataclasses= sapply(mindataset, class)
  mindataset = data.table(mindataset[,header[header[,3]==1,1]])
  setkeyv(mindataset, colnames(mindataset)[1])
  # Save to file
  savefile = paste0(settings$Ruwe_Datasets, "/", setfolder,"/",curdataname,".Rda")
  if (ID_Object) mindataset[,ID_Object:=1:nrow(mindataset)]
  
  if(curdataext=="shp") {
    save(spatialsetdataframe,mindataset,dataclasses,file=savefile,compress=cfg$compress)} 
  else{
    save(mindataset,dataclasses,file=savefile,compress=cfg$compress)
  }
  
  setpbarwrapper (pb, title = paste0("AHA_Data_Import,file: ",datafiles[filenumber]), label = "Done!");
  
}else if(mode=="header"){
  setpbarwrapper (pb, pc+2, label = "Saved header file to xlsx");
  
}else{
  cat("Wrong mode selected, load, save or shiny\n")
} } }

  
