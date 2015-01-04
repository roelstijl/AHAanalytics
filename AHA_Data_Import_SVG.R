AHA_Data_Import_SVG = function(){
  
  load(paste0(settings$Ruwe_Datasets,"/17. Storingsschetsen/OBJ3700673_STORINGSSCHETSFORMU_DATA_TABLE_MAIN.Rda"))
  schetstabel = mindataset
  
  flist = list.files(paste0(settings$Bron_Datasets,"/17. Storingsschetsen"),pattern = ".ldr",full.names=TRUE)
  flist = flist[2:length(flist)]
  pb = tkProgressBar (title = "Import", label = "Starting...", min = 0, max = length(flist), initial = 0, width = 900); pc=1;
  
  for (fileName in flist){
    setTkProgressBar (pb, pc, label = paste0("File: ", pc, " of ", length(flist),": ",basename(fileName))); pc=pc+1
  
    if (sum(schetstabel$Storingsschets %in% basename(fileName))==1)
      {
      
      ID_KLAK = schetstabel[Storingsschets == basename(fileName),ID_KLAK_Melding]
      Plaats = schetstabel[Storingsschets == basename(fileName),Plaats]
      
      a=readChar(fileName, file.info(fileName)$size)  
      b=gregexpr(pattern ='</svg>',a)
      c=substring(a,1,b[[1]][1]+5)
      d=paste0("<!DOCTYPE html><html><body><h1>KLAK melding: ",ID_KLAK,", Plaats: ",Plaats,"</h1>",c,"</body></html>")
      
      fileConn<-save(c,file=paste0(settings$Ruwe_Datasets,"/17. Storingsschetsen/Storingsschets, ID_KLAK ", ID_KLAK,".R"))
      fileConn<-file(paste0(settings$Ruwe_Datasets,"/17. Storingsschetsen/Storingsschets, ID_KLAK ", ID_KLAK,".html"))
      writeLines(d, fileConn)
      close(fileConn)  
    }
  }
}