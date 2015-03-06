#Script to quickly test a single coupling operation, useful when implementing new features in AHA_MVA_Coupling.R

genericOutFileName=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/KoppelOutput")


Nfiles=11
InputFileList=list(as.character(1:Nfiles))
InputFileList[1]=paste0(settings$Ruwe_Datasets,"/15. CBS/CBS_Gecombineerd_Gemeente_Wijk_Buurt.Rda")
InputFileList[2]=paste0(settings$Ruwe_Datasets,"/16. Zakking/Zakking.Rda")
InputFileList[3]=paste0(settings$Ruwe_Datasets,"/18. KNMI/KNMI_grouped_2007_2014_RDS.Rda")
InputFileList[4]=paste0(settings$Ruwe_Datasets,"/23. Grondsoort/Grondsoorten_shp.Rda")
InputFileList[5]=paste0(settings$Ruwe_Datasets,"/14. Risicokaart/risicokaartXY.Rda")
InputFileList[6]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/Iso_hoogtelijn_XY.Rda")
InputFileList[7]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/spoorbaandeel_lijn_XY.Rda")
InputFileList[8]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/inrichtingselement_lijn_XY_boom.Rda")
InputFileList[9]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/inrichtingselement_lijn_XY_overig.Rda")
InputFileList[10]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/inrichtingselement_punt_XY_boom.Rda")
InputFileList[11]=paste0(settings$Ruwe_Datasets,"/13. Kadaster_TOP10_NL_Sept/inrichtingselement_punt_XY_overig.Rda")

SetNo=6
currentInFile=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/Proxylist.Rda")
currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
         key2_nameA="Coo_X",key2_nameB="Coo_Y",includeNNamount=1,amountRad=100,amountName="Aantal_Isolijn",
         amountIDname="Isolijn_ID",includeNNdist=1,NNdistName="Afstand_Isolijn",
         outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])

# SetNo=11
# currentInFile=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/KoppelOutput10.Rda")
# currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
# coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
#          key2_nameA="Coo_X",key2_nameB="Coo_Y",
#          includeNNdist=1,NNdistName="Afstand_Overige_Punt_Inrichtingselement",
#          outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])