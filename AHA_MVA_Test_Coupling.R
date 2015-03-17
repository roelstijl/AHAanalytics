#Script to quickly test a single coupling operation, useful when implementing new features in AHA_MVA_Coupling.R
ptm=proc.time()


genericOutFileName=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/KoppelOutput")


Nfiles=13
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
InputFileList[12]=paste0(settings$Ruwe_Datasets,"/2. Kabelgegevens/KoppelFabrikanttypeBelasting.Rda")
InputFileList[13]=paste0(settings$Ruwe_Datasets,"/10. BAG/panden000_XY_clean.Rda") #should be the first in the sequence


# SetNo=6
# currentInFile=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/Proxylist.Rda")
# currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
# coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
#          key2_nameA="Coo_X",key2_nameB="Coo_Y",includeNNamount=1,amountRad=100,amountName="Aantal_Isolijn",
#          amountIDname="Isolijn_ID",includeNNdist=1,NNdistName="Afstand_Isolijn",
#          outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])

# SetNo=11
# currentInFile=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/KoppelOutput10.Rda")
# currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
# coupling(no_of_keys=2,couple_method=1,key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
#          key2_nameA="Coo_X",key2_nameB="Coo_Y",
#          includeNNdist=1,NNdistName="Afstand_Overige_Punt_Inrichtingselement",
#          outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])

#Max belasting kabel 
SetNo=12
cat("Starting ",SetNo," coupling \n")
currentInFile=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/Proxylist.Rda")
currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
(output=coupling(no_of_keys=1,couple_method=0,key1_nameA="Fabrikanttype",
         key2_nameA="Fabrikanttype",cleantextkey=1,
         outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]]))


# SetNo=12
# currentInFile=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/Proxylist.Rda")
# currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
# (output=coupling(key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
#             key2_nameA="Coo_X",key2_nameB="Coo_Y",pandensetRepeat=0,includeNNamount=1,
#             includeNNdist=1,NNdistName="Afstand_Pand",
#             amountRad=5,amountIDname="Gebouw_mID",amountName="Aantal_Panden_Binnen_5m",
#             outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]]))
# 
# InputFileNew=paste0(settings$Ruwe_Datasets,"/10. BAG/panden001_XY_clean.Rda")
# currentInFile=currentOutFile
# currentOutFile=paste0(genericOutFileName,SetNo,"Next.Rda")
# (output=coupling(key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
#                  key2_nameA="Coo_X",key2_nameB="Coo_Y",pandensetRepeat=1,includeNNamount=1,
#                  includeNNdist=1,NNdistName="Afstand_Pand",
#                  amountRad=5,amountIDname="Gebouw_mID",amountName="Aantal_Panden_Binnen_5m",
#                  outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileNew))

proc.time()-ptm