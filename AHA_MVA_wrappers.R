#-----------------------------------------------------------------#
#-------- Written by Michiel Musterd (BearingPoint) 2015 ---------#
#-----------------------------------------------------------------#
# PURPOSE: 
# This file contains a few functions to call separate 
# parts of the coupling routine and is meant for testing purposes
# 
# INCLUDED FUNCTIONS (see each function for a description):
# ---
# CallProxyNORcouple()
# TestCoupling()
#-----------------------------------------------------------------#

AHA_MVA_wrappers = function(){
  cat("The functions in this file are for separate use in testing, please call them separately \n")
}


CallProxyNORcouple = function (){
  rm(list = setdiff(ls(), lsf.str()))
  .First()
  
  #settings for the NORfile and proxylist
  Settype="LSmoffen"
  NORfile=paste0(settings$Analyse_Datasets,"/2. CoupledWithoutProxy/MVA_Coupled_AnalysisSet_LSmoffen.Rda")
  Proxyfile=paste0(settings$Analyse_Datasets,"/1. Proxylijsten/Proxy_koppellijst_2015-03-27 21.20.07inclOorzaak.Rda")
  
  #load the files and clean some memory
  load(NORfile)
  load(Proxyfile)
  gc()
  
  #Select only outages where less then 6 assets will be coupled
  setkey(koppellijst[[Settype]],ID_KLAK_Melding)
  koppellijst[[Settype]] = koppellijst[[Settype]][(koppellijst[[Settype]][,list(too_many=(sum(punten>=0.3)>5)), by=ID_KLAK_Melding])][
    ,c(names(koppellijst[[Settype]]),"too_many"),with=F]
  koppellijst[[Settype]] = koppellijst[[Settype]][!koppellijst[[Settype]]$too_many,]
  
  
  #set filename to write to
  finalSetTargetOutFileName=paste0(settings$Analyse_Datasets,"/3. CoupledWithProxy/MVA_Final_",Settype,".Rda")
  
  #couple the punten into the set
  coupledSet=AHA_MVA_CoupleNORproxy(NORset=coupledSet,ProxySet=koppellijst,Settype=Settype)
  gc()
  
  
  #couple the target variables into the set for the list of thresholds
  coupledSet=AHA_MVA_TargetVariables(coupledNOR=coupledSet,threshold=list(0.3,0.5,1.0))
  
  #save the set and clean memory again
  save(coupledSet,file=finalSetTargetOutFileName,compress=F)
  gc()
  
  
  
}

TestCoupling = function(){
  #Script to quickly test a single coupling operation, useful when implementing new features in AHA_MVA_Coupling.R
  ptm=proc.time()
  
  
  genericOutFileName=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/KoppelOutput")
  
  
  Nfiles=17
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
  InputFileList[13]=paste0(settings$Ruwe_Datasets,"/2. Kabelgegevens/MS_HLD_belasting.Rda")
  InputFileList[14]=paste0(settings$Ruwe_Datasets,"/10. BAG/panden000_XY_clean.Rda")
  InputFileList[15]=paste0(settings$Ruwe_Datasets,"/11. Nettopologie/LS_HLDbelastingindicators.Rda")
  InputFileList[16]=paste0(settings$Ruwe_Datasets,"/19. CDB/MS_HLDbelastingindicators.Rda")
  
  
  
  #Belastingindicator coupling
  
  SetNo=16
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSet=coupling(no_of_keys=1,couple_method=0,key1_nameA="ID_Verbinding_present",key2_nameA="ID_Verbinding_present",
                      outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
  
  SetNo=13
  cat("Starting ",SetNo," coupling, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  coupledSetV2=coupling(no_of_keys=1,couple_method=0,key1_nameA="ID_Hoofdleiding_present",
                        key2_nameA="ID_Hoofdleiding_present",
                        outFileName="InMemory", Set1Name="InMemory",Set2Name=InputFileList[[SetNo]],memorySet=coupledSet)
  
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
  # SetNo=12
  # cat("Starting ",SetNo," coupling \n")
  # currentInFile=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/Proxylist.Rda")
  # currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  # (output=coupling(no_of_keys=1,couple_method=0,key1_nameA="Fabrikanttype",
  #          key2_nameA="Fabrikanttype",cleantextkey=1,
  #          outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]]))
  
  
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
  
  # #BAG panden - XY
  # SetNo=13
  # currentInFile=paste0(settings$Ruwe_Datasets,"/25. KoppelOutput/KoppelOutput11.Rda")
  # cat("Starting BAG panden coupling \n")
  # currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  # coupling(key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
  #          key2_nameA="Coo_X",key2_nameB="Coo_Y",pandensetRepeat=0,includeNNdist=1,NNdistName="Afstand_Pand",
  #          includeNNamount=1,amountRad=5,amountIDname="Gebouw_mID",amountName="Aantal_Panden_Binnen_5m",
  #          outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=InputFileList[[SetNo]])
  # 
  # for (i in 1:16){
  #   cat("Coupling pandenfile ",i," of 53, runtime (s):",proc.time()[3]-ptm[3] ," \n")
  #   if (i<10){
  #     Set2NameInput=paste0(settings$Ruwe_Datasets,"/10. BAG/panden00",i,"_XY_clean.Rda")
  #   }else{
  #     Set2NameInput=paste0(settings$Ruwe_Datasets,"/10. BAG/panden0",i,"_XY_clean.Rda")
  #   }
  #   currentInFile=currentOutFile
  #   currentOutFile=paste0(genericOutFileName,SetNo,".Rda")
  #   coupling(key1_nameA="Coo_X_van",key1_nameB="Coo_Y_van",
  #            key2_nameA="Coo_X",key2_nameB="Coo_Y",pandensetRepeat=1,includeNNdist=1,NNdistName="Afstand_Pand",
  #            includeNNamount=1,amountRad=5,amountIDname="Gebouw_mID",amountName="Aantal_Panden_Binnen_5m",
  #            outFileName=currentOutFile, Set1Name=currentInFile,Set2Name=Set2NameInput)
  # }
  
  proc.time()-ptm
}