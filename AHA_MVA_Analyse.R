#-----------------------------------------------------------------#
#-------- Written by Michiel Musterd (BearingPoint) 2015 ---------#
#-----------------------------------------------------------------#
# PURPOSE: 
# This file contains several functions that are used in coupling
# the NOR dataset to a variety of internal and external datasources.
# The core of this file is coupling() which performs the coupling 
# itself, whereas other functions perform brief calculations or
# more specialized types of coupling
#
# 
# INCLUDED FUNCTIONS (see each function for a description):
# ---
# AHA_MVA_Analyse() - batch to run all coupling
# MVA_Actual_Analyse(methode, analyse, imputatie, aantalimp, aantalboom, minsplit, minbucket , cp ,inputfilename){
# BinGeographic = function(inputset,N)
#-----------------------------------------------------------------#


AHA_MVA_Analyse = function (){
  
  rm(list = setdiff(ls(), lsf.str()))
  .First()
  gc()
  cat("Note that the alldata set and the metadata already have to exist for 
      this script to run, output them first with the shiny preprocessing tool \n")
  
  #############################
  # Settings                  #
  #############################
  threshold=0.3
  settype="LSkabels"
  Ntrain=500000
  Ntest=1000000
  PercTrain=1
  Target_val="T"
  Target_var="gestoordAsset_th0.3"
  removeColumns=c("ID_unique","ID_unique_present","ID_NAN","ID_NAN_present")
  #binColumns=c("Afstand_Boom","Afstand_Overige_Punt_Inrichtingselement","Afstand_Spoorbaan","Afstand_Pand","Coo_X_van","Coo_Y_van")
  binColumns=""
  
  #NOTITIES:
  #OMBOUWEN SCRIPT ZODAT IK 7-FOLD RUNS KAN DOEN EN MAAR 1 UITEINDELIJKE OUTPUT KRIJG AAN KANSEN
  #VERDER OMBOUWEN TOT FUNCTIE
  
  #STUKJE SAMPLECODE WAARMEE IK DE RFs kan combineren
  # set.seed(42)
  # library(randomForest)
  # rf1 <-randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE) 
  # rf2 <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE) 
  # rf3 <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE) 
  # 
  # rf.all <- combine(rf1, rf2, rf3) 
  # predict(rf.all, type='prob')
  
  
  #############################
  #Load the dataset           #
  #############################
  ptmOrg=proc.time()[3]
  ptm=ptmOrg
  filename=paste0("MVA_Coupled_AnalysisSet_",settype,"_singleKLAK")
  load(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_alldata.Rda"))
  cat("Alldata loaded in ",proc.time()[3]-ptm," s \n")
  
  #########################################
  #Remove NAs by imputing mean or onbekend#
  #########################################
  ptm=proc.time()[3]
  if (exists("mindataset")==T){alldata=mindataset
                               rm(mindataset)}
  
  gc()
  for (i in names(alldata))
  {
    #karakters imputeren en omzetten naar factoren
    if (is.character(alldata[,get(i)]))
    {
      alldata[is.na(get(i)),eval(i):= "Onbekend"]
      alldata[,eval(i):=factor(alldata[,get(i)])]
    }
    #factoren imputeren (daarvoor moeten ze eerst karakter worden...)
    if (is.factor(alldata[,get(i)]))
    {
      alldata[,eval(i):= as.character(alldata[,get(i)])]
      alldata[is.na(get(i)),eval(i):= "Onbekend"]
      alldata[,eval(i):=factor(alldata[,get(i)])]
    }
    if (is.numeric(alldata[,get(i)]))
    {
      alldata[is.na(get(i)),eval(i):= mean(alldata[,get(i)], na.rm=TRUE)]
    }
  }
  
  save(alldata,metadata,cfg,file = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_alldataImputed.Rda"),compress=F)
  cat("Imputation performed and file saved in ",proc.time()[3]-ptm," s \n")
  
  ############################
  #Create test and train sets#
  ############################
  
  #set settings
  ptm=proc.time()[3]
  st= data.table(Tr_size=as.numeric(Ntrain),
                 Tr_tgt=as.numeric(PercTrain)/100,
                 tst_size=as.numeric(Ntest),
                 rnd_seed=as.numeric(10),
                 Target_Value=Target_val, 
                 Target_Variable=Target_var)
  
  
  alldata=alldata[,setdiff(names(alldata),removeColumns),with=F]
  
  #BIN THE COLUMNS HERE
  binnedColumns=AHA_MVA_Bin(alldata[,binColumns,with=F],N=10)
  alldata[,binColumns:=NULL,with=F]
  alldata=cbind(alldata,binnedColumns)
  
  #Create test and trainset from the already imputed and corrected set
  testrows = rep(F,nrow(alldata))
  testrows[sample(1:nrow(alldata),st$tst_size)] = T
  trainrow = c(sample(which(alldata[[st$Target_Variable]]==st$Target_Value & !testrows),st$Tr_size*(st$Tr_tgt)),
               sample(which(alldata[[st$Target_Variable]]!=st$Target_Value & !testrows),st$Tr_size*(1-st$Tr_tgt)))
  testrows = which(testrows)
  
  testset  = alldata[testrows,]
  trainset = alldata[trainrow,]
  
  save(testset,trainset,metadata,cfg,file = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_test_train_set.Rda"),compress=F)
  #write.table(testset,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_testset.csv"),sep = ";",na="",row.names = F);
  #write.table(trainset,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_trainset.csv"),sep = ";",na="",row.names = F);
  cat("Test and trainsets created and saved in ",proc.time()[3]-ptm," s \n")
  
  ############################
  #Run random forest analysis
  ############################
  ptm=proc.time()[3]
  paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_test_train_set.Rda")
  uitkomst=AHA_MVA_Analyse(aantalimp=1,inputfilename=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_test_train_set.Rda"))
  cat("Random forest run done in ",proc.time()[3]-ptm," s --- for trainsize: ",st$Tr_size,"\n")
  
  #calculate failure probabilities 
  probs=data.table(predict(uitkomst$resultaat[[1]],alldata,"prob"));
  setnames(probs,names(probs),c("P_Nietgestoord","P_Gestoord"))
  gc()
  
  ###########################################################
  #Load the original NOR dataset and couple P_fail into it  #
  ###########################################################
  setname=load(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/",filename,".Rda"))
  fullSet=get(setname)
  
  Pfailname=paste0("P_faal",Target_var)
  
  fullSet[,eval(Pfailname):=probs$P_Gestoord]
  
  cat("Total time taken (in minutes): ",(proc.time()[3]-ptmOrg)/60,"\n")
  save(fullSet,file=paste0(settings$Analyse_Datasets,"/6. MVA output/",filename,Pfailname,"PT2.Rda"),compress=F)
  save(uitkomst,file=paste0(settings$Analyse_Datasets,"/6. MVA output/",filename,Pfailname,"_modelPT2.Rda"),compress=F)
  
  #############################################################################
  #Save a sorted list of failprone assets and export model metrics (ROC etc)  #
  #############################################################################
  #save ROC curve
  jpeg(paste0(settings$Analyse_Datasets,"/6. MVA output/",filename,Pfailname,"_ROCPT2.jpg"),
       width = 1920, height = 1920,quality=100,pointsize = 40)
  plot(uitkomst$plotje)
  dev.off()
  
  #save rule ordering
  jpeg(paste0(settings$Analyse_Datasets,"/6. MVA output/",filename,Pfailname,"_RulesPT2.jpg"),
       width = 3410, height = 1920,quality=100,pointsize = 40)
  varImpPlot(uitkomst$resultaat[[1]])
  dev.off()
  
  
}


MVA_Actual_Analyse = function(methode = "RF", analyse = "testtrain", imputatie = "preprocess", aantalimp = 5, aantalboom = 500, minsplit = 60, minbucket = 20, cp = 0.001,inputfilename="-"){
  # Multivariate analyse uitvoeren
  
  # methode:
  #  RF = random forests; standaard
  #  LR = logistische regressie
  #  DT = beslisbomen
  
  # analyse:
  #  testtrain = laad test- en trainset uit preprocessing in; standaard
  #  fullset = laad volledige set uit preprocessing in (nog niet goed geïmplementeerd)
  
  # imputatie (invullen van missende waardes):
  # enige vorm van imputatie is benodigd voor random forests en logistische regressie; factoren en karakters worden altijd geïmputeerd (NA -> categorie "onbekend")!
  #  Amelia = imputatie op basis van Ameliapackage; aantal imputaties wordt bepaald door "aantalimp"; standaard
  #  geen = geen imputatie; logistische regressie en random forests werken hierdoor mogelijk niet; voor beslisbomen wordt de ingebouwde functionaliteit gebruikt
  #  compleet = zowel van de test- als trainset worden alleen de volledige rijen meegenomen in de analyse; zorg dat je wel genoeg rijen overhoudt zodat alle categorieën meer dan eenmaal voorkomen
  #  gemiddelde = imputatie met gemiddelde      
  
  # aantalimp
  # aantal imputaties voor het Amelia-algoritme; standaardwaarde = 5 (is ook standaard in package)
  
  # aantalboom
  # aantal bomen dat random forest groeit per imputatie; standaardwaarde = 500
  
  # minsplit, minbucket, cp
  # parameters voor beslisboomalgoritme; zie ?rpart.control (indien package geladen) voor meer info
  # ter info: in rpartpackage zijn standaardwaardes anders dan hier, namelijk (20, round(20/3), 0.01)
  
  
  # laden relevante packages
  require(randomForest);
  require(pROC);
  require(Amelia);
  require(rpart);
  
  # Deze code voert de MVA analyse uit, volgt op de preprocessing
  switch(analyse,
         testtrain= {  
           if (inputfilename=="-"){
           cat("Kies een test / train set... \n")
           filechooser = choose.files(default = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/*.Rda"))
           filename    = file_path_sans_ext(basename(filechooser))
           load(filechooser)}
           else{
             load(inputfilename)
           }
           
           trainset=data.table(trainset)
           testset=data.table(testset)
           
           #Factoren: NA -> Onbekend
           l_ply(names(trainset),function(x) {if(is.factor(trainset[[x]])) trainset[is.na(get(x)),eval(x):="Onbekend" ]})
           l_ply(names(testset),function(x) {if(is.factor(testset[[x]])) testset[is.na(get(x)),eval(x):="Onbekend" ]})
         },
         
         fullset = {
           cat("Kies een volledige dataset... \n")
           filechooser = choose.files(default = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/*.Rda"))
           filename    = file_path_sans_ext(basename(filechooser))
           load(filechooser)
           
           
           set.seed(12345);
           index <- 1:nrow(alldata)
           trainindex <- sample(index, trunc((length(index))*280000)/300000)
           View(trainindex);
           trainset <- alldata[trainindex,]
           testset <- alldata[-trainindex,]
           
           #   load("C:/Data/Asset Health Data/3. Analyse Datasets/5. MVA analyseset/Output/MVA sample MSR full_test_train_set.Rda")
           #   load("C:/Data/Asset Health Data/3. Analyse Datasets/5. MVA analyseset/Output/MVA sample MSR full_alldata.Rda")
         })
  
  # Imputeer
  switch(imputatie,
         Amelia= {
           #we slaan de doelvariabele apart op
           lengtetrain <- nrow(trainset);
           dataset <- rbind(trainset, testset);
           doelvariabele <- dataset[[cfg$Target_Variable]];
           dataset[,eval(cfg$Target_Variable) := NULL];
           dataset <- data.frame(dataset);
           
           for (i in 1:ncol(dataset))
           {
             #karakters imputeren en omzetten naar factoren
             if (is.character(dataset[,i]))
             {
               dataset[!complete.cases(dataset[,i]),i] <- "Onbekend";
               dataset[,i] <- as.factor(dataset[,i]);
             }
             #factoren imputeren (daarvoor moeten ze eerst karakter worden...)
             if (is.factor(dataset[,i]))
             {
               dataset[,i] <- as.character(dataset[,i])
               dataset[!complete.cases(dataset[,i]),i] <- "Onbekend";
               dataset[,i] <- as.factor(dataset[,i]);
             }
           }
           
           #rijtje maken met welke kolommen factoren zijn
           factoren <- c();
           teller <- 1;
           for (i in 1:ncol(dataset))
           {
             if (is.factor(dataset[,i]))
             {
               factoren[teller] <- i;
               teller <- teller + 1;
             }
           }
           
           #grenzen voor imputatie instellen d.m.v. minimum en maximum
           rijenmatrix <- ncol(dataset) - length(factoren);
           grenzen <- matrix(data = NA, nrow = rijenmatrix, ncol = 3);
           grenzen[,1] <- setdiff((1:ncol(dataset)), factoren);
           for (i in 1:nrow(grenzen))
           {
             grenzen[i,2] <- min(dataset[,grenzen[i,1]], na.rm = TRUE);
             grenzen[i,3] <- max(dataset[,grenzen[i,1]], na.rm = TRUE);
           }
           
           #imputatie
           set.seed(863);
           imputatie <- amelia(dataset, m = aantalimp, idvars = factoren, bounds = grenzen);
           
           #Voor elke imputatie slaan we de train- en testset los op
           imptrain <- lapply(imputatie$imputations, function(dataframe){dataframe[c(1:lengtetrain),]});
           imptest <- lapply(imputatie$imputations, function(dataframe){dataframe[c((lengtetrain+1):nrow(dataset)),]});
           
           #En we onthouden de doelvariabele die respectievelijk bij de train- en de testset hoort.
           traindoel <- doelvariabele[c(1:lengtetrain)];
           testdoel <- doelvariabele[c((lengtetrain+1):length(doelvariabele))];
         },
         
         geen = {
           #we slaan de doelvariabele apart op
           lengtetrain <- nrow(trainset);
           dataset <- rbind(trainset, testset);
           doelvariabele <- dataset[[cfg$Target_Variable]];
           dataset[,eval(cfg$Target_Variable) := NULL];
           dataset <- data.frame(dataset);
           
           for (i in 1:ncol(dataset))
           {
             #karakters imputeren en omzetten naar factoren
             if (is.character(dataset[,i]))
             {
               dataset[!complete.cases(dataset[,i]),i] <- "Onbekend";
               dataset[,i] <- as.factor(dataset[,i]);
             }
             #factoren imputeren (daarvoor moeten ze eerst karakter worden...)
             if (is.factor(dataset[,i]))
             {
               dataset[,i] <- as.character(dataset[,i])
               dataset[!complete.cases(dataset[,i]),i] <- "Onbekend";
               dataset[,i] <- as.factor(dataset[,i]);
             }
           }
           
           #aantal omzettingen naar juiste formaat
           aantalimp <- 1;
           imptrain <- vector("list", aantalimp);
           imptest <- vector("list", aantalimp);
           imptrain[[1]] <- dataset[c(1:lengtetrain),];
           imptest[[1]] <- dataset[c((lengtetrain+1):nrow(dataset)),];
           
           #En we onthouden de doelvariabele die respectievelijk bij de train- en de testset hoort.
           traindoel <- doelvariabele[c(1:lengtetrain)];
           testdoel <- doelvariabele[c((lengtetrain+1):length(doelvariabele))];
         },
         
         preprocess = {
           #we slaan de doelvariabele apart op
           lengtetrain <- nrow(trainset);
           dataset <- rbind(trainset, testset);
           doelvariabele <- dataset[[cfg$Target_Variable]];
           dataset[,eval(cfg$Target_Variable) := NULL];
           dataset <- data.frame(dataset);
           
           #aantal omzettingen naar juiste formaat
           aantalimp <- 1;
           imptrain <- vector("list", aantalimp);
           imptest <- vector("list", aantalimp);
           imptrain[[1]] <- dataset[c(1:lengtetrain),];
           imptest[[1]] <- dataset[c((lengtetrain+1):nrow(dataset)),];
           
           #En we onthouden de doelvariabele die respectievelijk bij de train- en de testset hoort.
           traindoel <- doelvariabele[c(1:lengtetrain)];
           testdoel <- doelvariabele[c((lengtetrain+1):length(doelvariabele))];
         },
         
         gemiddelde = {
           #we slaan de doelvariabele apart op
           lengtetrain <- nrow(trainset);
           dataset <- rbind(trainset, testset);
           doelvariabele <- dataset[[cfg$Target_Variable]];
           dataset[,eval(cfg$Target_Variable) := NULL];
           dataset <- data.frame(dataset);
                 
           
           for (i in 1:ncol(dataset))
           {
             #karakters imputeren en omzetten naar factoren
             if (is.character(dataset[,i]))
             {
               dataset[!complete.cases(dataset[,i]),i] <- "Onbekend";
               dataset[,i] <- as.factor(dataset[,i]);
             }
             #factoren imputeren (daarvoor moeten ze eerst karakter worden...)
             if (is.factor(dataset[,i]))
             {
               dataset[,i] <- as.character(dataset[,i])
               dataset[!complete.cases(dataset[,i]),i] <- "Onbekend";
               dataset[,i] <- as.factor(dataset[,i]);
             }
             if (is.numeric(dataset[,i]))
             {
               dataset[!complete.cases(dataset[,i]),i] <- mean(dataset[,i], na.rm=TRUE)
             }
           }
           
     
           
           
           #aantal omzettingen naar juiste formaat
           aantalimp <- 1;
           imptrain <- vector("list", aantalimp);
           imptest <- vector("list", aantalimp);
           imptrain[[1]] <- dataset[c(1:lengtetrain),];
           imptest[[1]] <- dataset[c((lengtetrain+1):nrow(dataset)),];
           
           
           
           
           
           
           #En we onthouden de doelvariabele die respectievelijk bij de train- en de testset hoort.
           traindoel <- doelvariabele[c(1:lengtetrain)];
           testdoel <- doelvariabele[c((lengtetrain+1):length(doelvariabele))];
         },
         
         compleet = {
           #verwijderen incomplete gevallen
           train <- trainset[complete.cases(trainset),];
           test <- testset[complete.cases(testset),];
           
           #we slaan de doelvariabele apart op
           lengtetrain <- nrow(train);
           dataset <- rbind(train, test);
           doelvariabele <- dataset[[cfg$Target_Variable]];
           dataset[,eval(cfg$Target_Variable) := NULL];
           dataset <- data.frame(dataset);
           
           for (i in 1:ncol(dataset))
           {
             #karakters imputeren en omzetten naar factoren
             if (is.character(dataset[,i]))
             {
               dataset[!complete.cases(dataset[,i]),i] <- "Onbekend";
               dataset[,i] <- as.factor(dataset[,i]);
             }
             #factoren imputeren (daarvoor moeten ze eerst karakter worden...)
             if (is.factor(dataset[,i]))
             {
               dataset[,i] <- as.character(dataset[,i])
               dataset[!complete.cases(dataset[,i]),i] <- "Onbekend";
               dataset[,i] <- as.factor(dataset[,i]);
             }
           }
           
           
           
           #aantal omzettingen naar juiste formaat
           aantalimp <- 1;
           imptrain <- vector("list", aantalimp);
           imptest <- vector("list", aantalimp);
           imptrain[[1]] <- train;
           imptest[[1]] <- test;
           
           #En we onthouden de doelvariabele die respectievelijk bij de train- en de testset hoort.
           traindoel <- doelvariabele[c(1:lengtetrain)];
           testdoel <- doelvariabele[c((lengtetrain+1):length(doelvariabele))];
         })
  
  # Draai de methode
  switch(methode,
         RF = 
{ 
  print("Bezig met random forests...");
  #Hierin slaan we het resultaat van de uiteindelijke random forest op.
  boompjes <- vector("list",aantalimp);
  
  #Voor elke imputatie…
  for (i in 1:aantalimp)
  {
    print("imputatie:")
    print(i)
    #Voegen we de doelvariabele aan de trainset toe.
    imptrain[[i]]$doel <- traindoel;
    set.seed(863368);
    #Maken we een random-forestmodel.
    boompjes[[i]] <- foreach(ntree=rep(aantalboom, 1), .combine=combine, .multicombine = TRUE, .packages='randomForest') %do% {randomForest(as.factor(doel) ~., data = imptrain[[i]], ntree=ntree, importance = TRUE, do.trace = TRUE)}
    #boompjes[[i]] <- foreach(ntree=rep(aantalboom, 1), .combine=combine, .multicombine = TRUE, .packages='randomForest') %do% {randomForest(as.factor(doel) ~., data = imptrain[[i]], ntree=ntree, sampsize = c("0" = 37500, "1" = 2500), importance = FALSE, do.trace = TRUE)}
    #En doen we een voorspelling op de testset.
    imptest[[i]]$voorspelling <- predict(boompjes[[i]], imptest[[i]], type="prob")[,2];
    #En nu pas voegen we de doelvariabele aan de testset toe.
    imptest[[i]]$doel <- testdoel;
  }
  
  #We berekenen de gemiddelde voorspelling
  voorspelling.tot <- numeric(length(imptest[[i]]$voorspelling));
  for (i in 1:aantalimp)
  {
    voorspelling.tot <- voorspelling.tot + imptest[[i]]$voorspelling
  }
  voorspelling.tot <- voorspelling.tot / aantalimp;
  
  #Maken van de ROC-curve
  grafiek <- roc(imptest[[1]]$doel ~ voorspelling.tot);
  jpeg("RF_ROC.jpg");
  plot(grafiek);
  dev.off();
  uitkomst <- list(train = imptrain, test = imptest, resultaat = boompjes, plotje = grafiek);
  return(uitkomst);
},

LR = {
  print("Bezig met logistische regressie");
  #Hierin slaan we het resultaat van de uiteindelijke logistische regressie op.
  logreg <- vector("list",aantalimp);
  
  #Voor elke imputatie…
  for (i in 1:aantalimp)
  {
    #Voegen we de doelvariabele aan de trainset toe.
    imptrain[[i]]$doel <- traindoel;
    set.seed(863368);
    #Maken we een random-forestmodel.
    logreg[[i]] <- glm(doel ~ ., data = imptrain[[i]], family = binomial)
    #En doen we een voorspelling op de testset.
    imptest[[i]]$voorspelling <- predict(logreg[[i]], imptest[[i]], type="response");
    #En nu pas voegen we de doelvariabele aan de testset toe.
    imptest[[i]]$doel <- testdoel;
  }
  
  #We berekenen de gemiddelde voorspelling
  voorspelling.tot <- numeric(length(imptest[[i]]$voorspelling));
  for (i in 1:aantalimp)
  {
    voorspelling.tot <- voorspelling.tot + imptest[[i]]$voorspelling
  }
  voorspelling.tot <- voorspelling.tot / aantalimp;
  
  #Maken van de ROC-curve
  grafiek <- roc(imptest[[1]]$doel ~ voorspelling.tot);
  jpeg("LR_ROC.jpg");
  plot(grafiek);
  dev.off();
  
  
},
DT = {
  print("Bezig met beslisbomen...")
  #Hierin slaan we het resultaat van de beslisboom op.
  boom <- vector("list",aantalimp);
  print(aantalimp);
  
  #Voor elke imputatie…
  for (i in 1:aantalimp)
  {
    print(i)
    #Voegen we de doelvariabele aan de trainset toe.
    imptrain[[i]]$doel <- traindoel;
    set.seed(863368);
    #Maken we een random-forestmodel.
    boom[[i]] <- rpart(doel ~ ., data = imptrain[[i]], control=rpart.control(minsplit=minsplit, minbucket=minbucket, cp=cp));
    print("hallo")
    #En doen we een voorspelling op de testset.
    imptest[[i]]$voorspelling <- predict(boom[[i]], new = imptest[[i]]);
    print("hallo2")
    #En nu pas voegen we de doelvariabele aan de testset toe.
    imptest[[i]]$doel <- testdoel;
  }
  
  #We berekenen de gemiddelde voorspelling
  voorspelling.tot <- numeric(length(imptest[[i]]$voorspelling));
  for (i in 1:aantalimp)
  {
    voorspelling.tot <- voorspelling.tot + imptest[[i]]$voorspelling
  }
  voorspelling.tot <- voorspelling.tot / aantalimp;
  
  #Maken van de ROC-curve
  grafiek <- roc(imptest[[1]]$doel ~ voorspelling.tot);
  jpeg(filename = "DT_ROC.jpg");
  plot(grafiek);
  dev.off();
  plot(grafiek);           
})
}



BinGeographic = function(inputset,N=10){
  
  outputset=inputset
  
  for (i in names(inputset)){
    
    set=inputset[,get(i)]
    
    #find value beyond which only 5% of the data exists, this will be the lower limit of the last bin
    f=function(x) (sum(set>x)/length(set)-0.05)
    topBin=round(uniroot(f,c(min(set),max(set)))$root)
    
    
    #cut the other 95% in N equal pieces with N-1 cuts
    binBottoms=c(seq(floor(min(set)),topBin,length.out=N),Inf)
    
    #binBottoms=c(exp(seq(log(min(set)), log(topBin), length.out = N)),Inf)
    
    
    binnedSet=cut(set,binBottoms,include.lowest=T,right=F)
    
    outputset[,eval(i):=binnedSet]
  }
  
  return(outputset)
  
}
