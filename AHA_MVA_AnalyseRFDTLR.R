<<<<<<< HEAD
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

AHA_MVA_Analyse = function(methode = "RF", analyse = "testtrain", imputatie = "Amelia", aantalimp = 5, aantalboom = 500, minsplit = 60, minbucket = 20, cp = 0.001){
  
  # laden relevante packages
  require(randomForest);
  require(pROC);
  require(Amelia);
  require(rpart);
  
  # Deze code voert de MVA analyse uit, volgt op de preprocessing
=======
AHA_MVA_Analyse = function(methode = "RF_Amelia", analyse = "testtrain", aantalimp = 5){
# Deze code voert de MVA analyse uit, volgt op de preprocessing
>>>>>>> 46774f35e420a33801ff684aafce5547240c3a61
  switch(analyse,
         testtrain= {  
           cat("Kies een test / train set... \n")
           filechooser = choose.files(default = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/*.Rda"))
           filename    = file_path_sans_ext(basename(filechooser))
           load(filechooser)
           trainset=data.table(trainset)
           testset=data.table(testset)
<<<<<<< HEAD
           
           #Factoren: NA -> Onbekend
           l_ply(names(trainset),function(x) {if(is.factor(trainset[[x]])) trainset[is.na(get(x)),eval(x):="onbekend" ]})
           l_ply(names(testset),function(x) {if(is.factor(testset[[x]])) testset[is.na(get(x)),eval(x):="onbekend" ]})
         },
=======
           },
>>>>>>> 46774f35e420a33801ff684aafce5547240c3a61
         
         fullset = {
           cat("Kies een volledige dataset... \n")
           filechooser = choose.files(default = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/*.Rda"))
           filename    = file_path_sans_ext(basename(filechooser))
           load(filechooser)
<<<<<<< HEAD
           
           
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
=======
         })
  
  # Draai de methode
  switch(methode,
         RF_Amelia = 
         {
>>>>>>> 46774f35e420a33801ff684aafce5547240c3a61
           #we slaan de doelvariabele apart op
           lengtetrain <- nrow(trainset);
           dataset <- rbind(trainset, testset);
           doelvariabele <- dataset[[cfg$Target_Variable]];
           dataset[,eval(cfg$Target_Variable) := NULL];
           dataset <- data.frame(dataset);
           
<<<<<<< HEAD
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
=======
           #karakters omzetten naar factoren
           for (i in 1:ncol(dataset))
           {
             if (is.character(dataset[,i]))
             {
>>>>>>> 46774f35e420a33801ff684aafce5547240c3a61
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
<<<<<<< HEAD
=======
           View(imputatie$imputations[[1]]);
>>>>>>> 46774f35e420a33801ff684aafce5547240c3a61
           
           #Voor elke imputatie slaan we de train- en testset los op
           imptrain <- lapply(imputatie$imputations, function(dataframe){dataframe[c(1:lengtetrain),]});
           imptest <- lapply(imputatie$imputations, function(dataframe){dataframe[c((lengtetrain+1):nrow(dataset)),]});
<<<<<<< HEAD
           
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
=======
                      
           #En we onthouden de doelvariabele die respectievelijk bij de train- en de testset hoort.
           traindoel <- doelvariabele[c(1:lengtetrain)];
           testdoel <- doelvariabele[c((lengtetrain+1):length(doelvariabele))];
           
           #Hierin slaan we het resultaat van de uiteindelijke random forest op.
           boompjes <- vector("list",imputatie$m);
           
           #Voor elke imputatie…
           for (i in 1:imputatie$m)
           {
             #Voegen we de doelvariabele aan de trainset toe.
             imptrain[[i]]$doel <- traindoel;
             set.seed(863368);
             #Maken we een random-forestmodel.
             boompjes[[i]] <- foreach(ntree=rep(500, 1), .combine=combine, .multicombine = TRUE, .packages='randomForest') %do% {randomForest(as.factor(doel) ~., data = imptrain[[i]], ntree=ntree, importance = TRUE, do.trace = TRUE)}
             #En doen we een voorspelling op de testset.
             imptest[[i]]$voorspelling <- predict(boompjes[[i]], imptest[[i]], type="prob")[,2];
             #En nu pas voegen we de doelvariabele aan de testset toe.
             imptest[[i]]$doel <- testdoel;
           }
           
           #We berekenen de gemiddelde voorspelling
           voorspelling.tot <- c();
           for (i in 1:imputatie$m)
           {
             voorspelling.tot <- voorspelling.tot + imptest[[i]]$voorspelling
           }
           
           #Maken van de ROC-curve
           grafiek <- roc(imptest[[1]]$doel ~ voorspelling.tot);
           plot(grafiek);           
         },
         
         LR = {
           
           
         },
         DT = {
           
           
         })
  
#   load("C:/Data/Asset Health Data/3. Analyse Datasets/5. MVA analyseset/Output/MVA sample MSR full_test_train_set.Rda")
#   load("C:/Data/Asset Health Data/3. Analyse Datasets/5. MVA analyseset/Output/MVA sample MSR full_alldata.Rda")
}
>>>>>>> 46774f35e420a33801ff684aafce5547240c3a61
