AHA_MVA_Analyse = function(methode = "RF_Amelia", analyse = "testtrain", aantalimp = 5){
# Deze code voert de MVA analyse uit, volgt op de preprocessing
  switch(analyse,
         testtrain= {  
           cat("Kies een test / train set... \n")
           filechooser = choose.files(default = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/*.Rda"))
           filename    = file_path_sans_ext(basename(filechooser))
           load(filechooser)
           trainset=data.table(trainset)
           testset=data.table(testset)
           },
         
         fullset = {
           cat("Kies een volledige dataset... \n")
           filechooser = choose.files(default = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/*.Rda"))
           filename    = file_path_sans_ext(basename(filechooser))
           load(filechooser)
         })
  
  # Draai de methode
  switch(methode,
         RF_Amelia = 
         {
           #we slaan de doelvariabele apart op
           lengtetrain <- nrow(trainset);
           dataset <- rbind(trainset, testset);
           doelvariabele <- dataset[[cfg$Target_Variable]];
           dataset[,eval(cfg$Target_Variable) := NULL];
           dataset <- data.frame(dataset);
           
           #karakters omzetten naar factoren
           for (i in 1:ncol(dataset))
           {
             if (is.character(dataset[,i]))
             {
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
           View(imputatie$imputations[[1]]);
           
           #Voor elke imputatie slaan we de train- en testset los op
           imptrain <- lapply(imputatie$imputations, function(dataframe){dataframe[c(1:lengtetrain),]});
           imptest <- lapply(imputatie$imputations, function(dataframe){dataframe[c((lengtetrain+1):nrow(dataset)),]});
                      
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