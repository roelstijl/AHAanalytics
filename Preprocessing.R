preprocessing = function(){
  storingenset <- read.csv("~/Multivariate analyse/storingenset.csv", sep=";")
variabelen <- c(2, 7, 8, 14, 15, 18, 21, 23, 28, 29, 30, 33, 37, 38, 40, 41, 47, 53, 55, 62, 64, 72, 80, 81, 83, 85, 86);
data <- storingenset[,variabelen];
data$failing_asset <- substr(data$failing_asset, 4, 4);
data$failing_asset <- as.numeric(data$failing_asset);
data <- data[complete.cases(data),];
#factoren <- c(2,3,4,6,8,9,10,11,12,13,14,15,16,17,18,19,24,25,26,27);
factoren <- which(sapply(data,class) == "character") #kolommen die als karakters geïnterpreteerd moeten worden
#eventueel kun je dit ook handmatig invoeren (bijv. als je een kolom hebt met alleen numerieke waardes die je toch als factor wilt zien)
#doe dit dan als 3 regels hierboven
listtake = logical()}

#functie die van aantal kolommen factoren maakt
naarfactor <- function(dataset, factoren){
  i <- 1;
  for (i in 1:ncol(dataset))
  {
    dataset[,factoren[i]] <- as.factor(dataset[,factoren[i]]);
  }
  dataset;
}  

#functie die vraagt of we bepaalde variabelen willen behouden
readkey <- function(){
#   cat ("Press [enter] to continue");

   n <- readline("Keep variable? T/F:\n")
  
#   line <- readline();
  return(n)
}

#functie die de lift van parameters berekent
preprocessing <- function(dataset){
  for (teller in 2:ncol(dataset))
  {
    tabeltotaal <- as.data.frame(table(data[,teller]));
    tabelgestoord <- as.data.frame(table(data[which(data[,1] == 1),teller]));
    if (is.numeric(data[,teller]))
    {
       tabelmerge <- merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = TRUE);
    }
    if (is.factor(data[,teller]))
    {
       tabelmerge <- merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = FALSE);
    }
    if (is.character(data[,teller]))
    {
      tabelmerge <- merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = FALSE);
    }
    tabelmerge[is.na(tabelmerge)] <- 0;
    print(tabelmerge);
    lengtetabel <- nrow(tabelmerge);
    rijenperkeer <- 0;
    if (is.numeric(data[,teller]))
    {
      rijenperkeer <- ceiling(lengtetabel/10); #bepaalt aantal klasses waar je set in opdeelt
    }
    if (is.factor(data[,teller]))
    {
      rijenperkeer <- 1;
    }
    if (is.character(data[,teller]))
    {
      rijenperkeer <- 1;
    }
    i <- 1;
    totaalonthouden <- c();
    gestoordonthouden <- c();
    
    #Stukje onzin maar anders krijg ik het niet werkend
    m <- "a"
    rechtergrens <- data.frame(m);
    rechtergrens <- as.character(m);
    
    for (i in 1:ceiling(lengtetabel/rijenperkeer))
    {
       a <- ((i-1)*rijenperkeer)+1;
       b <- min(i*rijenperkeer,lengtetabel);
       f <- as.character(tabelmerge[b,1]);
       rechtergrens <- rbind(rechtergrens, f)
       tabelletjetotaal <- tabelmerge$Freq.x[a:b];
       tabelletjegestoord <- tabelmerge$Freq.y[a:b];
       totaalonthouden[i] <- sum(tabelletjetotaal);
       gestoordonthouden[i] <- sum(tabelletjegestoord);
    }
    if (is.numeric(data[,teller]))
    {
       rechtergrens <- as.numeric(rechtergrens[2:nrow(rechtergrens),1]);
    }
    if (is.factor(data[,teller]))
    {
       rechtergrens <- rechtergrens[2:nrow(rechtergrens),1];
    }
    if (is.character(data[,teller]))
    {
      rechtergrens <- rechtergrens[2:nrow(rechtergrens),1];
    }
    totaalonthouden2 <- totaalonthouden / sum(totaalonthouden);
    gestoordonthouden <- gestoordonthouden / sum(gestoordonthouden);
    lift <- gestoordonthouden / totaalonthouden2;
    rechtergrens2 <- data.frame(rechtergrens, lift);
    rechtergrens3 <- data.frame(rechtergrens, totaalonthouden);
    par(mar = c(7, 4, 4, 4) + 0.3)  # Leave space for z axis
    barplot(rechtergrens2[,2], names.arg=rechtergrens2[,1], las = 2, col = "red", main=colnames(data[teller])) # first plot
    abline(h=1);
    par(new=TRUE)
    plot(x = factor(rechtergrens3[,1]), y = rechtergrens3[,2], pch = 1, xaxt="n",yaxt="n",xlab="",ylab="")
    axis(side=4)
    mtext("frequentie", side=4, line=3)
    listtake[teller] = readkey();
    teller <- teller + 1;
  }
  listtake[1] <- TRUE;
  return(as.logical(listtake))
}

#functie die de correlatie tussen numerieke parameters berekent
correlaties <- function(data){
   listtake = preprocessing(data);
   print(listtake)
   data <- data[,listtake];
   factoren <- which(sapply(data,class) == "character")
   data <- data[,-factoren];
   correlatie <- cor(data, use = "all.obs", method = "pearson");
   correlatie[lower.tri(correlatie, diag = TRUE)] <- 0;
   #correlatie[abs(correlatie) < 0.5] <- 0;
   #correlatie <- correlatie[rowSums(correlatie^2) > 0, colSums(correlatie^2) > 0];
   fix(correlatie);
}
