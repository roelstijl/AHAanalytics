AHA_MVA_Preprocessing = function(){
  mindataset = dataset.table(fread("E:/1. Alliander/1. Hypercube Storingen/koppel_kabels_min_klic.csv"))
  Simple_Lift(mindataset,"risicogebied_water")
  currentvariable = "asset_in"
}



Simple_Lift = function(dataset,targetvariable,currentvariable)
{
  tabeltotaal <- table(dataset[[currentvariable]]);
  tabelgestoord <- table(dataset[dataset[[targetvariable]] == 1,currentvariable,with=F]);
  
  tabelmerge = switch (class(dataset[[currentvariable]]),
          numeric =   merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = TRUE),
          factor  =   merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = FALSE),
          character = merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = FALSE))

  tabelmerge[is.na(tabelmerge)] <- 0;
  print(tabelmerge);
  lengtetabel <- nrow(tabelmerge);
  rijenperkeer <- 0;
  
  rijenperkeer = switch (class(dataset[[currentvariable]]),
                       numeric =   ceiling(lengtetabel/10);,
                       factor  =   rijenperkeer <- 1,
                       character = rijenperkeer <- 1)
  i <- 1;
  totaalonthouden <- c();
  gestoordonthouden <- c();
  
  #Stukje onzin maar anders krijg ik het niet werkend
  m <- "a"
  rechtergrens <- dataset.frame(m);
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
  
  rechtergrens = switch (class(dataset[[currentvariable]]),
                         numeric =   as.numeric(rechtergrens[2:nrow(rechtergrens),1]),
                         factor  =   rijenperkeer <- 1,
                         character = rijenperkeer <- 1)
  if (is.numeric(dataset[,currentvariable]))
  {
    rechtergrens <- as.numeric(rechtergrens[2:nrow(rechtergrens),1]);
  }
  if (is.factor(dataset[,currentvariable]))
  {
    rechtergrens <- rechtergrens[2:nrow(rechtergrens),1];
  }
  if (is.character(dataset[,currentvariable]))
  {
    rechtergrens <- rechtergrens[2:nrow(rechtergrens),1];
  }
  totaalonthouden2 <- totaalonthouden / sum(totaalonthouden);
  gestoordonthouden <- gestoordonthouden / sum(gestoordonthouden);
  lift <- gestoordonthouden / totaalonthouden2;
  rechtergrens2 <- dataset.frame(rechtergrens, lift);
  rechtergrens3 <- dataset.frame(rechtergrens, totaalonthouden);
  par(mar = c(7, 4, 4, 4) + 0.3)  # Leave space for z axis
  barplot(rechtergrens2[,2], names.arg=rechtergrens2[,targetvariable,with=F], las = 2, col = "red", main=colnames(dataset[currentvariable])) # first plot
  abline(h=1);
  par(new=TRUE)
  plot(x = factor(rechtergrens3[,targetvariable,with=F]), y = rechtergrens3[,2], pch = 1, xaxt="n",yaxt="n",xlab="",ylab="")
  axis(side=4)
  mtext("frequentie", side=4, line=3)
  listtake[currentvariable] = readkey();
  currentvariable <- currentvariable + 1;
}
listtake[1] <- TRUE;
return(as.logical(listtake))