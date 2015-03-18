AHA_MVA_Preprocessing = function(){
  mindataset = data.table(fread("E:/1. Alliander/1. Hypercube Storingen/koppel_kabels_min_klic.csv"))
  Simple_Lift(mindataset,"risicogebied_water","asset_in")
  frequencybar(mindataset[,list(AssetID)])
  currentvariable = "asset_in"
}

Save_preprocess = function(st,type){
  cat("Writing data to file...... ")
  load(filechooser)
  mindataset = data.table(mindataset)
  switch(type,
        Test_Train = {
          testrows = rep(F,nrow(mindataset))
          testrows[sample(1:nrow(mindataset),st$tst_size)] = T
          trainrow = c(sample(which(mindataset[[st$Target_Variable]]==st$Target_Value & !testrows),st$Tr_size*(st$Tr_tgt)),
                       sample(which(mindataset[[st$Target_Variable]]!=st$Target_Value & !testrows),st$Tr_size*(1-st$Tr_tgt)))
          testrows = which(testrows)
          
          testset  = mindataset[testrows,metadata$names[metadata$selected],with=F]
          trainset = mindataset[trainrow,metadata$names[metadata$selected],with=F]
          
          save(testset,trainset,file = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_test_train_set.Rda"),compress=F)
          write.csv(testset,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_testset.csv"));
          write.csv(trainset,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_trainset.csv"));
          },
        Full       = {
          alldata = mindataset[,metadata$names[metadata$selected],with=F]
          write.csv(alldata,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_alldata.csv"));
          save(alldata,file = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_alldata.Rda"),compress=F)
          
        }
        )
  cat("Done\n")
}

Simple_Lift = function(dataset,targetvariable,targetvalue,currentvariable)
{
  dataset.ext = na.omit(dataset[[currentvariable]])
  
  tabeltotaal <- table(dataset.ext);
  tabelgestoord <- table(dataset[dataset[[targetvariable]] == targetvalue,currentvariable,with=F]);
  
  if(nrow(tabelgestoord)==0 | nrow (tabeltotaal)==0) {plot(1:2); return()}
  
  if(class(dataset.ext)=="Date") dataset.ext = as.numeric(dataset.ext)
  if(class(dataset.ext)=="integer") dataset.ext = as.numeric(dataset.ext)
    
  tabelmerge = switch (class(dataset.ext),
                       numeric  =  merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = TRUE),
                       factor   =  merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = TRUE),
                       character=  merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = TRUE),
                       Date     =  merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = TRUE))
  
  tabelmerge[is.na(tabelmerge)] <- 0;
  
  rijenperkeer = switch (class(dataset.ext),
                         numeric =   ceiling(nrow(tabelmerge)/10),
                         factor  =   1,
                         character = 1)

  totaalonthouden <- c();
  gestoordonthouden <- c();
  
  m <- "a"
  rechtergrens <- data.frame(m);
  
  for (i in 1:ceiling(nrow(tabelmerge)/rijenperkeer))
  {
    a <- ((i-1)*rijenperkeer)+1
    b <- min(i*rijenperkeer,nrow(tabelmerge))
    rechtergrens <- rbind(rechtergrens, as.character(tabelmerge[b,1]))
    totaalonthouden[i] <- sum(tabelmerge$Freq.x[a:b]);
    gestoordonthouden[i] <- sum(tabelmerge$Freq.y[a:b]);
  }
  
  rechtergrens = switch (class(dataset.ext),
                         numeric =   as.numeric(rechtergrens[2:nrow(rechtergrens),1]),
                         factor  =   rechtergrens[2:nrow(rechtergrens),1],
                         character = rechtergrens[2:nrow(rechtergrens),1])
  
  lift <- (gestoordonthouden / sum(gestoordonthouden)) / ( totaalonthouden / sum(totaalonthouden));
  rechtergrens2 <- data.table(rechtergrens, lift);
  rechtergrens3 <- data.table(rechtergrens, totaalonthouden);
  
  par(mar = c(5, 4, 0, 4) + 0.3) # Leave space for z axis
  barplot(rechtergrens2$lift, names.arg=rechtergrens2$rechtergrens, las = 2, col = heat.colors(length(rechtergrens2$rechtergrens)), main=colnames(dataset.ext)
          ,cex.names  =0.8,cex.axis = 0.8) # first plot
  abline(h=1);
  mtext("Lift", side=2, line=3)
  
  par(new=TRUE)
  plot(x = as.factor(rechtergrens3$rechtergrens), y = rechtergrens3$totaalonthouden, pch = 1, xaxt="n",yaxt="n",xlab="",ylab="")

  axis(side=4)
  mtext("Frequency", side=4, line=3)
  
#   key(rechtergrens2,rechtergrens)
#   key(rechtergrens3,rechtergrens)
#   datasets = 
#   
#   ggplot(rechtergrens2) + 
#     geom_bar(aes(x = lift, y = rechtergrens ),colour = "black") + 
#     geom_lines()
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

frequencybar = function(datavector)
{
  varname = names(datavector)[1]
  
  datavector[[varname]] = ifelse(is.na(datavector[[varname]]),"NA",datavector[[varname]])
  
  datavector$NA_or_empty = 
    ifelse(is.na(datavector[[varname]])|datavector[[varname]]=="NA","NA",
           ifelse(datavector[[varname]] == "","Empty",
                  ifelse(tolower(datavector[[varname]]) == "unknown","Unknown",
                        ifelse(tolower(datavector[[varname]]) == "onbekend","Onbekend","Has Value"))
           ))
  
  ggplot(datavector, aes(x =factor(1),fill = NA_or_empty)) + 
    geom_bar(colour = "black") +
    coord_flip() + 
    #     coord_polar(theta="y") +
    guides(fill=guide_legend(title=NULL)) +
    theme_bw() +
    theme(axis.title.x = element_blank(),axis.title.y = element_blank(),panel.border = element_blank())
}
