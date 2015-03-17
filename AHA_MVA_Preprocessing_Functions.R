AHA_MVA_Preprocessing = function(){
  mindataset = data.table(fread("E:/1. Alliander/1. Hypercube Storingen/koppel_kabels_min_klic.csv"))
  Simple_Lift(mindataset,"risicogebied_water","asset_in")
  frequencybar(mindataset[,list(AssetID)])
  currentvariable = "asset_in"
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
  
  par(mar = c(4, 4, 4, 0))  # Leave space for z axis
  barplot(rechtergrens2$lift, names.arg=rechtergrens2$rechtergrens, las = 2, col = heat.colors(length(rechtergrens2$rechtergrens)), main=colnames(dataset.ext)
          ,cex.names  =0.8,cex.axis = 0.8) # first plot
  abline(h=1);
  mtext("Lift", side=2, line=3)
  
  par(new=TRUE)
  plot(x = as.factor(rechtergrens3$rechtergrens), y = rechtergrens3$totaalonthouden, pch = 1) #, xaxt="n",yaxt="n"
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
