Save_preprocess = function(st,type){
# Written by Roel Stijl (Bearingpoint B.V.) - 13-03-2015  
# Saves output from shiny function
  
  cat("Writing data to file...... ")
load(filechooser)
mindataset = data.table(mindataset)
l_ply(names(mindataset)[laply(mindataset,is.character)],function(x) mindataset[,eval(x):=as.factor(mindataset[,get(x)])])
l_ply(names(mindataset)[laply(mindataset,function(x) class(x)[1])=="POSIXct"],function(x) mindataset[,eval(x):=as.Date(get(x))])
l_ply(names(mindataset)[laply(mindataset,function(x) class(x)[1])=="integer"],function(x) mindataset[,eval(x):=as.numeric(get(x))])
l_ply(names(mindataset)[laply(mindataset,function(x) class(x)[1])=="Date"],function(x) mindataset[,eval(x):=as.numeric(get(x))])

l_ply(names(mindataset), function(x) 
  switch(class(mindataset[[x]]),
         factor ={
           mindataset[is.na(get(x))|tolower(get(x))=="onbekend"|tolower(get(x))=="unknown"|get(x)==""|get(x)=="NA"|get(x)=="",eval(x):=as.factor(NA)]
         }))

switch(type,
      Test_Train = {
        testrows = rep(F,nrow(mindataset))
        testrows[sample(1:nrow(mindataset),st$tst_size)] = T
        trainrow = c(sample(which(mindataset[[st$Target_Variable]]==st$Target_Value & !testrows),st$Tr_size*(st$Tr_tgt)),
                     sample(which(mindataset[[st$Target_Variable]]!=st$Target_Value & !testrows),st$Tr_size*(1-st$Tr_tgt)))
        testrows = which(testrows)
        
        testset  = mindataset[testrows,metadata$names[metadata$selected],with=F]
        trainset = mindataset[trainrow,metadata$names[metadata$selected],with=F]
        
        save(testset,trainset,metadata,cfg,file = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_test_train_set.Rda"),compress=F)
        write.table(testset,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_testset.csv"),sep = ";",na="");
        write.table(trainset,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_trainset.csv"),sep = ";",na="");
        },
      Full       = {
        alldata = mindataset[,metadata$names[metadata$selected],with=F]
        write.table(alldata,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_alldata.csv"),sep = ";",na="");
        save(alldata,metadata,cfg,file = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Output/",filename,"_alldata.Rda"),compress=F)
        
      }
      )
cat("Done\n")
}

AHA_MVA_CorrelationTable = function(dataset,colnumber=1){ 
# Written by Michiel Musterd / Roel Stijl (Bearingpoint B.V.) - 13-03-2015
# This function computes the correlation coefficients between the different columns
# in the input dataset based on pearson, anova_eta and cramers V for continuous-continuous,
# continuous-discrete, discrete-discrete respectively and returns a list with a table with these
# coefficients and a table indicating which type of correlation coefficient it is

# Note that the absolute value of the pearson coefficient is returned because we are
# only interested in effect size, not in effect direction

# Perform some corrections
l_ply(names(dataset)[laply(dataset,is.character)],function(x) dataset[,eval(x):=as.factor(dataset[,get(x)])])
l_ply(names(dataset)[which(laply(dataset,function(x) length(table(x)))==1)],function(x) dataset[,eval(x):=as.character(get(x))])
l_ply(names(dataset)[laply(dataset,function(x) class(x)[1])=="POSIXct"],function(x) dataset[,eval(x):=as.Date(get(x))])
l_ply(names(dataset)[laply(dataset,function(x) class(x)[1])=="integer"],function(x) dataset[,eval(x):=as.numeric(get(x))])
l_ply(names(dataset)[laply(dataset,function(x) class(x)[1])=="Date"],function(x) dataset[,eval(x):=as.numeric(get(x))])

l_ply(names(dataset)[which(laply(dataset,function(x) length(table(x)))>=cfg$max_categories 
                           & laply(dataset,function(x) class(x)[1])!="numeric") ],function(x) dataset[,eval(x):=as.character(get(x))])

l_ply(names(dataset)[laply(dataset,function(x) class(x)[1])=="numeric"],function(i)
  dataset[is.na(get(i)),eval(i):=median(dataset[,get(i)],na.rm=T)])
l_ply(names(dataset)[laply(dataset,function(x) class(x)[1])=="integer"],function(i)
  dataset[is.na(get(i)),eval(i):=median(dataset[,get(i)],na.rm=T)])

corTable=data.table(rowname=colnames(dataset))
corTypeTable=data.table(rowname=colnames(dataset))

pb = pbarwrapper(title= "Calculating correlation coefficients: ",label="temp",max=ncol(dataset)^2+1)

l_ply(names(dataset)[laply(dataset, function(x) length(unique(x)))<=1], function(x) dataset[,eval(x):=as.character(get(x))])

corTypeTable = 
  data.table(
    ldply(names(dataset), 
          function(i){
            laply(names(dataset),
                  function(j){
                    
                    switch(ifelse(class(dataset[[j]])=="factor","factor",ifelse(class(dataset[[j]])=="character","character","numeric")),
                           numeric = switch(class(dataset[[i]]),
                                            numeric = "C_to_C_Pearson",
                                            factor  = "C_to_D_Anova_eta",
                                            "Error"),
                           factor  = switch(class(dataset[[i]]),
                                            numeric = "D_to_C_Anova_eta",         
                                            factor  = "D_to_D_Cramers_V",
                                            "Error"),
                           "Error"
                    )})}))

setnames(corTypeTable,names(dataset))

corTable = data.table(ldply(1:length(names(dataset)), 
                            function(inum){
                              laply(names(dataset),
                                    function(j) calccor(j,inum,dataset,corTypeTable,pb)    
                              )}))
setpbarwrapper(pb,label="Done")

setnames(corTable,names(dataset))
corTypeTable[, row.names:= names(dataset)]
corTable[, row.names:= names(dataset)]

return(list(types=corTypeTable,correlations=corTable))
}

calccor = function(j,inum,dataset,corTypeTable,pb){
i =  names(dataset)[inum]
setpbarwrapper(pb,label = paste0(corTypeTable[inum,j,with=F],", i = ",as.character(inum),":", i, " -- j = ", j));

# cat(paste0(corTypeTable[inum,j,with=F],", i = ",as.character(inum),":", i, " -- j = ", j , "\n"))

out = switch(corTypeTable[inum,get(j)],
             C_to_C_Pearson = {#impute the data with median for missing values         
               abs(cor(dataset[[j]],dataset[[i]]))},
             
             C_to_D_Anova_eta = {
               sqrt(etasq(aovwrapper(dataset,i,j))$"Partial eta^2"[1])
             },
             
             D_to_C_Anova_eta = {
               sqrt(etasq(aovwrapper(dataset,j,i))$"Partial eta^2"[1])
             },
             
             D_to_D_Cramers_V =
               summary(assocstats(table(dataset[[j]],dataset[[i]])))$object$cramer,
             
             Error            = 1,
             
             testz(corTypeTable,i,inum,j))

return(out)
}

aovwrapper = function(dataset,i,j){
a = aov(dataset[[j]]~dataset[[i]])
a$residuals[a$residuals<0.1] = a$residuals[a$residuals<0.1] +0.0001
return(a)
}

testz=function(corTypeTable,i,inum,j){
a=1
}

Simple_Lift = function(dataset,targetvariable,targetvalue,currentvariable){
# Written by Roel Stijl (Bearingpoint), Frank Rijnders (Alliander), 03-2015
# Creates a simple lift graph

# dataset.local = dataset
# 
# if(class(dataset.local[[currentvariable]])=="Date")    dataset.local[[currentvariable]] = as.numeric(dataset.local[[currentvariable]])
# if(class(dataset.local[[currentvariable]])=="integer") dataset.local[[currentvariable]] = as.numeric(dataset.local[[currentvariable]])
# 
# if(class(dataset.local[[targetvariable]])=="Date")    dataset.local[[targetvariable]] = as.numeric(dataset.local[[targetvariable]])
# if(class(dataset.local[[targetvariable]])=="integer") dataset.local[[targetvariable]] = as.numeric(dataset.local[[targetvariable]])

tabeltotaal <- table(dataset[[currentvariable]]);
tabelgestoord <- table(dataset[dataset[[targetvariable]] == targetvalue,currentvariable,with=F]);

if(nrow(tabelgestoord)==0 | nrow (tabeltotaal)==0 | nrow(tabeltotaal)<=100) {plot(1:2); cat("Error, too few to too many (>100) variables\n"); return()}

tabelmerge = switch (class(dataset[[currentvariable]]),
                     numeric  =  merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = TRUE),
                     factor   =  merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = TRUE),
                     character=  merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = TRUE),
                     Date     =  merge(tabeltotaal, tabelgestoord, by = 1, all = TRUE, sort = TRUE))

tabelmerge[is.na(tabelmerge)] <- 0;

rijenperkeer = switch (class(dataset[[currentvariable]]),
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
rechtergrens = switch (class(dataset[[currentvariable]]),
                       numeric =   as.numeric(rechtergrens[2:nrow(rechtergrens),1]),
                       factor  =   rechtergrens[2:nrow(rechtergrens),1],
                       character = rechtergrens[2:nrow(rechtergrens),1])

lift <- (gestoordonthouden / sum(gestoordonthouden)) / ( totaalonthouden / sum(totaalonthouden));
rechtergrens2 <- data.table(rechtergrens, lift);
rechtergrens3 <- data.table(rechtergrens, totaalonthouden);

par(mar = c(5, 4, 0, 4) + 0.3) # Leave space for z axis
barplot(rechtergrens2$lift, names.arg=rechtergrens2$rechtergrens, las = 2, col = heat.colors(length(rechtergrens2$rechtergrens)), main=colnames(dataset[[currentvariable]])
        ,cex.names  =0.8,cex.axis = 0.8) # first plot
abline(h=1);
mtext("Lift", side=2, line=3)

par(new=TRUE)
plot(x = as.factor(rechtergrens3$rechtergrens), y = rechtergrens3$totaalonthouden, pch = 1, xaxt="n",yaxt="n",xlab="",ylab="")

axis(side=4)
mtext("Frequency", side=4, line=3)
}

frequencybar = function(datavector){
# Written by Roel Stijl (Bearingpoint) 2015
# Creates a plot with frequencies of e.g. empty values
# Datavector has to be a 1xM datatable
  
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
  ggtitle(paste0("NAs and empty values: ", varname)) +
  coord_flip() + 
  guides(fill=guide_legend(title=NULL)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),panel.border = element_blank())
}
