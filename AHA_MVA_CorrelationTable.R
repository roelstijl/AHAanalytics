AHA_MVA_CorrelationTable = function(dataset,colnumber=1){ 
#Written by Michiel Musterd - 13-03-2015
#This function computes the correlation coefficients between the different columns
#in the input dataset based on pearson, anova_eta and cramers V for continuous-continuous,
#continuous-discrete, discrete-discrete respectively and returns a list with a table with these
#coefficients and a table indicating which type of correlation coefficient it is

#Note that the absolute value of the pearson coefficient is returned because we are
#only interested in effect size, not in effect direction

# Perform some corrections
l_ply(names(dataset)[laply(dataset,is.character)],function(x) dataset[,eval(x):=as.factor(dataset[,get(x)])])
l_ply(names(dataset)[which(laply(dataset,function(x) length(table(x)))==1)],function(x) dataset[,eval(x):=as.character(get(x))])

corTable=data.table(rowname=colnames(dataset))
corTypeTable=data.table(rowname=colnames(dataset))

pb = pbarwrapper(title= "Calculating correlation coefficients: ",label="temp",max=ncol(dataset)^2+1)

for (currentVariable in names(dataset)){
for (i in names(dataset)){
  setpbarwrapper(pb,label = paste0(currentVariable, " -- ", i))

  corTypeTable = 
    switch(class(dataset[[currentVariable]]),
         numeric = switch(class(dataset[[i]]),
            numeric = "Pearson",
            factor = "Anova_eta",
            NA),
          factor  = switch(class(dataset[[i]]),
            numeric = "Anova_eta",         
            factor  = "Cramers V",
            NA))
  
  switch(class(dataset[[currentVariable]]),
  numeric = { #impute the data with median for missing values
    dataset[is.na(get(currentVariable)),eval(currentVariable):=median(dataset[,get(currentVariable)],na.rm=T)]
    
    switch(class(dataset[[i]]),
    numeric = {#impute the data with median for missing values
              dataset[is.na(get(i)),eval(i):=median(dataset[,get(i)],na.rm=T)]
              corTable[rowname==eval(currentVariable),eval(i):=abs(cor(dataset[,get(currentVariable)],dataset[,get(i)]))]
              corTypeTable[rowname==eval(currentVariable),eval(i):="Pearson"]},
    factor = {
              output=aov(dataset[,get(currentVariable)]~dataset[,get(i)])  
              corTable[rowname==eval(currentVariable),eval(i):=sqrt(etasq(output)$"Partial eta^2"[1])]
              corTypeTable[rowname==eval(currentVariable),eval(i):="Anova_eta"]},
    
    { # Other
              corTable[rowname==eval(currentVariable),eval(i):=NA]
              corTypeTable[rowname==eval(currentVariable),eval(i):=NA]}
    )
    
  },
  
  factor = {  
    switch(class(dataset[[i]]),
    numeric ={ #impute the data with median for missing values
              dataset[is.na(get(i)),eval(i):=median(dataset[,get(i)],na.rm=T)]
              output=aov(dataset[,get(i)]~dataset[,get(currentVariable)])  
              corTable[rowname==eval(currentVariable),eval(i):=sqrt(etasq(output)$"Partial eta^2"[1])]
              corTypeTable[rowname==eval(currentVariable),eval(i):="Anova_eta"] },
      
    factor = {
              output=summary(assocstats(table(dataset[,get(currentVariable)],dataset[,get(i)])))        
              corTable[rowname==eval(currentVariable),eval(i):=output$object$cramer]
              corTypeTable[rowname==eval(currentVariable),eval(i):="Cramers V"]}, 
    
    { # Other
              corTable[rowname==eval(currentVariable),eval(i):=NA]
              corTypeTable[rowname==eval(currentVariable),eval(i):=NA]
    })
  })
  
}

}
setpbarwrapper(pb,label="Done")
return(data.table(Correlation=corTable,CorrelationType=corTypeTable))

}