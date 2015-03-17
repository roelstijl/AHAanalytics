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
l_ply(names(dataset)[laply(dataset,function(x) class(x)[1])=="POSIXct"],function(x) dataset[,eval(x):=as.Date(get(x))])
l_ply(names(dataset)[laply(dataset,function(x) class(x)[1])=="integer"],function(x) dataset[,eval(x):=as.numeric(get(x))])
l_ply(names(dataset)[laply(dataset,function(x) class(x)[1])=="Date"],function(x) dataset[,eval(x):=as.numeric(get(x))])

l_ply(names(dataset)[which(laply(dataset,function(x) length(table(x)))>=30 & laply(dataset,function(x) class(x)[1])!="numeric") ],function(x) dataset[,eval(x):=as.character(get(x))])

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
#  setpbarwrapper(pb,label = paste0(corTypeTable[inum,j,with=F],", i = ",as.character(inum),":", i, " -- j = ", j));

   cat(paste0(corTypeTable[inum,j,with=F],", i = ",as.character(inum),":", i, " -- j = ", j , "\n"))

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