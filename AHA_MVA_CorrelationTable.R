

AHA_MVA_CorrelationTable = function(dataset){ 
#Written by Michiel Musterd - 13-03-2015
#This function computes the correlation coefficients between the different columns
#in the input dataset based on pearson, anova_eta and cramers V for continuous-continuous,
#continuous-discrete, discrete-discrete respectively and returns a list with a table with these
#coefficients and a table indicating which type of correlation coefficient it is
  
#Note that the absolute value of the pearson coefficient is returned because we are
#only interested in effect size, not in effect direction

  
library("vcd") #for the cramers V function
library("heplots") #for the etasq function
#library("mice") #for the imputation by linear regression with bootstrapped error correction

corTable=data.table(rowname=colnames(dataset))
corTypeTable=data.table(rowname=colnames(dataset))


for (currentVariable in names(dataset)){
  for (i in names(dataset)){
    cat(currentVariable, " -- ",i,"\n")
    
    if (is.numeric(dataset[,get(currentVariable)])==T){
      #impute the data with median for missing values
      dataset[is.na(eval(currentVariable)),get(currentVariable):=median(dataset$get(currentVariable))]
      

      if (is.numeric(dataset[,get(i)])==T){
        #impute the data with median for missing values
        dataset[is.na(eval(i)),get(i):=median(dataset$get(i))]
        
        
        corTable[rowname==eval(currentVariable),eval(i):=abs(cor(dataset[,get(currentVariable)],dataset[,get(i)]))]
        corTypeTable[rowname==eval(currentVariable),eval(i):="Pearson"]
      }else if (is.factor(dataset[,get(i)])==T){
        output=aov(dataset[,get(currentVariable)]~dataset[,get(i)])  
        corTable[rowname==eval(currentVariable),eval(i):=sqrt(etasq(output)$"Partial eta^2"[1])]
        corTypeTable[rowname==eval(currentVariable),eval(i):="Anova_eta"]
      } else{
        corTable[rowname==eval(currentVariable),eval(i):=NA]
        corTypeTable[rowname==eval(currentVariable),eval(i):=NA]
      } 
      
    }
    
    if (is.factor(dataset[,get(currentVariable)])==T){  
      if (is.numeric(dataset[,get(i)])==T){
        #impute the data with median for missing values
        dataset[is.na(eval(i)),get(i):=median(dataset$get(i))]
        
        output=aov(dataset[,get(i)]~dataset[,get(currentVariable)])  
        corTable[rowname==eval(currentVariable),eval(i):=sqrt(etasq(output)$"Partial eta^2"[1])]
        corTypeTable[rowname==eval(currentVariable),eval(i):="Anova_eta"]
      }else if (is.factor(dataset[,get(i)])==T){
        output=summary(assocstats(table(dataset[,get(currentVariable)],dataset[,get(i)])))        
        corTable[rowname==eval(currentVariable),eval(i):=output$object$cramer]
        corTypeTable[rowname==eval(currentVariable),eval(i):="Cramers V"]
      } else{
        corTable[rowname==eval(currentVariable),eval(i):=NA]
        corTypeTable[rowname==eval(currentVariable),eval(i):=NA]
      } 
    }
    
  }
  
}

return(list(corTable,corTypeTable))

}



