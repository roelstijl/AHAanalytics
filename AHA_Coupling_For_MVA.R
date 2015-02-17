#######################################################################
# Written by Michiel Musterd - 09-02-2015 (last update:13-02-2015)
# -------------
# This code is meant to couple two sets based on their common keys. 
# Possible keys:
# - x,y (nearest neighbour coupling)
# - PC4
# - PC6
# - Essentially any other single key
# When no_of_keys is 2: it is assumed that the coupling is on x,y data
#                       and a nearest neighbour merge is used. 
# When no_of_keys is 1: no assumptions on the type of key are made, merge is on identical matches
#
# The base set is Set1, in which duplicates are allowed (e.g. cables in the same location)
# whereas the dependent set is Set2, in which duplicates are not allowed 
# (and in fact filtered out to catch errors)

# Functionality to be added: 
# ----
# Add coupling on more than 2 keys
# Add coupling on geoquery (shape files: SP package)

# Non-implemented nice to haves:
# -----------------
# - Coupling between PC4 and PC6 by truncation of the PC6 set
# - 
# - ...
#
######################################################################

couplingMVA = function(){
  require("RANN")
  #############################
  #Start of user input section#
  #############################
  
  #set the number of keys to be used per dataset (1 or 2)
  no_of_keys=2
  
  #set the method to couple, 0 is direct comparison, 1 is nearest neighbour, 2 is geoquery
  couple_method=2
  
  #Set the primary column names to be used as key in set 1 and set 2
  key1_nameA="Coo_X"
  key2_nameA="Coo_X"
  
  #Set the secondary column names to be used as key. These keys are only used if no_of_keys is set to 2
  key1_nameB="Coo_Y"
  key2_nameB="Coo_Y"
  
  #Set the location and names of the input datasets
  #Set1Name=paste0(settings$Testcodes,"/Set1.Rda")
  #Set1Name=paste0(settings$Ruwe_Datasets,"/MS_kabels_BAR_KLAK_Zakking.Rda")
  Set1Name=paste0(settings$Ruwe_Datasets,"/16. Zakking/Sample1k_Zakking.Rda")
  Set2Name=paste0(settings$Ruwe_Datasets,"/10. BAG/Woonplaatsen/Woonplaatsen_shp.Rda")
  #Set2Name=paste0(settings$Testcodes,"/Set2.Rda")
  
  
  #Set the location and name of the output dataset
  outFileName=paste0(settings$Testcodes,"/trial.Rda")
  
  #############################
  #End of user input section  #
  #############################
  
  ptm <- proc.time()  
  #Load in the sets to be coupled. I do this by loading the object pointer into a variable
  #(Set?NameCheck) and then performing a get on that to store the object in my preferred data table
  Set1NameCheck=load(Set1Name)
  Set1=get(Set1NameCheck)
  
  #IMPORTANT NOTE: if the loaded set contains more then 1 data.table you have to directly specify mindataset
  
  Set2NameCheck=load(Set2Name)
  #Set2=mindataset
  Set2=get(Set2NameCheck)
  
  cat('Sets loaded, proceeding...\n')
  


  
  #check whether both sets are data.tables to avoid errors down the line
  if (is.data.table(Set1)==F || is.data.table(Set2)==F ){
    stop("One of the input sets is not in data table format, please correct")
  }
  
  #Here we do some error catching for:
  #A: coordinates with type character that should be numeric
  #B: coordinates in RDC should be converted to lat/lon format
  
  
  #A Check whether the class of key 1 is numeric and make sure that key 2 has the same class. Note that this
  #fails if key 2 column content is not supposed to be the same class, but in that case 
  #the coupling will fail anyway
  
  if (no_of_keys==1){
    if (is.numeric(Set1[,get(key1_nameA)])==T)
    {
      Set2[,(key2_nameA):=as.numeric(get(key2_nameA))]
    }
  }
  else if (no_of_keys==2){ 
    #in this case we assume to have x,y data so they HAVE to be numeric in both sets
    Set1[,(key1_nameA):=as.numeric(get(key1_nameA))]
    Set2[,(key2_nameA):=as.numeric(get(key2_nameA))]
    Set1[,(key1_nameB):=as.numeric(get(key1_nameB))]
    Set2[,(key2_nameB):=as.numeric(get(key2_nameB))]
  }
  
  #B: can use the existing function AHA_RDCtoGPS for this
  #First check whether the coordinates are in RDC by just checking whether they
  #exceed say 1000. Check this somewhere halfway, since NA are grouped at the beginning or end
  
  
  if (no_of_keys==2){
    if (Set1[round(nrow(Set1)/2)+1,get(key1_nameA)]>1000){
      Set1[,c(eval(key1_nameA),eval(key1_nameB)):=
             AHA_RDCtoGPS(subset(Set1,select=c(get(key1_nameA),get(key1_nameB))))]  
    }
    if (Set2[round(nrow(Set2)/2)+1,get(key2_nameA)]>1000){
      Set2[,c(eval(key2_nameA),eval(key2_nameB)):=
             AHA_RDCtoGPS(subset(Set2,select=c(get(key2_nameA),get(key2_nameB))))]  
    }
  }
  
  
  #In case of a single key, set the key such that unique will know how to filter
  if (no_of_keys==1){
    setkeyv(Set1,key1_nameA)
    setkeyv(Set2,key2_nameA)
  }
  
  
  #Merge the datasets
  #----
  #We merge in the following fashion: the "NOR" dataset (Set 1 for now) is the base table in which duplicates (in the key)
  #are allowed to exist (for example because we have low and medium powerlines in the same place). The "environmental" dataset 
  #(Set 2 for now) is the dependent table in which duplicates are not allowed, because that would mean that we have e.g.
  #at the same time 10-20 and 21-50 trees nearby, which should not be possible
  
  #first run unique on Set2 to make sure that there are no double entries
  
  cat("Identifying unique elements of Set2 \n")
  uniSet2=unique(Set2)
  
  #If there are 2 keys, we assume that they are x and y location and we merge on nearest neighbour
  #if there is only 1 key we don't need this operation
  
  if (no_of_keys==2){
    
    
    #IMPORTANT NOTE: nn2 fails when there is an NA in Set1 or Set2
    #some preprocessing and error catching for that is needed!!
    #CHECK whether I can work with this by using the RADIUS option of nn2
    
    #Radius works as expected, so I can identify NA entries, change them
    #to some ridiculous value and make sure that they are not a problem for
    #the search algorithm. Make sure to adjust parameters in set1 to large positive
    #and set2 to large negative (otherwise NA will couple to NA)
    
    #Select only x and y columns from both sets and work with that for the neighbour ID
    Set1Sub=Set1[,c(eval(key1_nameA),eval(key1_nameB)),with=F]
    Set2Sub=uniSet2[,c(eval(key2_nameA),eval(key2_nameB)),with=F]
    
    #identify the rows with NA in X or Y in both sets
    S1X_NA_IDs=which(is.na(Set1Sub[,get(key1_nameA)]))
    S1Y_NA_IDs=which(is.na(Set1Sub[,get(key1_nameB)]))
    S2X_NA_IDs=which(is.na(Set2Sub[,get(key2_nameA)]))
    S2Y_NA_IDs=which(is.na(Set2Sub[,get(key2_nameB)]))
    
    #change their values to large numbers
    Set1Sub[S1X_NA_IDs,eval(key1_nameA):=1e99]
    Set1Sub[S1Y_NA_IDs,eval(key1_nameB):=1e99]
    Set2Sub[S2X_NA_IDs,eval(key2_nameA):=-1e99]
    Set2Sub[S2Y_NA_IDs,eval(key2_nameB):=-1e99]
    
    cat("Starting nearest neighbour identification \n")
    
    
    #find the nearest neighbour indices using nn2 from the RANN package
    indexNearest=nn2(Set2Sub,Set1Sub,k=1)
    
    cat("Nearest neighours identified, proceed to coupling \n")
    

    
    #Correcting the identified neighbours which were actually NAs
    indexNearest$nn.idx[S1X_NA_IDs]=0
    indexNearest$nn.idx[S1Y_NA_IDs]=0
    
    #return(indexNearest)
    
    #Create a new column in Set1 with the merge ID, 
    #also insert a column in uniSet2 with these IDs (essentially row numbers for Set2)
    Set1[,mID:=indexNearest$nn.idx]
    uniSet2[,mID:=1:nrow(uniSet2)]
    
    #Now the indices have been added to the sets and they are unique in uniSet2 so we can
    #do the coupling if we set the key to only mID
  }
  
  #Set the keys for the different datasets, note that having different column names only works 
  #if I use the X[Y] merge option, not the merge(X,Y). Additionally, note that I use setkeyv instead of setkey, because
  #this allows me to use variables to set the names for the columns and keep the programming clean
  if (no_of_keys==1){
    setkeyv(Set1,key1_nameA)
    setkeyv(uniSet2,key2_nameA)
  }
  else if (no_of_keys==2) { #in this situation the merge is x,y so we switch to mID as key to merge
    setkey(Set1,mID)
    setkey(uniSet2,mID)
  }
  
  cat("Start coupling \n")
  #finally we merge uniSet2 into Set1
  coupledSet=uniSet2[Set1] #this notation couples according to the key in Set1, so an entry that doesn't exist
  #in Set 2 gets NA, but an entry that doesn't exist in Set1 (and does in Set2) does not appear at all
  #coupledSet=uniSet2[Set1,nomatch=0] #This is perhaps a cleaner option, then above, because it eliminates rows for which we don't
  #have the "environmental" information
  
  cat("Coupling done, starting cleanup \n")
  
  #throw away the merge identifier to clean up the data and the x/y data in the second set, because
  #it isn't descriptive
  if (no_of_keys==2){
    coupledSet[,mID:=NULL]
    coupledSet[,eval(key2_nameA):=NULL]
    coupledSet[,eval(key2_nameB):=NULL]
    
    #if column names were the same in both dataset, adjust them to their old name here
    if (key1_nameA==key2_nameA){
      setnames(coupledSet,paste0("i.",key1_nameA),key1_nameA)
    }
    if (key1_nameB==key2_nameB){
      setnames(coupledSet,paste0("i.",key1_nameB),key1_nameB)
    }
  }
  
  
  cat("Saving coupled set \n")
  #write the coupled set to the RDA file
  save(coupledSet,file=outFileName)
  
  #write the coupled set to a csv file
  #write.csv(coupledSet,outFileName,row.names=F)
  cat("Freeing memory \n")
  gc()
  
  cat("Done! \n")
  
  return(coupledSet)
}

