#######################################################################
# Written by Michiel Musterd - 09-02-2015 (last update:11-02-2015)
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


# Non-implemented nice to haves:
# -----------------
# - Coupling between PC4 and PC6 by truncation of the PC6 set
# - Maximum on the nearest neighbour coupling (e.g. only couple within 1 km radius)
# - ...
#
######################################################################

couplingMVA = function(){
  #############################
  #Start of user input section#
  #############################
  
  #set the number of keys to be used per dataset (1 or 2)
  no_of_keys=2
  
  #Set the primary column names to be used as key in set 1 and set 2
  key1_nameA="LON..east."
  key2_nameA="Coo_X"
  
  #Set the secondary column names to be used as key. These keys are only used if no_of_keys is set to 2
  key1_nameB="LAT..north."
  key2_nameB="Coo_Y"
  
  #Set the location and names of the input datasets
  #Set1Name=paste0(settings$Testcodes,"/Set1.Rda")
  Set1Name=paste0(settings$Ruwe_Datasets,"/18. KNMI/Sample1k_KNMI.Rda")
  Set2Name=paste0(settings$Ruwe_Datasets,"/16. Zakking/500x250_Zakking_Nederland.Rda")
  #Set2Name=paste0(settings$Testcodes,"/Set2.Rda")
  
  
  #Set the location and name of the output dataset
  outFileName=paste0(settings$Testcodes,"/CoupledToNearestWithLatLon.Rda")
  
  #############################
  #End of user input section  #
  #############################
  
  ptm <- proc.time()  
  #Load in the sets to be coupled. I do this by loading the object pointer into a variable
  #(Set?NameCheck) and then performing a get on that to store the object in my preferred data table
  Set1NameCheck=load(Set1Name)
  Set1=get(Set1NameCheck)
  
  
  Set2NameCheck=load(Set2Name)
  Set2=get(Set2NameCheck)
  
  #check whether both sets are data.tables to avoid errors down the line
  if (is.data.table(Set1)==F || is.data.table(Set1)==F ){
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
  #exceed say 1000. Check this on the last element, since NA are grouped at the beginning
  
  if (no_of_keys==2){
    if (Set1[nrow(Set1),get(key1_nameA)]>1000){
      Set1[,c(eval(key1_nameA),eval(key1_nameB)):=
             AHA_RDCtoGPS(subset(Set1,select=c(get(key1_nameA),get(key1_nameB))))]  
    }
    if (Set2[nrow(Set2),get(key2_nameA)]>1000){
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
  
  uniSet2=unique(Set2)
  
  #If there are 2 keys, we assume that they are x and y location and we merge on nearest neighbour
  #if there is only 1 key we don't need this operation
  
  if (no_of_keys==2){
    N=nrow(Set1) #check the number of rows in Set1 for looping

    #create an empty data.table to be filled inside the loop
    indexVec=data.table(ind=(1:N)*0)
    
    
    for (i in 1:N){
      if (i %% 100==0) {
      cat("Coupling ",i," of ",N,"\n")
      }
      
      #calculate vector with distance of i to uniSet2 
      vec=with(uniSet2,(Set1[i,get(key1_nameA)]-get(key2_nameA))^2+(Set1[i,get(key1_nameB)]-get(key2_nameB))^2)
          
      #catch NA entries in Set1, just leave indexVec$ind at 0 for those
      if (is.na(Set1[i,get(key1_nameA)])==F && is.na(Set1[i,get(key1_nameB)])==F){
        #calculate minimum of that vector and store the index
        indexVec[i,ind:=which.min(vec)]
      }
    }#loop over all rows in Set1
    
    
    #Create a new column in Set1 with the merge ID, 
    #also insert a column in uniSet2 with these IDs (essentially row numbers for Set2)
    Set1[,mID:=indexVec$ind]
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
  
  
  #finally we merge uniSet2 into Set1
  coupledSet=uniSet2[Set1] #this notation couples according to the key in Set1, so an entry that doesn't exist
                           #in Set 2 gets NA, but an entry that doesn't exist in Set1 (and does in Set2) does not appear at all
  #coupledSet=uniSet2[Set1,nomatch=0] #This is perhaps a cleaner option, then above, because it eliminates rows for which we don't
  #have the "environmental" information
  
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
  
  
  
  #write the coupled set to the RDA file
  save(coupledSet,file=outFileName)
  
  #write the coupled set to a csv file
  #write.csv(coupledSet,outFileName,row.names=F)
  
  return(proc.time() - ptm)
}


