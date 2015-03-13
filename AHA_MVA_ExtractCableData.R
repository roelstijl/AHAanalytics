
AHA_MVA_ExtractMofData=function(mindataset){
#Written by Michiel Musterd - 10-03-2014
#This script is meant to extract useful categorical data from the constructie and soort entries in the NOR for moffen
  MofSet=data.table(LowConstructie=gsub(" ","",tolower(as.character(mindataset[,Constructie]))),
                    LowSoort=tolower(as.character(mindataset[,Soort])))
  
  #Categorize the constructie in a few useful groups
  # Wikkelmof Filoform
  # Wikkelmof Cellpack
  # Wikkelmof overig
  # Kunstharsmof
  # Nekaldietmof
  # Krimpmof Raychem
  # Krimpmof Cellpack
  # Krimpmof overig
  # Gietharsmof
  # Oliemof
  # Overig
  # Onbekend  
  MofSet[grep("wikkelmof",MofSet$LowConstructie),Constructie:="Wikkelmof overig"]
  MofSet[grep("krimpmof",MofSet$LowConstructie),Constructie:="Krimpmof overig"]
  #note, the two commands above should be run first because they are more general then two below
  
  MofSet[grep("wikkelmoffiloform",MofSet$LowConstructie),Constructie:="Wikkelmof Filoform"]
  MofSet[grep("wikkelmofcellpack",MofSet$LowConstructie),Constructie:="Wikkelmof Cellpack"]
  MofSet[grep("krimpmofraychem",MofSet$LowConstructie),Constructie:="Krimpmof Raychem"]
  MofSet[grep("krimpmofcellpack",MofSet$LowConstructie),Constructie:="Krimpmof Cellpack"]
  MofSet[grep("kunsthars",MofSet$LowConstructie),Constructie:="Kunstharsmof"]
  MofSet[grep("nekaldiet",MofSet$LowConstructie),Constructie:="Nekaldietmof"]
  MofSet[grep("giethars",MofSet$LowConstructie),Constructie:="Gietharsmof"]
  MofSet[grep("olie",MofSet$LowConstructie),Constructie:="Oliemof"]
  MofSet[is.na(MofSet$Constructie),Constructie:="Overig"]
  MofSet[is.na(MofSet$LowConstructie),Constructie:="Onbekend"]
  MofSet[grep("onbekend",MofSet$LowConstructie),Constructie:="Onbekend"]
  MofSet$Constructie=as.factor(MofSet$Constructie)
  
  #Categorize the soort and change into more readable names
  MofSet[grep("tn|vm|verbindingsmof",MofSet$LowSoort),Soort:="Verbindingsmof"]
  MofSet[grep("em|gm",MofSet$LowSoort),Soort:="Eindmof"]
  MofSet[grep("ae|tae",MofSet$LowSoort),Soort:="Aftak-Eindmof"]
  MofSet[grep("vam|am",MofSet$LowSoort),Soort:="Aftakmof"]
  MofSet[grep("aam|ap",MofSet$LowSoort),Soort:="Aansluitmof"]
  MofSet[is.na(MofSet$Soort),Soort:="Overig"]
  MofSet[grep("onbekend",MofSet$LowSoort),Constructie:="Onbekend"]
  MofSet[is.na(MofSet$LowSoort),Constructie:="Onbekend"]
  MofSet$Soort=as.factor(MofSet$Soort)
  
  
  mindataset$Soort=MofSet$Soort
  mindataset$Constructie=MofSet$Constructie
  
  return(mindataset)
}


AHA_MVA_ExtractCableData=function(mindataset){

#Written by Michiel Musterd - 10-03-2014
#This script is meant to extract useful categorical data from the Fabrikanttype entry in the NOR for cables

#Note: the extra cable information after the + in the fabrikanttype is about extra cables for specific 
#goals e.g. street lighting or grounding. We will ignore those in our specification

#load and preprocess the data (lowercase and , to . conversion)
FabrikantSet=data.table(LowFabrikanttype=gsub(",",".",tolower(as.character(mindataset[,Fabrikanttype]))))


#Isolatiemedium herkenning: Algemene classificering en bijbehorende namen
# YMvK-asmb
# VMvKh-sas
# GPLK
# XLPE:  Overige YMvK en XLPE
# Overig Kunststof: VM (en VMvK)
# Overig
# Onbekend
FabrikantSet[grep("xlpe|ymvk|ymek",FabrikantSet$LowFabrikanttype),Isolatie:="XLPE"]
FabrikantSet[grep("vm",FabrikantSet$LowFabrikanttype),Isolatie:="Overig Kunststof"]
#note, the two commands above should be run first because they are more general then two below

FabrikantSet[grep("ymvk-asmb",FabrikantSet$LowFabrikanttype),Isolatie:="YMvK-asmb"]
FabrikantSet[grep("vmvkh-sas",FabrikantSet$LowFabrikanttype),Isolatie:="VMvKh-sas"]
FabrikantSet[grep("gplk",FabrikantSet$LowFabrikanttype),Isolatie:="GPLK"]
FabrikantSet[is.na(FabrikantSet$Isolatie),Isolatie:="Overig"]
FabrikantSet[is.na(FabrikantSet$LowFabrikanttype),Isolatie:="Onbekend"]
FabrikantSet[grep("onbekend",FabrikantSet$LowFabrikanttype),Isolatie:="Onbekend"]
FabrikantSet$Isolatie=as.factor(FabrikantSet$Isolatie)

#First strip away anything after the +
FabrikantSet[,CutType:=gsub("\\+.*","",FabrikantSet$LowFabrikanttype)]

#Then also remove the spaces around an x, first remove XLPE to avoid issues
FabrikantSet[,CutType:=gsub("xlpe","",CutType)]
FabrikantSet[,CutType:=gsub(" x|x ","x",CutType)]


#recognize rows with multiple x's
# FabrikantSet[grep(".*x.*x.*",FabrikantSet$CutType),]


#Herkenning aantal aders - is het eerste getal voor de x (aanname: aders<10)
x_location=gregexpr("x",FabrikantSet$CutType)
FabrikantSet[,xloc:=rapply(x_location, function(x) head(x, 1))]
FabrikantSet[,Aantal_Aders:=as.factor(as.numeric(substr(FabrikantSet$CutType,xloc-1,xloc-1)))]
FabrikantSet[is.na(Aantal_Aders),Aantal_Aders:="Onbekend"]


#Herkenning geleidermateriaal
FabrikantSet[grep("al",CutType),Geleidermateriaal:="Al"]
FabrikantSet[grep("cu",CutType),Geleidermateriaal:="Cu"]
FabrikantSet[is.na(Geleidermateriaal),Geleidermateriaal:="Onbekend"]
FabrikantSet$Geleidermateriaal=as.factor(FabrikantSet$Geleidermateriaal)

#Herkenning diameter kabel
char1=as.numeric(substr(FabrikantSet$CutType,FabrikantSet$xloc+1,FabrikantSet$xloc+1))
char2=as.numeric(substr(FabrikantSet$CutType,FabrikantSet$xloc+1,FabrikantSet$xloc+2))
char3=as.numeric(substr(FabrikantSet$CutType,FabrikantSet$xloc+1,FabrikantSet$xloc+3))
char4=as.numeric(substr(FabrikantSet$CutType,FabrikantSet$xloc+1,FabrikantSet$xloc+4))
FabrikantSet[,Diameter_Kabel:=pmax(char1,char2,char3,char4,na.rm=T)]

mindataset=cbind(mindataset,FabrikantSet[,list(Isolatie,Aantal_Aders,Geleidermateriaal,Diameter_Kabel)])

return(mindataset)

}
