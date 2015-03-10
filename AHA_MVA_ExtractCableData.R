

AHA_MVA_ExtractCableData=function(mindataset){

#Written by Michiel Musterd - 10-03-2014
#This script is meant to extract useful categorical data from the Fabrikanttype entry in the NOR

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
# Overig/Onbekend
FabrikantSet[grep("xlpe|ymvk|ymek",FabrikantSet$LowFabrikanttype),Isolatie:="XLPE"]
FabrikantSet[grep("vm",FabrikantSet$LowFabrikanttype),Isolatie:="Overig Kunststof"]
#note, the two commands above should be run first because they are more general then two below

FabrikantSet[grep("ymvk-asmb",FabrikantSet$LowFabrikanttype),Isolatie:="YMvK-asmb"]
FabrikantSet[grep("vmvkh-sas",FabrikantSet$LowFabrikanttype),Isolatie:="VMvKh-sas"]
FabrikantSet[grep("gplk",FabrikantSet$LowFabrikanttype),Isolatie:="GPLK"]
FabrikantSet[is.na(FabrikantSet$Isolatie),Isolatie:="Overig/Onbekend"]
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

mindataset=cbind(subset(mindataset, select=-c(Diameter,Geleidermateriaal,Isolatiemedium)),FabrikantSet[,list(Isolatie,Aantal_Aders,Geleidermateriaal,Diameter_Kabel)])

return(mindataset)

}
