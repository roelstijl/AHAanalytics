# Roel Stijl (Bearingpoint) 2015
# Tool for the preprocessing of data for MVA purposes
cfg <<-list()
cfg$max_categories <<- 25
cfg$namelength     <<- 25
cfg$samplesize     <<- 5000

# Prepare some variables for the first run
filechooser <<- choose.files(default = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/*.Rda"))
filename <<- file_path_sans_ext(basename(filechooser))

# Sample the data
if(file.exists(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Settings/",filename,"_sample.Rda")))
{ # Load data if ran before
  load(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Settings/",filename,"_sample.Rda"),envir = globalenv())
} else { # Run it for the first time
  cat("Creating sample set .... ")
  SetName  = load(filechooser)
  mindataset = data.table(get(SetName))
  
  dataset    <<- mindataset[sample(1:nrow(mindataset),cfg$samplesize)]
  
  l_ply(names(dataset)[laply(dataset,is.character)],function(x) dataset[,eval(x):=as.factor(dataset[,get(x)])])
  l_ply(names(dataset)[laply(dataset,function(x) class(x)[1])=="POSIXct"],function(x) dataset[,eval(x):=as.Date(get(x))])
  l_ply(names(dataset)[laply(dataset,function(x) class(x)[1])=="integer"],function(x) dataset[,eval(x):=as.numeric(get(x))])
  l_ply(names(dataset)[laply(dataset,function(x) class(x)[1])=="Date"],function(x) dataset[,eval(x):=as.numeric(get(x))])
  
  setcolorder(dataset,cn(dataset))
  save(dataset,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Settings/",filename,"_sample.Rda"))
  cat("Done\n ")
}

# Settings in excel file
if(file.exists(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Settings/",filename,"_metadata.xlsx")))
{ # load an excel file with the metadata if none exist
 temp = read.xlsx(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Settings/",filename,"_metadata.xlsx"),1)
 temp$selected = as.logical(temp$selected)
 metadata <<- data.table(temp)
} else { # Create metadata
  metadata  <<- data.table(
  names     = cn(dataset),
  selected  = T,
  shortname = paste0(substring(cn(dataset),1,cfg$namelength),ifelse(nchar(cn(dataset))>=cfg$namelength,"...","")))
}

# load or calculate the correlations, depending of wether this has been done before
if(file.exists(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Settings/",filename,"_correlations.Rda")))
{
 load(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Settings/",filename,"_correlations.Rda"),envir = globalenv())
} else {
 Correlations <<- AHA_MVA_CorrelationTable(dataset);
 save(Correlations,file = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Settings/",filename,"_correlations.Rda"))
}

setcolorder(Correlations$types,c(metadata$names,"row.names"))
setcolorder(Correlations$correlations,c(metadata$names,"row.names"))

Variable_names <<- as.list(metadata$names)
names(Variable_names) <<-metadata$names

#Convert the data into something usefull
cfg$elements          <<- 1:min(15,length(metadata$selected));
cfg$checkboxes        <<- 1:length(metadata$selected);  
names(cfg$checkboxes) <<- metadata$shortname;
names(cfg$elements)   <<- rep("-",15);

# Prepare some variables to be used to keep track of the GUI
update_no_1 <<- 0;
update_no_2 <<- 0;
save_to_file <<- 0;
next_button<<-0;
last_button<<-0;
updatedcheckbox <<-0;
createtrain <<-0;
createfull <<-0;
targetvariable <<-0;


# Define UI for application that draws a histogram
shinyUI(fluidPage(  
  # Sidebar with a slider input for the number of bins
    sidebarPanel(
      h3("Preprocessing MVA"),
      textOutput("dataset_details"),
      
      selectInput("Target_Variable", label="Target variable",choices = Variable_names,selected = Variable_names[1]),
      
      selectInput("Target_Value", label="Target value",
                  choices = setNames(as.list(unique(dataset[,metadata$names[1],with=F])),as.list(unique(dataset[,metadata$names[1],with=F])))),      
fluidRow(
  column(4,actionButton("vorige_x", label = "Last")),
  column(4,actionButton("save_to_file", label = "Save")),
  column(4,actionButton("volgende_x", label = "Next"))
),
      fluidRow(
      column(2,radioButtons("radiobutton", label = "",choices=cfg$elements,selected="4")),
          
      column(10,checkboxGroupInput("Checkbox", label = "",
              choices = cfg$checkboxes[cfg$elements],selected = as.character(cfg$elements[1]-1+which(metadata[cfg$elements,selected]==1)))
      ))
      ,
fluidRow(
  column(6,textInput("Tr_size",label="Train size",value = "10000")),
  column(6,textInput("Tr_tgt", label="Train % ",value = "50"))
),

fluidRow(
  column(6,textInput("tst_size",label="Test size",value = "10000")),
  column(6,textInput("rnd_seed",label="Seed",value = "10"))
),

fluidRow(
  column(6,actionButton("Gen_test_train", label = "Test/Train")),
  column(6,actionButton("Gen_full", label = "Full Data"))
)),
    
    # Show a plot of the generated distribution
    mainPanel( 
      fluidRow(h5("Aantal niet ingevulde of NA rijen"),
      plotOutput("Chart_frequencies", height = "100px")),
      
      h5("Lift per geselecteerde component"),
      plotOutput("Lift_graph", height = "200px"),
      
      
      h5("Correlatie coefficient top 10"),
      tableOutput("Cor_Table"))
      
))