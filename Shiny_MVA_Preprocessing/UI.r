# Prepare some variables for the first run
filechooser <<- choose.files(default = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/*.Rda"))
load(filechooser)
dataset <<- mindataset
len     <<- length(dataset[,1])
namelength = 20
Correlations <<- AHA_MVA_CorrelationTable(dataset,colnumber=1)

# load an excel file with the metadata if none exist
ifelse(file.exists(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/",basename(filechooser),".xlsx")),
{temp = read.xlsx(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/",basename(filechooser),".xlsx"),1, as.data.frame=TRUE)
 temp$selected = as.logical(temp$selected)
 metadata <<- data.table(temp)},
{metadata  <<- data.table(
  names     = cn(dataset),
  selected  = T,
  shortname = paste0(substring(cn(dataset),1,namelength),ifelse(nchar(cn(dataset))>=namelength,"...","")))})

Variable_names <<- as.list(metadata$names)
names(Variable_names) <<-metadata$names

update_no_1 <<- 0;
update_no_2 <<- 0;

save_to_file <<- 0;
text_in_box <<- "";
next_button<<-0;
last_button<<-0;
updatedcheckbox <<-0;

#Convert the data into something usefull
elements <<- 1:min(15,length(metadata$selected));
checkboxes <<- 1:length(metadata$selected);  
names(checkboxes)   <<- metadata$shortname;
names(elements) <<- matrix("-",elements[length(elements)]);

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(h4("Preprocessing tool AHA Liander/Bearingpoint 2015")),
  
  # Sidebar with a slider input for the number of bins
    sidebarPanel
    (       textInput("Sample size",value = "10000",label=NULL),

      selectInput("Target_Variable", label=NULL,choices = Variable_names,selected = Variable_names[1]),
      
      selectInput("Target_Value", label=NULL,
                  choices = setNames(as.list(unique(dataset[,metadata$names[1],with=F])),as.list(unique(dataset[,metadata$names[1],with=F])))),
      
      textInput("text",value = "New name",label=NULL),
      actionButton("Update_Name", label = "Update name"),       
      br(),

      actionButton("save_to_file", label = "Save to file"),br(),
      
      textOutput("value"),
      div(),
      fluidRow(
      column(2,radioButtons("radiobutton", label = "",choices=elements,selected="4")),
          
      column(10,checkboxGroupInput("Checkbox", label = "",
              choices = checkboxes[elements],selected = as.character(elements[1]-1+which(metadata[elements,selected]==1)))
      )),
      
      actionButton("vorige_x", label = "Last 15"),actionButton("volgende_x", label = "Next 15"),br()
      ),
    
    # Show a plot of the generated distribution
    mainPanel( 
      fluidRow(h5("Aantal niet ingevulde of NA rijen"),
      plotOutput("Chart_frequencies", height = "100px")),
      
      h5("Lift per geselecteerde component"),
      plotOutput("Lift_graph", height = "200px"),
      
      
      h5("Correlatie coefficient top 10"),
      tableOutput("Cor_Table"))
      
))