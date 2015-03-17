# Prepare some variables for the first run
filechooser= choose.files(default = paste0(settings$Analyse_Datasets,"/5. MVA analyseset/*.Rda"))
load(filechooser)
mindataset <<- mindataset
len        <<- length(mindataset[,1])

# savedheader   = read.xlsx(paste0(settings$Analyse_Datasets,"/5. MVA analyseset/*.Rda"),1, as.data.frame=TRUE)
metadata   <<- data.table(
  names     = cn(mindataset),
  selected  = T,
  shortname = paste0(substring(cn(mindataset),1,15),ifelse(nchar(cn(mindataset))>=15,"...",""))
  )

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
names(radiobuttons) <<- matrix("-",elements[length(elements)]);

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(
    h2(paste0("Dataset: ", colnames(metadata$names[1]))),
       h4("Dataset tool AHA (c) Bearingpoint 2014")),
  
  # Sidebar with a slider input for the number of bins
    sidebarPanel
    (      
      textInput("text",value = "New name",label=NULL),
      actionButton("Update_Name", label = "Update name"),       
      br(),

      actionButton("save_to_file", label = "Save to file"),br(),
      actionButton("close", label = "Close"),br(),br(),
      
      textOutput("value"),
      actionButton("vorige_x", label = "Last 15"),actionButton("volgende_x", label = "Next 15"),br(),
      div(),
      fluidRow(
      column(2,radioButtons("radiobutton", label = "",choices=elements,selected="4")),
          
      column(10,checkboxGroupInput("Checkbox", label = "",
              choices = checkboxes[elements],selected = as.character(elements[1]-1+which(metadata[elements,selected]==1)))
      ))),
    
    # Show a plot of the generated distribution
    mainPanel( 
      fluidRow(
      column(6,plotOutput("Chart_frequencies"), height = "600px"),
      column(6,plotOutput("piechart2"), height = "600px")
      ),
      
      fluidRow(
      column(6,h5("Aantal elementen"),tableOutput("tableout")),
      column(6,h5("50 eerste waarden"),tableOutput("tableout2"))
      ))
))