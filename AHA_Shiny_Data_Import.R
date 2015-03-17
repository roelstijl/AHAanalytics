AHA_Data_Import_Shiny = function(dataset,curdataname="Default",header = "empty"){
# Load Generic settings -----------------

if (header == "empty"){
  header       = data.frame(matrix(0,length(colnames(mindataset)),4))
  header[,1]   = data.frame(colnames(mindataset))
  header[,2]   = (colnames(mindataset))
  header[,3]   = matrix(0,ncol(mindataset))
  header[,4]   = matrix("comment",length(colnames(mindataset)))
  header[,5]   = sapply(mindataset, class,simplify=TRUE);
  header[,6:8] = t(mindataset[1:3,])
  colnames(header) = c(curdataname,"Original name","Meenemen","Notities","Class","Value 1","Value 2","Value 3")
}


# Server component, notice that <<- denotes a global call. Else this won't work
# Load the datasets required
len = length(dataset[,1])
# Prepare some variables for the first run
update_no_1 <<- 0;
update_no_2 <<- 0;

save_to_file <<- 0;
text_in_box <<- "";
next_button<<-0;
last_button<<-0;
updatedcheckbox <<-0;

#Convert the data into something usefull
elements <<- 1:min(15,length(header[,3]));
checkboxes <<- 1:length(header[,3]);  names(checkboxes)  <<- paste0(substring(header[,1],1,15),"...");
radiobuttons <<- elements; names(radiobuttons) <<- matrix("-",elements[length(elements)]);
  
# Shiny UI functions-----------------
shinyApp(
UI = (
  # Define UI for application that draws a histogram
  fluidPage(
    
    # Application title
    titlePanel(
      h2(paste0("Dataset: ", colnames(header)[1])),
      h4("Dataset tool AHA (c) Bearingpoint 2014")),
    
    # Sidebar with a slider input for the number of bins
    sidebarPanel
    (
      
      textInput("text",value = "New name",label=NULL),
      actionButton("Update_Name", label = "Update name"),       
      br(),
      
      selectInput("Select_Class", label=NULL,choices = 
                    list("integer"="integer","numeric"="numeric","logical"="logical","character"="character", "dmy_hm"="dmy_hm",
                         "dmy" = "dmy", "ymd" = "ymd", "my_hms" = "my_hms","ymd_hms"="ymd_hms"),selected = "character"),
      actionButton("Update_Class", label = "Update class"),
      br(),
      
      actionButton("save_to_file", label = "Save to file"),br(),
      actionButton("close", label = "Close"),br(),br(),
      
      textOutput("value"),
      actionButton("vorige_x", label = "Last 15"),actionButton("volgende_x", label = "Next 15"),br(),
      div(),
      fluidRow(
        column(2,radioButtons("radiobutton", label = "",choices=radiobuttons,selected="4")),
        
        column(10,checkboxGroupInput("Checkbox", label = "",
                                     choices = checkboxes[elements],selected = as.character(elements[1]-1+which(header[elements,3]==1)))
        ))),
    
    # Show a plot of the generated distribution
    mainPanel( 
      plotOutput("piechart"),
      
      fluidRow(
        column(6,h5("Aantal elementen"),tableOutput("tableout")),
        column(6,h5("50 eerste waarden"),tableOutput("tableout2"))
      ))
)),

# Shiny Server -------------------------------
# Start the service and update with content
server = (function(input, output,session) {
  
  # This contains the updating of fields  
  observe({   
    header[elements,3]<<-is.element(elements, input$Checkbox)+0
    
    # Set the elements to display
    if(input$volgende_x[1] > next_button) {elements <<-elements+15; next_button<<-next_button+1};
    if(input$vorige_x[1]   > last_button) {elements <<-elements-15; last_button<<-last_button+1};
    
    # Make sure they dont exeed bounds
    if(elements[1]<1) elements <<- elements - elements[1]+1
    if(elements[length(elements)]>length(header[,3])) elements <<- elements - (-length(header[,3])+elements[length(elements)])-1
    
    # Update names when button pressed
    if(input$Update_Name[1] > update_no_2){      
      names(checkboxes)[elements[1]-1+as.numeric(input$radiobutton[1])] <<- input$text
      header[elements[1]-1+as.numeric(input$radiobutton[1]),1]<<-input$text
      update_no_2 <<- update_no_2 + 1}
    
    if(input$Update_Class[1] > update_no_1){      
      header[elements[1]-1+as.numeric(input$radiobutton[1]),5]<<-(input$Select_Class)
      update_no_1 <<- update_no_1 + 1}
    
    # Update checkbox values
    updateCheckboxGroupInput(session,"Checkbox", label = "", choices  = checkboxes[elements], 
                             selected = as.character(elements[1]-1+which(header[elements,3]==1)));
    
    # Save the file
    if(input$save_to_file[1] > save_to_file)
    { write.xlsx(header,file="header.xlsx",row.names=FALSE);
      save_to_file <<- save_to_file + 1;
      isolate(cat("Saved to file \n"))
    }
  })
  
  # Stop the app
  observe({if(input$close[1] > 0){stopApp(header)}})
  
  # Start rendering some graphs and text
  output$value <- renderText({ 
    input$volgende_x[1]
    paste0(names(checkboxes)[elements[1]-1+as.numeric(input$radiobutton[1])], 
           "[",(length(dataset[,as.numeric(input$radiobutton[1])+elements[1]-1])),"]: ",class(dataset[,as.numeric(input$radiobutton[1])+elements[1]-1]));
  })    
  
  output$piechart <- renderPlot({
    
    tables=sort(table(dataset[,as.numeric(input$radiobutton[1])+elements[1]-1],useNA="always"),decreasing=TRUE);
    
    par(mfrow=c(1, 1), mar=c(2, 15, 0, 2))
    barplot(tables[1:25],
            horiz=TRUE,las=1,
            col=terrain.colors(length(table(dataset[as.numeric(input$radiobutton[1])+elements[1]-1]))));
  })
  
  output$tableout <- renderTable ({
    
    tables=sort(table(dataset[,as.numeric(input$radiobutton[1])+elements[1]-1],useNA="always"),decreasing=TRUE);
    Frequenties = tables[1:10];
    Frequenties = Frequenties[!is.na(Frequenties)];
    Frequenties = as.data.frame(Frequenties);
    
  })
  
  output$tableout2 <- renderTable ({
    Eerste_waarden = dataset[1:10,as.numeric(input$radiobutton[1])+elements[1]-1]
    as.data.frame(as.character(Eerste_waarden));
  })
  
}))

}