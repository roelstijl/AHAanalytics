library(shiny)
# Server component, notice that <<- denotes a global call. Else this won't work

# Load the datasets required
len = length(dataset[,1])

# Start the service and update with content
shinyServer(function(input, output,session) {
  
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

})