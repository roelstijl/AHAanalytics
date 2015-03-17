# Start the service and update with content
shinyServer(function(input, output,session) {
  
  # This contains the updating of fields  
  observe({   
    
    metadata[elements,selected := is.element(elements, input$Checkbox)]
    
    # Set the elements to display
    if(input$volgende_x[1] > next_button) {elements <<-elements+15; next_button<<-next_button+1};
    if(input$vorige_x[1]   > last_button) {elements <<-elements-15; last_button<<-last_button+1};
    
    # Make sure they dont exeed bounds
    if(elements[1]<1) elements <<- elements - elements[1]+1
    if(elements[length(elements)]>length(metadata$selected)) elements <<- elements - (-length(metadata$selected)+elements[length(elements)])-1
        
    # Update names when button pressed
    if(input$Update_Name[1] > update_no_2){      
      names(checkboxes)[elements[1]-1+as.numeric(input$radiobutton[1])] <<- input$text
      metadata[elements[1]-1+as.numeric(input$radiobutton[1]),names]<<-input$text
      update_no_2 <<- update_no_2 + 1}
    
    # Update checkbox values
    updateCheckboxGroupInput(session,"Checkbox", label = "", choices  = checkboxes[elements], 
                             selected = as.character(elements[1]-1+which(metadata[elements,selected]==1)));
    
    # Save the file
    if(input$save_to_file[1] > save_to_file)
    { write.xlsx(metadata,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/header.xlsx"),row.names=FALSE);
      save_to_file <<- save_to_file + 1;
      isolate(cat("Saved to file \n"))
    }
  })

  output$Chart_frequencies <- renderPlot({
        
    par(mfrow=c(1, 1), mar=c(2, 5, 0, 2))
    barplot(sort(table(dataset[,as.numeric(input$radiobutton[1])+elements[1]-1,with=F],useNA="always"),decreasing=TRUE)[1:25],
            horiz=TRUE,las=1,
            col=terrain.colors(length(table(dataset[as.numeric(input$radiobutton[1])+elements[1]-1]))));
  })
  
  output$piechart2 <- renderPlot({
    par(mfrow=c(1, 1), mar=c(2, 5, 0, 2))
    barplot(sort(table(dataset[,as.numeric(input$radiobutton[1])+elements[1]-1,with=F],useNA="always"),decreasing=TRUE)[1:25],
            horiz=TRUE,las=1,
            col=terrain.colors(length(table(dataset[as.numeric(input$radiobutton[1])+elements[1]-1]))));
  })
  
  output$tableout <- renderTable ({
    
    tables=sort(table(dataset[,as.numeric(input$radiobutton[1])+elements[1]-1,with=F],useNA="always"),decreasing=TRUE);
    Frequenties = tables[1:10];
    Frequenties = Frequenties[!is.na(Frequenties)];
    Frequenties = as.data.frame(Frequenties);
    
    })
    
  output$tableout2 <- renderTable ({
    Eerste_waarden = dataset[1:10,as.numeric(input$radiobutton[1])+elements[1]-1]
    as.data.frame(as.character(Eerste_waarden));
  })

  # Stop the app
  observe({if(input$close[1] > 0){stopApp()}})
})