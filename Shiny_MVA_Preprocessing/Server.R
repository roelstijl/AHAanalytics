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

    # Update the selectinput
     updateSelectInput(session, 
                       "Target_Value", 
                       choices = 
                         setNames(laply(unique((dataset[,input$Target_Variable,with=F])),as.list),
                         laply(unique((dataset[,input$Target_Variable,with=F])),as.list)),
                       label = NULL,  
                       selected = NULL);
    
    
    # Save the file
    if(input$save_to_file[1] > save_to_file)
    { metadata$selected = metadata$selected+0
      write.xlsx(metadata,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/",basename(filechooser),".xlsx"),row.names=FALSE);
      save_to_file <<- save_to_file + 1;
      isolate(cat("Saved to file \n"))
    }
  })

  output$Chart_frequencies <- renderPlot(
    frequencybar(dataset[,as.numeric(input$radiobutton[1])+elements[1]-1,with=F]),
  )
  
  output$Lift_graph <- renderPlot(
     Simple_Lift(dataset,input$Target_Variable,input$Target_Value,Variable_names[[as.numeric(input$radiobutton[1])+elements[1]-1]])
  )
  
  output$Cor_Table <- renderTable ({
    cor_tabel = cor_tabel[,as.numeric(input$radiobutton[1])+elements[1]-1),]
    setnames(cor_tabel,c("Variabele","Correlatie","xx","Methode"))
    setorder(cor_tabel,-Correlatie)
    cor_tabel[,c(1,2,4),with=F][1:25]
    })

})