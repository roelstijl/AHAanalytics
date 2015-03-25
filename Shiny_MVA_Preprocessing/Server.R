# Start the service and update with content
shinyServer(function(input, output,session) {
  
  # This contains the updating of fields  
  observe({   
    
    metadata[elements,selected := is.element(elements, input$Checkbox)]
    
    setsettings = data.table(Tr_size=as.numeric(input$Tr_size),
                             Tr_tgt=as.numeric(input$Tr_tgt)/100,
                             tst_size=as.numeric(input$tst_size),
                             rnd_seed=as.numeric(input$rnd_seed),
                             Target_Value=input$Target_Value, 
                             Target_Variable=input$Target_Variable)
    
    output$dataset_details= renderText(paste0("File: ",filename,", ",
                                              "Variables: ",ncol(dataset),", ",
                                              "Total size: ",datalength,", ",
                                              "Sample size: ",nrow(dataset),", ",
                                              "Target %: ",100*nrow(dataset[get(input$Target_Variable)==input$Target_Value])/nrow(dataset)))
    
    # Set the elements to display
    if(input$volgende_x[1] > next_button) {elements <<-elements+15; next_button<<-next_button+1};
    if(input$vorige_x[1]   > last_button) {elements <<-elements-15; last_button<<-last_button+1};
    
    # Make sure they dont exeed bounds
    if(elements[1]<1) elements <<- elements - elements[1]+1
    if(elements[length(elements)]>length(metadata$selected)) elements <<- elements - (-length(metadata$selected)+elements[length(elements)])-1
    
    # Update checkbox values
    updateCheckboxGroupInput(session,"Checkbox", label = "", choices  = checkboxes[elements], 
                             selected = as.character(elements[1]-1+which(metadata[elements,selected]==1)));

    # Update the selectinput
    if(input$Target_Variable[1] > targetvariable) {
      targetvariable <<- targetvariable+100;
      }
    
    # Write output if required
    if(input$Gen_test_train[1] > createtrain) {Save_preprocess(setsettings,"Test_Train"); createtrain<<-createtrain+1};
    if(input$Gen_full[1] > createfull) {Save_preprocess(setsettings,"Full"); createfull<<-createfull+1};
    
    cfg$Target_Variable <<-input$Target_Variable
    cfg$Target_Value <<-input$Target_Value
    
    # Save the file
    if(input$save_to_file[1] > save_to_file)
    { metadata$selected = metadata$selected+0
      write.xlsx(metadata,file=paste0(settings$Analyse_Datasets,"/5. MVA analyseset/Settings/",filename,"_metadata.xlsx"),sheetName="Sheet1",row.names=F);
      save_to_file <<- save_to_file + 1;
      isolate(cat("Saved to file \n"))
    }
  })
  
  observe({
    updateSelectInput(session,"Target_Value", 
                      choices = 
                        setNames(laply(unique((dataset[,input$Target_Variable,with=F])),as.list),
                                 laply(unique((dataset[,input$Target_Variable,with=F])),as.list)),
                      label = NULL, selected = NULL)
  })
 

  output$Chart_frequencies <- renderPlot(
    frequencybar(dataset[,metadata$names[as.numeric(input$radiobutton[1])+elements[1]-1],with=F]),
  )
  
  output$Lift_graph <- renderPlot(
     Simple_Lift(dataset,input$Target_Variable,input$Target_Value,Variable_names[[as.numeric(input$radiobutton[1])+elements[1]-1]])
  )
  
  output$Cor_Table <- renderTable ({
    cor_tabel = data.table(Variabele=Correlations$types$row.names,
                           Correlatie=Correlations$correlations[[as.numeric(input$radiobutton[1])+elements[1]-1]],
                           Methode=Correlations$types[[as.numeric(input$radiobutton[1])+elements[1]-1]])
    cor_tabel = cor_tabel[pmatch(metadata$names,cor_tabel$Variabele)][metadata$selected]
    
    setorder(cor_tabel,-Correlatie)
    rbind(cor_tabel[Methode!="Error"][1:min(20,nrow(cor_tabel[Methode!="Error"]))],cor_tabel[Methode=="Error"])
    })

})