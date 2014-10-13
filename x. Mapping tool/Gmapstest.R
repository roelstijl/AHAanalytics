library(plotGoogleMaps)
library(shiny)

runApp(list(
  ui = pageWithSidebar(
    headerPanel('Map'),
    sidebarPanel(""),
    mainPanel(uiOutput('mymap'))
  ),
  server = function(input, output){
    output$mymap <- renderUI({
      data(meuse)
      coordinates(meuse) = ~x+y
      proj4string(meuse) <- CRS("+init=epsg:28992")
      m <- plotGoogleMaps(meuse, filename = 'myMap1.html', openMap = F)
      tags$iframe(
        srcdoc = paste(readLines('myMap1.html'), collapse = '\n'),
        width = "100%",
        height = "600px"
      )
    })
  }
))