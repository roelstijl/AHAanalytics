library(shiny)
library(plotGoogleMaps)

data(meuse)
# Define server logic required to plot various variables on map
shinyServer(function(input, output) {
  
  output$mymap <- renderUI({
     coordinates(meuse) = ~x+y
    proj4string(meuse) <- CRS("+init=epsg:28992")
    m <- plotGoogleMaps(meuse, filename = 'myMap1.html', openMap = F)
#     tags$iframe(
#       srcdoc = paste(readLines('myMap1.html'), collapse = '\n'),
#       width = "100%",
#       height = "600px"
#     )
  })
})