library(shiny)
library("XML")
# http://stackoverflow.com/questions/18109815/plotgooglemaps-in-shiny-app

xmlfile=xmlParse("storingsschets1.svg")
gmapsfile=readLines('myMap1.html')
rawfile=scan("storingsschets1.svg", what="character")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(img(src = "logo-alliander1-300x118.jpg", height = 118, width = 300)),
  
  # Sidebar with a slider input for the number of bins
    sidebarPanel(
     selectInput("variable", "Variable:",
                  choices=list("storingsschets1.svg" = "storingsschets1.svg", 
                               "storingsschets2.svg" = "storingsschets2.svg", 
                               "storingsschets3.svg" = "storingsschets3.svg"))),
    
    # Show a plot of the generated distribution
    mainPanel( 
#       HTML(" <h1>My first SVG</h1>     
#              <svg width=\"100\" height=\"100\">
#              <circle cx=\"50\" cy=\"50\" r=\"40\" stroke=\"green\" stroke-width=\"4\" fill=\"yellow\" />
#              </svg>"),

      HTML(rawfile)
#       HTML(readLines('myMap1.html'))
      
#       uiOutput('mymap')
#       fluidRow(
#       column(6, ),
#       column(6,h5("50 eerste waarden"))
#       )
)))