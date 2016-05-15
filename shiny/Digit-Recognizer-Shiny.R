####################CAMBIAR###################
setwd("C:/Users/deyban.perez/Documents/Repos/Digit_Recognizer")
##############################################
source("src/functions.r")
install("shinythemes")
install("shiny")
install("e1071")
library(e1071)
library(shinythemes)
require(shiny)

#WAITING: LOADING test.csv
test = read.csv("data/test.csv")
load(file = "models/model_linear.rda")
load(file = "models/model_polynomial.rda")
load(file = "models/model_radial.rda")
load(file = "models/model_sigmoid.rda")
runApp(
  list(
    ui = fluidPage(theme = shinytheme("cerulean"),
      headerPanel('Digit-Recognizer'),
      sidebarPanel(
        numericInput("n", "Introduzca la fila del test.csv:", min = 1, max = nrow(test),value = "Ej: 50"),
        br(),
        actionButton("goButton", "RECONOCER DIGITO", style='float:center;padding:20px; font-size:150%'),br(),
        p("Click en el boton para reconocer el digito"),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
      ),
      mainPanel(
        plotOutput("plot"),
        h4('Resultados:'),
        verbatimTextOutput("nText"))
        
      ),
   
    
    server = function(input, output) { 
      ntext <- eventReactive(input$goButton, {
        input$n
      })
      
      output$plot <- renderPlot({
        n <- ntext()
        plotDigit(n, test)
      })
      
      output$nText <- renderPrint({
        n <- as.numeric(ntext())
        testModel(n, test)
        
      })
      
      
      
    }
  )
)
