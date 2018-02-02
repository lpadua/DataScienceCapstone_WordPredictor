#
# Next Word Predictor - Shiny App
#       User Interface Code
#       by Luis Padua - jan2018
#    
# consists of creation of Text Inputs and buttons to the user interacts with the program

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Next Word Predictor"),
        
        mainPanel(
                tabsetPanel(
                        tabPanel("Predictor", 
                                 br(),
                                 fluidRow(
                                         column(12, textAreaInput("inputEntry", "Type Your Text:", "", rows = 3, cols = 80))
                                 ),
                                 br(),
                                 h3("Software Predictions"),
                                 fluidRow(
                                         column(4, actionButton("predict1", "Predict 1")),
                                         column(4, actionButton("predict2", "Predict 2")),
                                         column(4, actionButton("predict3", "Predict 3"))
                                 ),
                                 br(),
                                 fluidPage(
                                         column(12, "Wait for Corpus loading:"),
                                         column(12, textOutput("outputTest"))
                                 )
                        ),
                        tabPanel("About", verbatimTextOutput("about"))
                )
        )
))


