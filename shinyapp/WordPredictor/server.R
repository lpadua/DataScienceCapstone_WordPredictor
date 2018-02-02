#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(tm)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
        
        source("loadVCorpus.R")
        source("predictCurrentWord.R")
        source("predictNextWord.R")
        source("predictNextWordwithInitials.R")
        
        output$outputTest <- renderText("Corpus dataset loaded sucessfully...")
        output$about <- renderText("This app was created as the milestone Project\nof the Coursera Data Science Specialization by John Hopkins University.\n\ncreated by Luis Padua - jan 2018") 
        
        values <- reactiveValues(type = NULL, pred = NULL)
        
        observe({
                input <- as.character(input$inputEntry)
                
                if(str_sub(input, start=-1) != " " && input != "") {
                        pcw <- predictCurrentWord(input)
                        isolate(values$type <- 1)
                        isolate(values$pred <- pcw)
                        updateActionButton(session, "predict1",
                                           label = pcw[1])
                        updateActionButton(session, "predict2",
                                           label = pcw[2])
                        updateActionButton(session, "predict3",
                                           label = pcw[3])
                }
                if(str_sub(input, start=-1) == " ") {
                        pnw <- predictNextWord(input)
                        isolate(values$type <- 0)
                        isolate(values$pred <- pnw)
                        updateActionButton(session, "predict1",
                                           label = pnw[1])
                        updateActionButton(session, "predict2",
                                           label = pnw[2])
                        updateActionButton(session, "predict3",
                                           label = pnw[3])
                }
        })
        
        observeEvent(input$predict1, {
                if (values$type == 1) {
                        textPred <- strsplit(input$inputEntry, split = " ")[[1]]
                        textPred <- textPred[1:length(textPred)-1]
                        textPred <- paste(textPred, collapse = " ")
                        textPred <- paste(textPred, values$pred[1], "", sep = " ")
                } else {
                        textPred <- paste(input$inputEntry, values$pred[1], " ", sep = "")
                }
                updateTextInput(session, "inputEntry",
                                label = "Type Your Text:",
                                value = textPred)
        })
        
        observeEvent(input$predict2, {
                if (values$type == 1) {
                        textPred <- strsplit(input$inputEntry, split = " ")[[1]]
                        textPred <- textPred[1:length(textPred)-1]
                        textPred <- paste(textPred, collapse = " ")
                        textPred <- paste(textPred, values$pred[2], "", sep = " ")
                } else {
                        textPred <- paste(input$inputEntry, values$pred[2], " ", sep = "")
                }
                updateTextInput(session, "inputEntry",
                                label = "Type Your Text:",
                                value = textPred)
        })
        
        observeEvent(input$predict3, {
                if (values$type == 1) {
                        textPred <- strsplit(input$inputEntry, split = " ")[[1]]
                        textPred <- textPred[1:length(textPred)-1]
                        textPred <- paste(textPred, collapse = " ")
                        textPred <- paste(textPred, values$pred[3], "", sep = " ")
                } else {
                        textPred <- paste(input$inputEntry, values$pred[3], " ", sep = "")
                }
                updateTextInput(session, "inputEntry",
                                label = "Type Your Text:",
                                value = textPred)
        })
})


