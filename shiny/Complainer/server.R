library(shiny)
library(shinyjs)

library(ggplot2)
library(readr)
library(RDSTK)
library(Matrix)
library(dplyr)
library(forcats)
library(tm)
library(stopwords)
library(caTools)
library(tidytext)
library(Rfast)

## Reading in Data 
numTopics <- 12 # 7, 10 or 30
topicsFileName <- paste("./lda",numTopics,".Rds", sep = "") 
train.lda <- readRDS(topicsFileName)
train.terms <- terms(train.lda, 10) # first 10 terms of every topic

full_sorted_issues_list <- readRDS("full_sorted_issues_list.Rds") 
sampleComplaints <- readRDS("sampleComplaints.Rds") 
sampleComplaintsNum <- nrow(sampleComplaints)

# server logic
shinyServer(function(input, output, session) {

  # callback - 'updateRandom' button clicked 
  observeEvent(input$updateRandom, {
    id <- sample.int(sampleComplaintsNum,1)
    text <- sampleComplaints[[id,"Consumer complaint narrative"]]
    updateTextInput(session, "complaintIn", value=text)
  })

  # callback - 'recommend' button clicked
  observeEvent(input$analyze, {
    output$msg <- renderText({
      "Analysis\nAnalysis\nAnalysis"
    })
  })
  
})
