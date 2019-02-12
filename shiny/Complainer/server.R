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

issueTopicsProbMat <- readRDS("issueTopicsProbMat.Rds")
testThreshold =0.1

## corpus prep function
prepareCorpus <- function(textArr) {
  myCorpus <- Corpus(VectorSource(textArr))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, tolower)
  myStopwords <- c(stopwords(language="en", source="smart"), 
                   "xx", "xxxx", "xxxxxxxxxxxx", "xxxxxxxx")
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  myCorpus <- tm_map(myCorpus, stemDocument)
  
  return (myCorpus)
}

## get best topics function
getBestTopicNums <- function(arr, threshold) {
  topNums <- sort(arr, index.return=TRUE, decreasing = TRUE)$ix
  arr <- arr[arr>threshold]
  return (topNums[1:length(arr)])
}

distInfo <- function(vec, mat) {
  retVec <- vector(length = nrow(mat))
  for (i in 1:nrow(mat)) {
    retVec[i] <- dist(rbind(vec,mat[i,]))
  }
  retVec <- sort(retVec, index.return=TRUE)
  return (retVec)
}

## server logic
shinyServer(function(input, output, session) {

  # callback - 'updateRandom' button clicked 
  observeEvent(input$updateRandom, {
    id <- sample.int(sampleComplaintsNum,1)
    text <- sampleComplaints[[id,"Consumer complaint narrative"]]
    updateTextInput(session, "complaintIn", value=text)
    js$refocus("analyze") #set focus to 'Analyze It" 
  })

  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("dataset", "Choose data set",
                placeholder = 'Try "mtcars" or "abc"'
      ),
      span('(Try the name of a valid data object like "mtcars", ',
           'then a name of a non-existent object like "abc")'),
      if (failed)
        div(tags$b("Invalid name of data object", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # callback - 'recommend' button clicked
  observeEvent(input$analyze, {
    text <- input$complaintIn
    corpus <- prepareCorpus(text)
    dtm <- DocumentTermMatrix(corpus, control = list(minWordLength = 1))
    topics <- posterior(train.lda,dtm)
    issuesFound <- distInfo (topics$topics[1,],issueTopicsProbMat)
    tops <- getBestTopicNums(topics$topics[1,], testThreshold)
    print (tops)
    print (issuesFound)
    showModal(dataModal())
  })

  # When OK button is pressed, attempt to load the data set. If successful,
  # remove the modal. If not show another modal, but this time with a failure
  # message.
  observeEvent(input$ok, {
    # Check that data object exists and is data frame.
    if (!is.null(input$dataset) && nzchar(input$dataset) &&
        exists(input$dataset) && is.data.frame(get(input$dataset))) {
      vals$data <- get(input$dataset)
      removeModal()
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })

  js$refocus("complaintIn") #set focus to 'Complain Here" 

})
