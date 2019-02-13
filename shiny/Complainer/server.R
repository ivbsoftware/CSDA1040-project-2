library(shiny)
library(shinyjs)

library(forcats)
library(tm)
library(stopwords)
library(caTools)
library(philentropy)
library(ngram)
library(topicmodels)
library(SnowballC)
library(wordcloud)

#set.seed(123)

## Reading in Data 
numTopics <- 12 # 7, 10 or 30
topicsFileName <- paste("./lda",numTopics,".Rds", sep = "") 
train.lda <- readRDS(topicsFileName)
train.terms <- terms(train.lda, 10) # first 10 terms of every topic

full_sorted_issues_list <- unlist (readRDS("full_sorted_issues_list.Rds"))
sampleComplaints <- readRDS("sampleComplaints.Rds") 
sampleComplaintsNum <- nrow(sampleComplaints)

issueTopicsProbMat <- readRDS("issueTopicsProbMat.Rds")

maxDistThreshold = 0.004

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

## calc sorted distances from vector 'vec'
## to each row in matrix 'mat'
distInfo <- function(vec, mat) {
  retVec <- vector(length = nrow(mat))
  for (i in 1:nrow(mat)) {
    retVec[i] <- JSD(rbind(vec,mat[i,]), est.prob = "empirical")
  }
  retVec <- sort(retVec, index.return=TRUE)
  return (retVec)
}

## Vector arr is a vector of probabilities for the topics to fit
## the corpus, sum of arr values is 1.
## Best topics are those with probability > (1/numOfTopics + testThreshold)
testThreshold =0.001
getBestTopicNums <- function(arr, threshold) {
  topNums <- sort(arr, index.return=TRUE, decreasing = TRUE)$ix
  cutThrshold <- testThreshold + 1/length(arr)
  print("------------")
  print(cutThrshold)
  print(arr)
  arr <- arr[arr > cutThrshold]
  print(arr)
  print("------------")
  len <- length(arr)
  if (len > 0) {
    return (topNums[1:len])
  }
  else {
    return (NULL)
  }
}

## server logic
shinyServer(function(input, output, session) {

  observeEvent(input$reset, {
    updateTextInput(session, "complaintIn", value="")
    js$refocus("complaintIn") 
  })
  
  # callback - 'updateRandom' button clicked 
  observeEvent(input$updateRandom, {
    id <- sample.int(sampleComplaintsNum,1)
    text <- sampleComplaints[[id,"Consumer complaint narrative"]]
    updateTextInput(session, "complaintIn", value=text)
    js$refocus("analyze") #set focus to 'Analyze It" 
  })

  dataModal <- function(issuesFoundTextArr) {
    modalDialog(
      verbatimTextOutput(issuesFoundTextArr),
      span('(Try the name of a valid data object like "mtcars", ',
           'then a name of a non-existent object like "abc")'),

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
    topicsFoundNum <- 0
    issuesFoundNum <- 0
    if (wordcount(text) > 1) {
      topics <- posterior(train.lda,dtm)
      print(topics$topics[1,])
  
      print ("5 terms from betsTopics: ")
      bestTopics <- getBestTopicNums(topics$topics[1,])
      print(bestTopics)
      print(train.terms[1:10,bestTopics])
      
      # no topics, no issues...
      topicsFoundNum <- length(bestTopics)
      issuesFoundNum <- 0
      if (topicsFoundNum > 0) {
        issuesFound <- distInfo(topics$topics[1,],issueTopicsProbMat)
        print (paste("issuesFound: ",issuesFound))
        
        list.condition <- sapply(issuesFound$x, function(v) v <= maxDistThreshold)
        probableIssues  <- issuesFound$ix[list.condition]
        maxLen <- 3
        if (length(probableIssues) > maxLen) {
          probableIssues <- probableIssues[1:maxLen]
        }
        print ("probableIssues: ")
        print (probableIssues)
        issuesFoundNum <- length(probableIssues)
      }
    }
    # response 
    if (topicsFoundNum > 0 & issuesFoundNum > 0) {
      issuesFoundText <- full_sorted_issues_list[probableIssues]
      #print (paste("issuesFoundText: ", issuesFoundText))
      
      message <- "It seems that you are having one or more of the following issues with one of the financial institutions:<ul>"
      for (issue in issuesFoundText) {
        message <- paste0(message, "<li>", issue, "</li>")
      }
      message <- paste0(message, "</ul>")
      message <- paste0(message, 
        "<br>We will pass your complaint to our service department and let you know.")
      
    } else {
      message <- "We don't know what your problem is... Try to complain somewhere else."      
    }

    showModal(modalDialog(
      HTML(message),
      title = "We reviewed your complaint...",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))    
  })

  # Set focus to 'Complain Here" field
  js$refocus("complaintIn") 

})
