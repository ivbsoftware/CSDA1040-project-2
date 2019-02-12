#
# This is the user-interface definition of a Shiny web application.
#

library(shiny)
library(shinythemes)
library(shinyjs)

shinyUI(fluidPage(
  theme=shinytheme("readable"),
  shinyjs::useShinyjs(),
  tags$style(HTML(" .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; height: 100%; }")),
  
  # Application title
  titlePanel("Customer Complaints Analyzer"),
  
  # Sidebar with a slider input for user rating
  sidebarLayout(
    position = "left",
    sidebarPanel(
      actionButton("updateRandom", "Load Random Complaint",width = "100%"),  
      p(),              
      actionButton("analyze", "Analyze It!",width = "100%"),
      p(),
      verbatimTextOutput("msg")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      column(12,
        #verbatimTextOutput("msg"),
        textAreaInput(
          inputId="complaintIn", 
          label="Complain here:", 
          #value="Please write your complaint or load a random one an edit it",
          resize = "none", rows = 20
        )
      )
    )
  )
))

