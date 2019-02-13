#
# This is the user-interface definition of a Shiny web application.
#

library(shiny)
library(shinythemes)
library(shinyjs)

## UI extensions
jscode <- "
shinyjs.refocus = function(e_id) {
document.getElementById(e_id).focus();
}"

shinyUI(fluidPage(
  theme=shinytheme("readable"),
  shinyjs::useShinyjs(),
  extendShinyjs(text = jscode, functions = "refocus"),
  tags$style(HTML(" .shiny-input-container:not(.shiny-input-container-inline) { width: 100%; height: 100%; }")),
  tags$head(tags$style(type="text/css", ".container-fluid {  max-width: 1000px;}")),
  
  # Application title
  titlePanel("Customer Complaints Analyzer"),
  
  # Sidebar with a slider input for user rating
  sidebarLayout(
    position = "left",
    sidebarPanel(
      actionButton("analyze", "Analyze It!", width = "100%"),
      p(),              
      actionButton("updateRandom", "Random Complaint",width = "100%"),  
      p(),              
      actionButton("reset", "Reset", width = "100%")  
      #p(),              
      #actionButton("help", "Help", width = "100%")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      column(12,
        #verbatimTextOutput("msg"),
        textAreaInput(
          inputId="complaintIn", 
          label="Complain here:", 
          resize = "none", rows = 20
        )
      )
    )
  )
))

