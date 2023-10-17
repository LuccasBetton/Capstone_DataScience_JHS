# Libraries
library(dplyr)
library(sweary)
library(qdap)
library(tm)
library(stringr)
library(shinythemes)
library(rhandsontable)

# Code
source('./prediction_algorithm.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("cerulean"),
    titlePanel("Predict Word"),
    sidebarLayout(
      
      sidebarPanel(
        #textOutput("text_fix"),
        h4("Summary"),
        h5("This application is part of \n to the Capstone Project for JohnS Hopkins Data Science Course."),
        h4("How to Use"),
        h5("* Write in the box a word or setence in order to predict the next word. "),
        h5("* Just press 'Enter' or click in the button 'Predict'"),
        h5("* The top three words it will be presented"),
        ),
      
      mainPanel(
        div(style="display: flex; align-items: center;",
            textInput(inputId = "user_input", "User input"),
            actionButton("predict_button", "Predict", style = "margin-top: 10px; margin-left: 15px;")
        ),
        rHandsontableOutput("my_table")
       
      )
    ),
    tags$script(HTML('
    document.addEventListener("keydown", function(e) {
      if (e.keyCode === 13) {
        Shiny.setInputValue("enter_pressed", Math.random(), {priority: "event"});
      }
    });
  ')),
    tags$div(
      style = "text-align: left; padding: 10px; font-size: 12px;",
      "Data Base used for word prediction available by SwiftKey."
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #output$text_fix <- renderText("Test")
  
  
  observeEvent(input$enter_pressed, {
     
     input_clean <- prep_input(input$user_input,swear_words)
     sel_words <- predict_word(input_clean)
     string_words <- paste(sel_words$pred_word, collapse = " ")

     output$my_table <- renderRHandsontable({
       rhandsontable(sel_words,
       colHeaders = c("Predict Word"),
       rowHeaders = c("Word 1","Word 2","Word 3"),
       readOnly = TRUE,
       colWidths = c("200px"),  # Set the width of each column
       rowHeaderWidth = 80
       )
     })
     
  })
  
  observeEvent(input$predict_button, {
    
    input_clean <- prep_input(input$user_input,swear_words)
    sel_words <- predict_word(input_clean)
    string_words <- paste(sel_words$pred_word, collapse = " ")
    
    output$my_table <- renderRHandsontable({
      rhandsontable(sel_words,
                    colHeaders = c("Predict Word"),
                    rowHeaders = c("Word 1","Word 2","Word 3"),
                    readOnly = TRUE,
                    colWidths = c("200px"),  # Set the width of each column
                    rowHeaderWidth = 80
      )
    })
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
