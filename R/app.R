library(shiny)
library(tidyverse)
library(gutenbergr)
library(tidytext)

#--------------------------------------
# Functions

middle_end <- function(work){
  df <- gutenberg_download(work, meta_fields = c("title", "author"))
  
  words <- df %>%
    unnest_tokens(words, text)
  
  sentences <- df %>%
    unnest_tokens(sentences, text, token = "sentences")
  
  middle <- words[length(words$words)/2, ]
  end <- sentences[length(sentences$sentences), ]
  
  df <- left_join(middle, end)
  colnames(df) <- c("gutenberg_id", "title", "author", "middle", "end")
  
  return(df)
}

analyze <- function(work) {
  df <- lapply(work, middle_end) %>%
    bind_rows
  
  return(df)
}

# records <- analyze(c(42, 57, 5400, 777, 874))


#--------------------------------------
# App

server <- function(input, output) {
  
  newRecords <- reactive({records})
  
  recordNum <- reactiveValues()
  
  df <- eventReactive(input$textSubmit, {
    gutenberg_download(as.numeric(records$gutenberg_id[recordNum]), meta_fields = c("title", "author"))
    })
  
  recordNum$a <- 1:length(records)
  
  eventReactive(input$textSubmit, {
    recordNum <- recordNum() + 1
  })
  
  output$idNum <- renderText({recordNum()})
  output$text <- renderPrint({df()$text})
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("ID Number"),
      textOutput("idNum"),
#      actionButton("idSubmit", "Submit ID Number"),
      hr(),
      textInput("firstWord", "Type first word:", "First word"),
      actionButton("textSubmit", "Submit Text")
    ),
    mainPanel(verbatimTextOutput("text"))
  )
)

shinyApp(ui = ui, server = server)
