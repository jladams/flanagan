library(shiny)
library(tidyverse)
library(gutenbergr)
library(tidytext)

server <- function(input, output) {
  df <- eventReactive(input$idSubmit, {
    gutenberg_download(as.numeric(input$textId), meta_fields = c("title", "author"))
    })
  output$text <- renderPrint({df()$text})
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("textId", "Input Gutenberg ID Number", value = 1),
      actionButton("idSubmit", "Submit ID Number"),
      hr(),
      textInput("firstWord", "Type first word:", "First word"),
      actionButton("textSubmit", "Submit Text")
    ),
    mainPanel(verbatimTextOutput("text"))
  )
)

shinyApp(ui = ui, server = server)
