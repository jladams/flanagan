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


# books <- sample(50:600, 10)
# records <- analyze(books)


#--------------------------------------
# App

server <- function(input, output) {
  
  firstWords <- reactiveValues()
  firstWords$df <- read_csv("../data/firsts.csv")
  
  recordNum <- reactiveValues()
  recordNum$a <- isolate(length(unique(firstWords$df$gutenberg_id)))
  
  df <- eventReactive(input$textSubmit, {
    gutenberg_download(as.numeric(records$gutenberg_id[recordNum$a - 1]), meta_fields = c("title", "author"))
    })
  
  observeEvent(input$textSubmit, {
    tmp <- data_frame(gutenberg_id = ifelse(!is.null(unique(records$gutenberg_id[recordNum$a - 1])), 
                                            unique(records$gutenberg_id[recordNum$a - 1]), 
                                            NA), 
                      first = input$firstWord)
    
    isolate(firstWords$df <- rbind(firstWords$df, tmp) %>% 
              filter(first != "Click 'Submit Text' to begin")
            )
    
    recordNum$a <- recordNum$a + 1
    
    write_csv(firstWords$df, "../data/firsts.csv")
    write_csv(newRecords(), "../data/records.csv")
  })
 
  newRecords <- reactive({
    records %>%
      left_join(firstWords$df)
  }) 
  
  
  output$idNum <- renderText({unique(df()$gutenberg_id)})
  output$text <- renderPrint({df()$text})
  output$data <- renderTable({newRecords()})
  output$firsts <- renderTable({firstWords$df})
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("ID Number"),
      textOutput("idNum"),
      hr(),
      textInput("firstWord", "Type first word:", "Click 'Submit Text' to begin"),
      actionButton("textSubmit", "Submit Text")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Text", verbatimTextOutput("text")),
        tabPanel("Data", tableOutput("data")),
        tabPanel("First Words", tableOutput("firsts"))
      )
    )
  )
)

shinyApp(ui = ui, server = server)
