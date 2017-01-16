library(tidyverse)
library(gutenbergr)
library(tidytext)
library(stringi)

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


first_line <- function(work) {
  df <- gutenberg_download(work, meta_fields = c("title", "author"))
  
  lines <- df %>%
    unnest_tokens(lines, text, token = "lines")
  
  for (line in lines$lines) {
    charsWhite <- stri_stats_latex(line)[3]
    words <- stri_stats_latex(line)[4]
    if((words > 10) & ((charsWhite - words) < 2)) {
      firstLine <- line
      break
    }
  }
  
  return(firstLine)

}


records <- analyze(c(42, 57, 5400, 777, 874))
