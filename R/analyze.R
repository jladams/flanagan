library(tidyverse)
library(gutenbergr)
library(tidytext)
library(stringi)
library(stringr)

# Get first line of work based on word count of line vs. number of blank characters
first_line <- function(df, wordCount = 9, diff = 3) {
  
  lines <- df %>%
    unnest_tokens(lines, text, token = "lines")
  
  i <- 1
  
  for (line in lines$lines) {
    charsWhite <- stri_stats_latex(line)[3]
    words <- stri_stats_latex(line)[4]
    if((words >= wordCount) & ((charsWhite - words) <= diff)) {
      firstLine <- line
      break
    }
    i <- i + 1
  }
  
  return(lines[i,])
  
}


# Get first word of first line
first_word <- function(df) {
  
  fword <- first_line(df) %>%
    mutate(beginning = word(lines)) %>%
    select(-lines)

  return(fword) 
  
}

# Get middle word and last sentence
middle_end <- function(df){
  
  words <- df %>%
    unnest_tokens(words, text)

  sentences <- df %>%
    unnest_tokens(sentences, text, token = "sentences")

  beginning <- first_word(df)
  middle <- words[length(words$words)/2, ]
  end <- sentences[length(sentences$sentences), ]
  
  df <- left_join(beginning, middle)
  df <- left_join(df, end)
  colnames(df) <- c("gutenberg_id", "title", "author", "beginning", "middle", "end")
  
  return(df)
}

# Provide a vector of Gutenberg ID numbers to get the first word, middle word, and last sentence
analyze <- function(work) {
  
  dlist <- lapply(work, gutenberg_download, meta_fields = c("title", "author"))
  
  df <- lapply(dlist, middle_end) %>%
    bind_rows
  
  return(df)
}

# Get vector of Gutenberg ID numbers by exact subject
books_by_subject <- function(search) {
  
  subs <- gutenberg_subjects %>%
    filter(subject == search)

  return(subs$gutenberg_id)
  
}


