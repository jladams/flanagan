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
  
  # Iterate through each line, compare white space to number of words (higher amount of white space indicates formatting for table of contents, etc.)
  for (line in lines$lines) {
    charsWhite <- stri_stats_latex(line)[3]
    words <- stri_stats_latex(line)[4]
    if((words >= wordCount) & ((charsWhite - words) <= diff)) {
      firstLine <- line
      break
    }
    i <- i + 1
  }
  
  # Return the line detected as the first of the body
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

  # Tokenize work into individual words  
  words <- df %>%
    unnest_tokens(words, text)

  # Tokenize work into sentences
  sentences <- df %>%
    unnest_tokens(sentences, text, token = "sentences")

  # Run first_word() on work to get the beginning
  beginning <- first_word(df)
  
  # Find the word in the middle
  middle <- words[length(words$words)/2, ]
  
  # Find the word at the end
  end <- sentences[length(sentences$sentences), ]
  
  # Combine beginning, middle, and end
  df <- beginning %>%
    left_join(middle) %>%
    left_join(end)
  
  # Rename columns appropriately
  colnames(df) <- c("gutenberg_id", "title", "author", "beginning", "middle", "end")
  
  return(df)
}

# Provide a vector of Gutenberg ID numbers to get the first word, middle word, and last sentence
analyze <- function(work) {
  
  # Retrieve a list of dataframes containing the text to be analyzed
  dlist <- lapply(work, gutenberg_download, meta_fields = c("title", "author"))
  
  # Apply the middle_end() function to each and then bind the list together to create a single dataframe of results
  df <- lapply(dlist, middle_end) %>%
    bind_rows()
  
  return(df)
}

# Get vector of Gutenberg ID numbers by exact subject
books_by_subject <- function(search) {
  
  subs <- gutenberg_subjects %>%
    filter(subject == search)

  return(subs$gutenberg_id)
  
}

# Analyze works based on exact match of subject heading
analyze_by_subject <- function(search) {
  
  works <- books_by_subject(search)
  
  df <- analyze(works)
  
  return(df)
}

