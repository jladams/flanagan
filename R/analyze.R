library(tidyverse)
library(gutenbergr)
library(tidytext)
library(stringi)
library(stringr)

# Get first line of work based on word count of line vs. number of blank characters
# Accepts a dataframe containing a variable called "text" as occurs in the gutenbergr package
# Returns a tibble with the row of data containing the first line of body text (as detected by the function)
# df = data frame to be analyzed, no default 
# wordCount = minimum number of words in line considered for the first line, default is 9
# diff = minimum difference between number of words and number of white spaces in line considered for first line, default is 3
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
# Accepts a dataframe containing a variable called "text" as occurs in the gutenbergr package
# Returns a tibble with the first word of the first line as detected by first_line()
# df = data frame to be analyzed, no default 
# wordCount = passed to first_line()
# diff = passed to first_line()
first_word <- function(df, wordCount = 9, diff = 3) {
  
  fword <- first_line(df, wordCount, diff) %>%
    mutate(beginning = word(lines)) %>%
    select(-lines)

  return(fword) 
  
}

# Get first word, middle word, and last sentence of a work
# Accepts a dataframe containing variables "gutenberg_id", "title", "author", "text" as occurs in the gutenbergr package
# Returns a tibble with beginning, middle, and end 
# df = data frame to be analyzed, no default 
# wordCount = passed to first_line()
# diff = passed to first_line()
middle_end <- function(df, wordCount = 9, diff = 3){
    
  # Tokenize work into individual words  
  words <- df %>%
    unnest_tokens(words, text)
  
  # Tokenize work into sentences
  sentences <- df %>%
    unnest_tokens(sentences, text, token = "sentences")
  
  # Run first_word() on work to get the beginning
  beginning <- first_word(df, wordCount, diff)
  
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

# Error handling for downloading books
# Downloads text with title and author metadata, assigns global variable name to downloaded text, passes errors and warnings
# Accepts gutenberg ID number
download_text <- function(book) {
  print(paste("Downloading Gutenberg ID", book))
  return(
    tryCatch(
      assign(paste0("gutenberg_", book), gutenberg_download(book, meta_fields = c("title", "author")), envir = .GlobalEnv),
             error = function(e) {
               print(paste("*** Error with Gutenberg ID ***", book))
               return(data_frame(gutenberg_id = integer(), title = character(), author = character(), beginning = character(), end = character()))
               },
             warning = function(e) {
               print(paste("*** Warning with Gutenberg ID ***", book))
               return(data_frame(gutenberg_id = integer(), title = character(), author = character(), beginning = character(), end = character()))
             }
    )
  )
}

# Error handling for analyzing work
# Accepts a dataframe containing variables "gutenberg_id", "title", "author", "text" as occurs in the gutenbergr package
# Returns a tibble with beginning, middle, and end 
# df = data frame to be analyzed, no default 
# wordCount = passed to first_line()
# diff = passed to first_line()
analyze_work <- function(work, wordCount = 9, diff = 3) {
  print(paste("Analyzing Gutenberg ID", work[1,1]))
  return(
    tryCatch(middle_end(work, wordCount, diff),
             error = function(e) {
               print(paste("*** Error with Gutenberg ID ***", work[1,1]))
               return(data_frame(gutenberg_id = integer(), title = character(), author = character(), beginning = character(), end = character()))
             },
             warning = function(e) {
               print(paste("*** Warning with Gutenberg ID ***", work[1,1]))
               return(data_frame(gutenberg_id = integer(), title = character(), author = character(), beginning = character(), end = character()))
             }
    )
  )
}

# Provide a vector of Gutenberg ID numbers to get the first word, middle word, and last sentence
# Accepts a vector containing Gutenberg ID numbers
# Returns a tibble with beginning, middle, and end
# Automatically saves downloaded texts as variables
# work = vector of Gutenberg ID numbers, no default
# wordCount = passed to first_line()
# diff = passed to first_line()
analyze <- function(work, wordCount = 9, diff = 3) {
 
  print("Downloading Files")
   
  # Retrieve a list of dataframes containing the text to be analyzed
  dlist <- lapply(work, download_text)
  
  # Apply the middle_end() function to each and then bind the list together to create a single dataframe of results
  df <- lapply(dlist, analyze_work, wordCount, diff) %>%
    bind_rows()
  
  return(df)
}

# Get vector of Gutenberg ID numbers by exact subject
# Accepts a LCSH or LCC subject heading as found in gutenberg_subjects as a character string
# Returns a vector of Gutenberg ID numbers matching the subject heading
# search = character string of LCSH or LCC subject heading
books_by_subject <- function(search) {
  
  subs <- gutenberg_subjects %>%
    filter(subject == search)

  return(subs$gutenberg_id)
  
}

# Analyze works based on exact match of subject heading
# Accepts a LCSH or LCC subject heading as found in gutenberg_subjects as a character string
# Returns a tibble containting gutenberg_id, title, author, beginning, middle, and end of all works matching provided subject heading
# search = character string of LCSH or LCC subject heading
# wordCount = passed to first_line()
# diff = passed to first_line()
analyze_by_subject <- function(search, wordCount = 9, diff = 3) {
  
  works <- books_by_subject(search)
  
  df <- analyze(works, wordCount, diff)
  
  return(df)
}

View(gutenberg_subjects)



