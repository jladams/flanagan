library(tidyverse)
library(gutenbergr)
library(tidytext)


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

df <- middle_end(42)