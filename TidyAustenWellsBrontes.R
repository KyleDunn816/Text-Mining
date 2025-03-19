# Load necessary libraries
library(gutenbergr)
library(dplyr)
library(unrest)

ja_books <- gutenberg_download(c(105, 121, 141, 946, 1342))

hg_books <- gutenberg_download(c(35, 36, 1013, 5230))

bs_books <- gutenberg_download(c(767, 768, 969, 1260, 9182))

#jane austen 
ja_tidy <- ja_books %>%
  mutate(author = "Jane Austen") %>%
  unrest_tokens(word, text)

#Harold Gerald Wells
hg_tidy <- hg_books %>%
  mutate(author = "H.G. Wells") %>%
  unrest_tokens(word, text)

#bronte sisters 
bs_tidy <- bs_books %>%
  mutate(author = "Bronte Sisters") %>%
  unrest_tokens(word, text)

