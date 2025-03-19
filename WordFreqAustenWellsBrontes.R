# Libraries for Silge & Robinson Chapter 1 ----

library(gutenbergr)
library(tidyverse)
library(tidytext)

# Import Austen books ----

#first assignment 
library(gutenbergr)
tale <- gutenberg_download(98)
#Hg wells
hg_books <- gutenberg_download(c(35, 36, 1013, 5230))
#bronte Sisters
bs_books <- gutenberg_download(c(767,768,969,1260,9182))

#ja_books
#105 Persuasion
#121 northanger abbey
#141 Lady susan
#946 emma
#1342 pride and prejudice



ja_books <- gutenberg_download(c(161,42671,141,158,121,105))

# Tidy Austen books ----

ja_tidy <- ja_books %>%
  unnest_tokens(word,text)

# Get word frequencies for Austen books ----

ja_word_freq <- ja_tidy %>%
  count(word,sort=TRUE)

# It might be interesting to see What happens 
# if you set sort=FALSE (or just delete "sort=TRUE")
# above.  
