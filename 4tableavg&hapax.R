# Libraries
library(gutenbergr)
library(tidyverse)
library(tidytext)

# First 30000 words of a book = first 30000 rows of the tidy dataset 
# of the book.  If you have those in your environment already
# then you don't need the next bit of code

# Get books from Gutenberg and tidy them as usual--------------------

ja_books <- gutenberg_download(c(161,42671,141,158,121,105))
hg_books <- gutenberg_download(c(35,36,5230,1013))
bs_books <- gutenberg_download(c(1260,768,969,9182,767))

ja_tidy <- ja_books %>%
  unnest_tokens(word, text)
hg_tidy <- hg_books %>%
  unnest_tokens(word, text) 
bs_tidy <- bs_books %>% 
  unnest_tokens(word, text) 

#get types and hapax for austin

ja_types_hapax <- ja_tidy %>%
  group_by(gutenberg_id) %>%
  slice(1:30000) %>%   
  count(word, sort = TRUE) %>% 
  summarize(types = n(), hapax = sum(n == 1))            

#get types and hapax for wells
hg_types_hapax <- hg_tidy %>%
  group_by(gutenberg_id) %>% 
  slice(1:30000) %>% 
  count(word, sort = TRUE) %>% 
  summarize(types = n(), hapax = sum(n == 1)) 

#types and hapax for brontes

bs_types_hapax <- bs_tidy %>%
  group_by(gutenberg_id) %>%
  slice(1:30000) %>% 
  count(word, sort = TRUE) %>% 
  summarize(types = n(), hapax = sum(n == 1)) 

#gets average and combines all tables

ja_avg <- ja_types_hapax %>% 
  summarize(author = "Austen", avg_types = mean(types), avg_hapax = mean(hapax)) 
hg_avg <- hg_types_hapax %>% 
  summarize(author = "Wells", avg_types = mean(types), avg_hapax = mean(hapax)) 
bs_avg <- bs_types_hapax %>% 
  summarize(author = "Brontes", avg_types = mean(types), avg_hapax = mean(hapax)) 

avg_types_hapax <- rbind(ja_avg, hg_avg, bs_avg)  


