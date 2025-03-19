# Libraries for Silge & Robinson Chapter 1 ----

library(gutenbergr)
library(tidyverse)
library(tidytext)

#Import  books
ja_books <- gutenberg_download(c(161,42671,141,158,121,105))
hg_books <- gutenberg_download(c(35, 36, 1013, 5230))
bs_books <- gutenberg_download(c(767,768,969,1260,9182))

#austin books
ja_tidy_no_stop <- ja_books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

ja_word_freq_no_stop <- ja_tidy_no_stop %>%
  count(word, sort = TRUE)

#wells books
hg_tidy_no_stop <- hg_books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

hg_word_freq_no_stop <- hg_tidy_no_stop %>%
  count(word, sort = TRUE)

#bronte sisters
bs_tidy_no_stop <- bs_books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

bs_word_freq_no_stop <- bs_tidy_no_stop %>%
  count(word, sort = TRUE)


par(mfrow = c(3, 1))  

barplot(top_n(ja_word_freq_no_stop, 10, n)$n, 
        names.arg = top_n(ja_word_freq_no_stop, 10, n)$word, 
        main = "Austen No Stopwords", 
        las = 2)

barplot(top_n(hg_word_freq_no_stop, 10, n)$n, 
        names.arg = top_n(hg_word_freq_no_stop, 10, n)$word, 
        main = "Wells No Stopwords", 
        las = 2)

barplot(top_n(bs_word_freq_no_stop, 10, n)$n, 
        names.arg = top_n(bs_word_freq_no_stop, 10, n)$word, 
        main = "Bronte Sisters No Stopwords", 
        las = 2)


