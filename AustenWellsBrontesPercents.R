library(gutenbergr)
library(tidyverse)
library(tidytext)

# Downloading ja books
ja_books <- gutenberg_download(c(161, 42671, 141, 158, 121, 105))

hg_books <- gutenberg_download(c(35, 36, 1013, 5230))
bs_books <- gutenberg_download(c(767, 768, 969, 1260, 9182))

#tidy ja books
ja_tidy <- ja_books %>%
  unnest_tokens(word, text)

#frequencies for ja books
ja_word_freq <- ja_tidy %>%
  count(word, sort = TRUE)

#number of words in the ja
total_words_ja <- sum(ja_word_freq$n)

#ja_percent
ja_word_freq <- ja_word_freq %>%
  mutate(ja_percent = n / total_words_ja)

#tidy hg
hg_tidy <- hg_books %>%
  unnest_tokens(word, text)

#frequencies hg books
hg_word_freq <- hg_tidy %>%
  count(word, sort = TRUE)

#hg word count
total_words_hg <- sum(hg_word_freq$n)

#create column  hg with the frequency percentage
hg_word_freq <- hg_word_freq %>%
  mutate(hg_percent = n / total_words_hg)

#tidy bs books
bs_tidy <- bs_books %>%
  unnest_tokens(word, text)

#frequencies for bs
bs_word_freq <- bs_tidy %>%
  count(word, sort = TRUE)

#total bs 
total_words_bs <- sum(bs_word_freq$n)

# Create a new column "bs_percent" with the frequency percentage
bs_word_freq <- bs_word_freq %>%
  mutate(bs_percent = n / total_words_bs)

#Merges the dataframes
all_words_percents <- ja_word_freq %>%
  full_join(hg_word_freq, by = "word") %>%
  full_join(bs_word_freq, by = "word")

#stopper of the stopwords
all_words_percents_no_stop <- all_words_percents %>%
  anti_join(stop_words, by = "word")

#first few row viewer
head(all_words_percents_no_stop)
