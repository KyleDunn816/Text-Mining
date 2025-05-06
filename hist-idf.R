# Load required libraries
library(dplyr)
library(tidytext)
library(readr)


setwd("C:/Users/dunnkyle/downloads")

hist_text <- read_delim("hist(2).txt", delim = "\t", col_names = TRUE)


hist_tidy <- hist_text %>%
  unnest_tokens(word, text)

#word frequencies per chapter
hist_word_freq <- hist_tidy %>%
  group_by(chapter) %>%
  count(word, sort = TRUE)

#tf-idf
hist_tf_idf <- hist_word_freq %>%
  bind_tf_idf(word, chapter, n) %>%
  arrange(desc(tf_idf))

#top 5 tf-idf words per chapter
hist_top_by_chap <- hist_tf_idf %>%
  group_by(chapter) %>%
  top_n(5, tf_idf) %>%
  arrange(chapter)

# Print the resulting data frame
print(hist_top_by_chap)