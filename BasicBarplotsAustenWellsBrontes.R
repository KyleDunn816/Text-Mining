# Libraries for Silge & Robinson Chapter 1 ----

library(gutenbergr)
library(tidyverse)
library(tidytext)
library(ggplot2)
# Import Austen books ----

ja_books <- gutenberg_download(c(161,42671,141,158,121,105))
hg_books <- gutenberg_download(c(35, 36, 1013, 5230))
bs_books <- gutenberg_download(c(767,768,969,1260,9182))


# Tidy Austen books ----

ja_tidy <- ja_books %>%
  unnest_tokens(word,text)

hg_tidy <- hg_books %>%
  unnest_tokens(word,text)

bs_tidy <- bs_books %>%
  unnest_tokens(word,text)

# Get word frequencies for Austen books ----

ja_word_freq <- ja_tidy %>%
  count(word,sort=TRUE)

ja_word_freq <- ja_tidy %>%
  count(word,sort=TRUE)

hg_word_freq <- hg_tidy %>%
  count(word,sort=TRUE)

hg_word_freq <- hg_tidy %>%
  count(word,sort=TRUE)

bs_word_freq <- bs_tidy %>%
  count(word,sort=TRUE)

bs_word_freq <- bs_tidy %>%
  count(word,sort=TRUE)

# Create basic barplots for Austen books ----

# We will spend some time later making better plots.
# For now we'll just make some basic utilitarian bar charts 
# suitable for exploratory data analysis.

# First one for the whole dataset.
barplot(ja_word_freq$n)
barplot(bs_word_freq$n)
barplot(hg_word_freq$n)


# Then one for the top 1000 words.
barplot(top_n(ja_word_freq,1000)$n)
barplot(top_n(bs_word_freq,1000)$n)
barplot(top_n(hg_word_freq,1000)$n)

# Then a labeled one for the top 10 words.
barplot(top_n(ja_word_freq,10)$n,names=top_n(ja_word_freq,10)$word)
barplot(top_n(bs_word_freq,10)$n,names=top_n(bs_word_freq,10)$word)
barplot(top_n(hg_word_freq,10)$n,names=top_n(hg_word_freq,10)$word)

# Note that the arrow buttons on the Plots tab 
# of the File pane will flip back and forth between plots
# and the Zoom button will give you a full-screen look.


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

hg_tidy <- hg_books %>%
  unnest_tokens(word,text)

bs_tidy <- bs_books %>%
  unnest_tokens(word,text)

# Get word frequencies for Austen books ----

ja_word_freq <- ja_tidy %>%
  count(word,sort=FALSE)


hg_word_freq <- hg_tidy %>%
  count(word,sort=FALSE)


bs_word_freq <- bs_tidy %>%
  count(word,sort=FALSE)
# It might be interesting to see What happens 
# if you set sort=FALSE (or just delete "sort=TRUE")
# above.  



#Bar Creation center of Bar Creation

# First one for the whole dataset the big one
barplot(ja_word_freq$n, main = "Jane Austen - All Words")
barplot(hg_word_freq$n, main = "H.G. Wells - All Words")
barplot(bs_word_freq$n, main = "Brontë Sisters - All Words")
#1000 words
barplot(top_n(ja_word_freq, 1000)$n, main = "Jane Austen - Top 1000 Words")
barplot(top_n(hg_word_freq, 1000)$n, main = "H.G. Wells - Top 1000 Words")
barplot(top_n(bs_word_freq, 1000)$n, main = "Brontë Sisters - Top 1000 Words")


#top 10 words
barplot(top_n(ja_word_freq, 10)$n, names = top_n(ja_word_freq, 10)$word, 
        main = "Jane Austen - Top 10 Words")

barplot(top_n(hg_word_freq, 10)$n, names = top_n(hg_word_freq, 10)$word, 
        main = "H.G. Wells - Top 10 Words")

barplot(top_n(bs_word_freq, 10)$n, names = top_n(bs_word_freq, 10)$word, 
        main = "Brontë Sisters - Top 10 Words")


