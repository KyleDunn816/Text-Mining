# Libraries for Silge Chapter 1 ----
library(tidyverse)
library(tidytext)
library(scales)
library(gutenbergr)
# The scales library provides some additional ways to 
# label the axes, for instance showing 10% instead of .10.

# Basic plot of hg_percent vs ja_percent
ggplot(all_words_percents, aes(x = hg_percent, y = ja_percent)) +
  geom_point()
  
# Points are "bunched up."  Can confirm with transparency.
ggplot(all_words_percents, aes(x = hg_percent, y = ja_percent)) +
  geom_point(alpha=0.1)

# Can stretch out using log scale (like with mammals data)
ggplot(all_words_percents, aes(x = hg_percent, y = ja_percent)) +
  geom_point(alpha=0.1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) 
  
# We see "strata".  Can get "cloud" using jitter.
ggplot(all_words_percents, aes(x = hg_percent, y = ja_percent)) +
  geom_jitter(alpha=0.1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) 

# To get jitter to work need to give it "wiggle room."
ggplot(all_words_percents, aes(x = hg_percent, y = ja_percent)) +
  geom_jitter(alpha=0.1, width=0.3, height=0.3) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) 

# What words are which points?  Can put text geoms down.
ggplot(all_words_percents, aes(x = hg_percent, y = ja_percent)) +
  geom_jitter(alpha=0.1, width=0.3, height=0.3) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_text(aes(label = word)) 
  
# Ack!  Need to not overlap so much!
ggplot(all_words_percents, aes(x = hg_percent, y = ja_percent)) +
  geom_jitter(alpha=0.1, width=0.3, height=0.3) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_text(aes(label = word),check_overlap = TRUE) 
  
#  Which words are on which side?  Dividing line layer.
ggplot(all_words_percents, aes(x = hg_percent, y = ja_percent)) +
  geom_jitter(alpha=0.1, width=0.3, height=0.3) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_text(aes(label = word),check_overlap = TRUE) +
  geom_abline(linetype=2)

# Rest is basically "theme" window-dressing.  Final:
ggplot(all_words_percents, aes(x = hg_percent, y = ja_percent, color = abs(ja_percent - hg_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "black") +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = "H.G. Wells")




#my stuff

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


#NEW STUFF

austen_vs_brontes <- all_words_percents %>%
  select(word, ja_percent, bs_percent) %>%
  drop_na()

ggplot(austen_vs_brontes, aes(x = ja_percent, y = bs_percent, color = abs(bs_percent - ja_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "black") +
  theme(legend.position = "none") +
  labs(y = "Brontes", x = "Jane Austen")


#Wells vs. Brontës

wells_vs_brontes <- all_words_percents %>%
  select(word, hg_percent, bs_percent) %>%
  drop_na()

ggplot(wells_vs_brontes, aes(x = hg_percent, y = bs_percent, color = abs(bs_percent - hg_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "black") +
  theme(legend.position = "none") +
  labs(y = "Brontes", x = "wells")


#no stops 

austen_vs_brontes_no_stop <- all_words_percents_no_stop %>%
  select(word, ja_percent, bs_percent) %>%
  drop_na()

ggplot(austen_vs_brontes_no_stop, aes(x = ja_percent, y = bs_percent, color = abs(bs_percent - ja_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "black") +
  theme(legend.position = "none") +
  labs(y = "Brontes", x = "Jane Austen")


#Wells vs. Brontës

wells_vs_brontes <- all_words_percents %>%
  select(word, hg_percent, bs_percent) %>%
  drop_na()

ggplot(wells_vs_brontes, aes(x = hg_percent, y = bs_percent, color = abs(bs_percent - hg_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "black") +
  theme(legend.position = "none") +
  labs(y = "Brontes", x = "wells")

wells_vs_brontes_no_stop <- all_words_percents_no_stop %>%
  select(word, hg_percent, bs_percent) %>%
  drop_na()

ggplot(wells_vs_brontes_no_stop, aes(x = hg_percent, y = bs_percent, color = abs(bs_percent - hg_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "black") +
  theme(legend.position = "none") +
  labs(y = "Brontes", x = "Wells")


austen_vs_wells_no_stop <- all_words_percents_no_stop %>%
  select(word, ja_percent, hg_percent) %>%
  drop_na()

ggplot(austen_vs_wells_no_stop, aes(x = ja_percent, y = hg_percent, color = abs(hg_percent - ja_percent))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "black") +
  theme(legend.position = "none") +
  labs(y = "H.G. Wells", x = "Jane Austen")


