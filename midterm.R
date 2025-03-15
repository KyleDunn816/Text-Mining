library(dplyr)
library(tidytext)
library(readr)
library(ggplot2)
library(stylo)
#|
#found on google makes this stuff work
getwd()
setwd("C:/Users/dunnkyle/Documents")
#reads the data
hp_data <- read_csv("HarryPotterSeries.txt")

#PART 1
word_freq <- hp_data %>%
  count(word, sort = TRUE)
#top10 frequent word
head(word_freq, 10) 

data("stop_words")  
word_freq_no_stop <- hp_data %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

#top10 without stopwords
head(word_freq_no_stop, 10)  

#PART 2
#word frequency for each book no stop words
word_freq_no_stop <- hp_data %>%
  anti_join(stop_words, by = "word") %>%
  count(book, word, sort = TRUE)

#top ten words per book
top_words <- word_freq_no_stop %>%
  group_by(book) %>%
  slice_max(n, n = 10) %>%
  ungroup()

#the bar plot itself
ggplot(top_words, aes(x = reorder_within(word, n, book), y = n, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ book, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "10 Most Frequent Words in the Harry Potter Books",
       x = "words",
       y = "freq") 


#PART 4
#RIO_data the riordan book data
#reads all five books riordan
RIO_data <- bind_rows(
  read_csv("Riordan_PJ1.txt", col_names = "text"),
  read_csv("Riordan_PJ2.txt", col_names = "text"),
  read_csv("Riordan_PJ3.txt", col_names = "text"),
  read_csv("Riordan_PJ4.txt", col_names = "text"),
  read_csv("Riordan_PJ5.txt", col_names = "text"))

#tokenize tokenize tokenize!
RIO_data <- RIO_data %>%
  unnest_tokens(word, text) 

#count word frequency
word_freq <- RIO_data %>%
  count(word, sort = TRUE)

#top 10 words
head(word_freq, 10)

#remove stopwords
word_freq_no_stop <- RIO_data %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

#prints without stopwords
head(word_freq_no_stop, 10)


#PART 5

#Merge data
word_freq_comparison <- full_join(word_freq_riordan, word_freq_rowling, by = "word") %>%
  replace_na(list(n_riordan = 0, n_rowling = 0))

#log transformation
word_freq_comparison <- word_freq_comparison %>%
  mutate(log_riordan = log(n_riordan + 1),  # Adding 1 to avoid log(0)
         log_rowling = log(n_rowling + 1))  # Adding 1 to avoid log(0)

#scatter plot
ggplot(word_freq_comparison, aes(x = log_rowling, y = log_riordan, label = word)) +
  geom_jitter(alpha = 0.5, color = "orange") +
  geom_text(aes(label = ifelse(n_riordan > 50 | n_rowling > 50, word, "")), 
            check_overlap = TRUE, hjust = 1.1, size = 3) +
  labs(title = "Word Frequencies: Riordan",
       x = "Log-transformed Frequency ", 
       y = "Log-transformed Frequency ")


#PART 6
#ttr word uniqueness
#hapax word count

#Harry Potter stopwords removed
word_freq_hp <- hp_data %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

#total word count in harry potter
total_words_hp <- sum(word_freq_hp$n)

#harry potter ttr
ttr_hp <- nrow(word_freq_hp) / total_words_hp

#for words that appear only once or hapax
hapax_hp <- sum(word_freq_hp$n == 1)

#print ttr and hapax 
print(paste("Harry Potter TTR: ", ttr_hp))
print(paste("Harry Potter Hapax: ", hapax_hp))

#total word count in riordan
total_words_riordan <- sum(word_freq_riordan$n)

#ttr for riordan 
ttr_riordan <- nrow(word_freq_riordan) / total_words_riordan

#for the words that appear only once
hapax_riordan <- sum(word_freq_riordan$n == 1)

#print ttr & hapax 
print(paste("Riordan TTR: ", ttr_riordan))
print(paste("Riordan Hapax: ", hapax_riordan))




#PART 7
#Galbraith_Cuckoo.txt 
#Excerpt of “A Cuckoo’s Calling”, an adult crime novel by Robert Galbraith.
#By JK Rowling
stylo()
stylo.network()
#Rowling_Leopard.txt  
#Excerpt of “Harry Potter and Leopard-Walks-Up-To-Dragon”.
#By JK Rowling


