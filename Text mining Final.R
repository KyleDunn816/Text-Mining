#libraries-----
library(readxl)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(quanteda)
library(quanteda.textmodels)
library(topicmodels)
library(readr)
library(scales)
library(tidyverse)
library(reshape2)

#part 2 - Jitter Plot
#=========================================================================
#reads reviews put the in documents 
#used more then once throughout project
pos_reviews <- read_csv("pos_reviews.csv")
neg_reviews <- read_csv("neg_reviews.csv")


#binds to one data frame
all_reviews <- bind_rows(pos_reviews, neg_reviews)

#gets word count
word_counts <- all_reviews %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(author, word, sort = TRUE) %>%
  pivot_wider(names_from = author, values_from = n, values_fill = 0) %>%
  rename(n_riordan = riordan, n_rowling = rowling)

#transform
word_freq_comparison <- word_counts %>%
  mutate(log_riordan = log(n_riordan + 1),
         log_rowling = log(n_rowling + 1))

#jitter plot!
ggplot(word_freq_comparison, aes(x = log_rowling, y = log_riordan, label = word)) +
  geom_jitter(alpha = 0.5, color = "orange") +
  geom_text(aes(label = ifelse(n_riordan > 50 | n_rowling > 50, word, "")), 
            check_overlap = TRUE, hjust = 1.1, size = 3) +
  labs(title = "Word Frequencies: Riordan vs Rowling",
       x = "positive reviews", 
       y = "negative reviews")


#part 3 - Bing 
#=======================================================================
#reads from pos_reviews and neg_reviews up above
ANF_Reviews <- bind_rows(pos_reviews, neg_reviews)

#source and stars
pos_reviews <- pos_reviews %>%
  mutate(source = paste0("pos", row_number()), stars = 8)

neg_reviews <- neg_reviews %>%
  mutate(source = paste0("neg", row_number()), stars = 3)

# Tidy data
ANF_tidy <- ANF_Reviews %>% unnest_tokens(word, text)

#bing to get sentiment
ANF_sent <- ANF_tidy %>% 
  inner_join(get_sentiments("bing"))

#finds sum of positive and negative 
ANF_sent_cnt <- ANF_sent %>%
  count(source, stars, sentiment)

#make two columns: positive and negative
ANF_sent_cnt <- ANF_sent_cnt %>%
  spread(sentiment, n, fill = 0)

#makes column for negative and positive
ANF_sent_cnt <- ANF_sent_cnt %>%
  mutate(diff = positive - negative)

# Scatterplot of stars (metacritic score) vs sentiment scores
ANF_sent_cnt %>%
  ggplot(aes(diff, stars)) +
  geom_point() +
  labs(x = "Bing Sentiment Score", y = "metacritic score", title = "sentiment v metacritic Score")

#Scatterplot
ANF_sent_cnt %>%
  ggplot(aes(x = diff, y = stars, color = abs(diff - stars))) +
  geom_abline(color = "gray40", lty = 2) +  
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +  
  geom_text(aes(label = source), check_overlap = TRUE, vjust = 1.5) +  
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_gradient(limits = c(0, max(abs(ANF_sent_cnt$diff - ANF_sent_cnt$stars))), low = "darkslategray4", high = "black") + 
  theme(legend.position = "none") +  
  labs(y = "positive", x = "Negative", title = "Part 3 Bing")

#part 4 - NCR Sentiment Lexicon
#====================================================================================

#review loader same elsewhere
pos_reviews <- read_csv("pos_reviews.csv")
neg_reviews <- read_csv("neg_reviews.csv")

#colmun for negative and positive
pos_reviews <- pos_reviews %>% mutate(id = row_number(), val = "positive")
neg_reviews <- neg_reviews %>% mutate(id = row_number(), val = "negative")

#combine neg and pos
reviews <- bind_rows(pos_reviews, neg_reviews) %>%
  select(id, val, text)

#the NRC lexicon
nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

#tokenize reviews and joy words
joy_reviews <- reviews %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_joy, by = "word") %>%
  count(id, val, word, sort = TRUE)

#joy sentiment
joy_summary <- joy_reviews %>%
  group_by(val) %>%
  summarise(total_joy_words = sum(n),
            unique_reviews = n_distinct(id),
            avg_joy_per_review = total_joy_words / unique_reviews)

print(joy_summary)

#Plot joy table
ggplot(joy_summary, aes(x = val, y = avg_joy_per_review, fill = val)) +
  geom_col(show.legend = FALSE) +
  labs(title = "joy Word Frequency per Review",
       x = "Review Type", y = "Joy Words") +
  scale_fill_manual(values = c("negative" = "red", "positive" = "blue")) +
  theme_minimal()



#PART 5 - Topic Model 
#=================================================================================

#reads files same as elsewhere
pos_reviews <- read_csv("pos_reviews.csv", col_names = c("val", "text"))
neg_reviews <- read_csv("neg_reviews.csv", col_names = c("val", "text"))

#positive and negative combiner
reviews <- bind_rows(pos_reviews, neg_reviews)
data("stop_words")

reviews_tidy <- reviews %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "^[a-z]+$"))

#create dtm
dtm <- reviews_tidy %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

#model with 20 topics
#this takes an extremely long time to load, I apologize
lda_model <- LDA(dtm, k = 20, control = list(seed = 1234))

#top terms for each 20 topics
top_terms <- tidy(lda_model, matrix = "beta") %>%
  arrange(topic, desc(beta)) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup()

#printer of top 10 terms for all 20 topics
print(top_terms)

#ggplot of the 20 topics
top_terms %>%
  ggplot(aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Terms in Each Topic",
       x = "Term",
       y = "Beta")


#part 6 - 75%/25% text training
#==================================================================================
#label columns
pos_reviews$label <- "pos"
neg_reviews$label <- "neg"

#pos-75% train, neg-25% test
pos_train <- pos_reviews[1:750, ]
pos_test <- pos_reviews[751:1000, ]
neg_train <- neg_reviews[1:750, ]
neg_test <- neg_reviews[751:1000, ]

#combine train and test sets
review_train <- bind_rows(pos_train, neg_train)
review_test <- bind_rows(pos_test, neg_test)

#the corpus maker
review_corpus <- corpus(bind_rows(review_train, review_test))

#dtm
review_dfm <- dfm(tokens(review_corpus), tolower = TRUE)

# Split dfm the same way
review_dfm_train <- review_dfm[1:nrow(review_train), ]
review_dfm_test <- review_dfm[(nrow(review_train) + 1):nrow(review_dfm), ]

#trainer
review_classifier <- textmodel_nb(review_dfm_train, review_train$label)

#Predictor
review_pred <- predict(review_classifier, newdata = review_dfm_test)

#matrix
conf_matrix <- table(review_pred, review_test$label)
print(conf_matrix)