# Necessary packages
library(tidyverse)
library(tidytext)
library(readr)

# Load "Apples Never Fall" review data
# (Assumes ANFReviews3.csv is in working directory)
ANFReviews <- read_csv("ANFReviews3.csv")

# Tidy data
ANF_tidy <- ANFReviews %>% unnest_tokens(word, text)

#bing to get sentiment
ANF_sent <- ANF_tidy %>% 
  inner_join(get_sentiments("bing"))

#finds sum of positive and negative sentiment by source and stars
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

#Scatterplot with sentiment v metacritic score
ANF_sent_cnt %>%
  ggplot(aes(x = diff, y = stars, color = abs(diff - stars))) +
  geom_abline(color = "gray40", lty = 2) +  
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +  
  geom_text(aes(label = source), check_overlap = TRUE, vjust = 1.5) +  
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_gradient(limits = c(0, max(abs(ANF_sent_cnt$diff - ANF_sent_cnt$stars))), low = "darkslategray4", high = "black") + 
  theme(legend.position = "none") +  
  labs(y = "the metacritic Score", x = "bing sentiment score", title = "sentiment v metacritic score")





