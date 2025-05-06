# Libraries-----
library(tidyverse)
library(tidytext)
library(topicmodels)
library(scales)
library(reshape2)

#gamma -> topics
#beta -> words in topics

#gets hist from downloads
setwd("C:/Users/dunnkyle/downloads")

hist_text <- read_delim("hist(2).txt", delim = "\t", col_names = TRUE)


# Assumes you have data frame ja_tidy from unit 3.
# It has one column for "gutenber_id" and one for "word".
# (You may also want to have the books labeled with their
# actual titles rather than their Gutenberg IDs.
# It does make identifying the "documents" easier in LDA.)

# Get word frequencies and remove stopwords.
hist_word_freq_no_stop <- hist_text %>% 
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  count(word,chapter) 

# Just in case there are NA cells, eliminate those.
hist_word_freq_no_stop <- na.omit(hist_word_freq_no_stop)

# For consistency with document-term matrix
# we'll rename headings (optional) 
names(hist_word_freq_no_stop) <- c("term","document","n")



# We've eliminated stopwords but could eliminate 
# low-frequency terms (numbers) too, for example, 
# so long as they play no role in creating topics.


# Cast tidy data frame into document term matrix.
hist_dtm <- hist_word_freq_no_stop %>%
  cast_dtm(document,term,n)

# LDA.  Choosing 10 topics just to see.  
# This will take a minute or two.
hist_lda <- LDA(hist_dtm,k=100,control = list(seed = 1234))

# The object hist_lda is a wrapper for lots of LDA outputs.
# We only care here about the two matrices beta and gamma.  
# Beta relates the words in the topics.
# Gamma relates the topics in the documents.

# Getting beta matrix from hist_lda.
hist_topics <- tidy(hist_lda,matrix="beta")

# Looking up the top 10 words in each topic.
hist_top_terms <- hist_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


# Plot topics as bar chart.  One small note:  we are
# treating the topic column as a factor here so that
# it works as a facet.
hist_top_terms %>%  
  ggplot(aes(term, beta, fill = factor(as.factor(topic)))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Can also look up individual topics, which can be useful
# when there are many and we want to focus on a few.
hist_top_terms %>%  
  filter(topic==7 | topic==11) %>%
  ggplot(aes(term, beta, fill = factor(as.factor(topic)))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()




# Getting gamma, which gives the topics in the documents.
hist_docs <- tidy(hist_lda,matrix="gamma")

# Plot this as a tile plot.
hist_docs %>%
  ggplot(aes(document, topic, fill = gamma)) +
  geom_tile(color="grey") +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  coord_flip()

##second paragraph instructions



#makes the matrix
hist_dtm <- hist_word_freq_no_stop %>%
  cast_dtm(document, term, n)





#Top 10 words per topic or chapter
hist_top_terms <- hist_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#plot bar graph top words by topic
split_topics <- split(hist_top_terms, ceiling(hist_top_terms$topic / 5))

for (i in seq_along(split_topics)) {
  p <- split_topics[[i]] %>%  
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    labs(title = paste("Top Terms in Topics", 
                       min(split_topics[[i]]$topic), 
                       "to", 
                       max(split_topics[[i]]$topic)))
  print(p)
  }



#Heatmaps for topics
hist_docs$document <- as.integer(hist_docs$document)
split_docs <- split(hist_docs, ceiling(hist_docs$document / 100))
for (i in seq_along(split_docs)) {
  p <- split_docs[[i]] %>%
    ggplot(aes(factor(document), factor(topic), fill = gamma)) +
    geom_tile(color = "grey") +
    scale_fill_gradient2(high = "red", label = percent_format()) +
    labs(title = paste("Topic Proportions in Chapters", 
                       min(split_docs[[i]]$document), 
                       "to", 
                       max(split_docs[[i]]$document)),
         x = "Chapter", y = "Topic")+ 
    coord_flip()
  print(p)
  }
