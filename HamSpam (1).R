# The usual libraries------
library(tidyverse)
library(tidytext)
# Also requires the following quanteda libraries,
# You will need to install as usual.
library(quanteda)
library(quanteda.textmodels)

# ham/spam category in "label" and messages in "text" column.
names(emails) <- c("label","text")


# Check how many ham, how many spam.
emails %>% group_by(label) %>% summarise(length(label))


# Create corpus object for quanteda.
emails_corpus <- corpus(emails)
# If you want to view it (View doesn't work with corpus objects)
summary(emails_corpus)


# Create dfm matrix for analysis
emails_dfm <- dfm(tokens(emails_corpus), tolower = TRUE)
# More options for improvements, possibly useful later:
# hamspam_dfm <- dfm_trim(hamspam_dfm, min_count = 5, min_docfreq = 3)
#    would omit terms occurring less than 5 times or 
#    in less than 3 documents
# hamspam_dfm <- dfm_weight(hamspam_dfm, type = "tfidf")
#    would use tf-idf as weights instead of frequencies


# Get 75-25 train-test split.  Since 75% of our data
# is 4180, and data randomized, split at 4180 should do it.
emails_dfm_train <- emails_dfm[1:4180,]
emails_dfm_test <- emails_dfm[4181:nrow(emails_dfm),]

# Need to split original as well for ham/spam labels.
emails_train <- emails[1:4180,]
emails_test <- emails[4181:nrow(emails),]

# Check split to be sure
table(emails_train$label)
# Checks out, ham/spam split very close to original. 


# Ready to produce classifier.
emails_classifier <- textmodel_nb(emails_dfm_train, emails_train$label)

# How well does classifier do?
# Let's get its predictions for test set.
emails_pred <- predict(emails_classifier,newdata = emails_dfm_test)
# Now check those predictions against actual labels.
table(emails_pred, emails_test$label)
# Pretty darn good.  That's 97% accuracy!