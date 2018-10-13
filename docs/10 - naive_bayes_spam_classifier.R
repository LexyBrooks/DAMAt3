#spam classification using Naive Bayes
#uses the spam.csv file in the DAM_BS4 folder

library(e1071)
library(tm)
library(wordcloud)
rm(list=ls())
#read dataset
sms_dataset <- read.csv(file= 'spam.csv',header = T, stringsAsFactors=TRUE, fileEncoding='latin1')
str(sms_dataset)

#class count
table(sms_dataset$label)

#Explore subsets using wordclouds

#ham wordcloud
ham <- subset(sms_dataset, label=="ham")
set.seed(42)
wordcloud(ham$sms, max.words = 50, 
          colors=brewer.pal(8, "Dark2"))
#note that wordcloud uses tm to preprocess text and compute term frequencies - hence the 
#warnings.

#spam wordcloud
spam <- subset(sms_dataset, label=="spam")
set.seed(42)
wordcloud(spam$sms, max.words = 50, 
          colors=brewer.pal(8, "Dark2"))

#use tm to generate features (= words)
#Create corpus
sms_corpus <- VCorpus(VectorSource(sms_dataset$sms))

#Preprocess data and generate dtm
sms_dtm <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

sms_dtm

## 75% of the sample size, use floor to round down to nearest integer
trainset_size <- floor(0.80 * nrow(sms_dtm))

# first step is to set a random seed to ensure we get the same result each time
#All random number generators use a seed 

set.seed(42) 

#get indices of observations to be assigned to training set...
#this is via randomly picking observations using the sample function
trainset_indices <- sample(seq_len(nrow(sms_dtm)), size = trainset_size)

#split messages into train and test sets
sms_dtm_train <- sms_dtm[trainset_indices,]
sms_dtm_test<- sms_dtm[-trainset_indices,]
#train and test labels
train_labels <- sms_dataset$label[trainset_indices]
test_labels <- sms_dataset$label[-trainset_indices]
#check that both sets have a representative distribution of classes (roughly in same 
#proportion as original)
table(train_labels)
table(test_labels)

#find words that appear 8 times or more in the corpus
freq_terms <- findFreqTerms(sms_dtm,8)

sms_dtm_train_freq_terms <- sms_dtm_train[,freq_terms]
sms_dtm_test_freq_terms <- sms_dtm_test[,freq_terms]
#view section of matrix
inspect(sms_dtm_train_freq_terms[30:40,100:130])

#convert frequencies to binary (0=term not present, 1=term is present)
counts_to_true_false <- function(x) {
  x <- ifelse(x > 0, "Y", "N")
}

#convert test and train sets
sms_dtm_train_final <- apply(sms_dtm_train_freq_terms, MARGIN = 2, counts_to_true_false) 
sms_dtm_test_final <- apply(sms_dtm_test_freq_terms, MARGIN = 2, counts_to_true_false) 

#build model using Naive Bayes algorithm
#laplace smoothing: assign a small non-zero conditional probability to terms that are 
#in test but not in train

spam_pred_model <- naiveBayes(sms_dtm_train_final, train_labels, laplace = 1)

#predict on test
spam_predict <- predict(spam_pred_model,sms_dtm_test_final)

#accuracy
mean(spam_predict==test_labels)

#confusion matrix
table(predict=spam_predict,true=test_labels)

