#importing data
train_data_df <- read.csv("E:/train.csv")
test_data_df <- read.csv("E:/test.csv")

#converting label to factors
train_data_df$label <- factor(train_data_df$label)
str(train_data_df$label)

View(test_data_df)
View(train_data_df)

#cleaning the training data tweets
train_data_df$tweet <- gsub("[[:punct:]]", "",train_data_df$tweet)
train_data_df$tweet <- gsub("[[:cntrl:]]", "",train_data_df$tweet)
train_data_df$tweet <- gsub("\\d+ ", "",train_data_df$tweet)
train_data_df$tweet <- tolower(train_data_df$tweet)

#cleaning the testing data tweets
test_data_df$tweet <- gsub("[[:punct:]]", "",test_data_df$tweet)
test_data_df$tweet <- gsub("[[:cntrl:]]", "",test_data_df$tweet)
test_data_df$tweet <- gsub("\\d+ ", "",test_data_df$tweet)
test_data_df$tweet <- tolower(test_data_df$tweet)

#subsetting negative tweet from training data set
negative_tweet <- subset(train_data_df,label=="1")
View(negative_tweet)


library(tm)

#creating text corpus
train_corpus <- Corpus(VectorSource(train_data_df$tweet))
test_corpus <- Corpus(VectorSource(test_data_df$tweet))
train1_corpus <- Corpus(VectorSource(negative_tweet$tweet))

#creating word cloud to remove words
library(wordcloud)

wordcloud(train1_corpus,min.freq = 30,colors = brewer.pal(8,"Set2"),random.order = F)

#cleaning training(negative comment) corpus
train1_corpus <- tm_map(train1_corpus, content_transformer(tolower))
train1_corpus <- tm_map(train1_corpus, removePunctuation)
train1_corpus <- tm_map(train1_corpus, removeWords, c("user","and","the","for","amp","you"))
train1_corpus <- tm_map(train1_corpus, removeWords, stopwords("english"))
train1_corpus <- tm_map(train1_corpus, stripWhitespace)
train1_corpus <- tm_map(train1_corpus, stemDocument)

#cleaning training corpus
train_corpus <- tm_map(train_corpus, content_transformer(tolower))
train_corpus <- tm_map(train_corpus, removePunctuation)
train_corpus <- tm_map(train_corpus, removeWords, stopwords)
train_corpus <- tm_map(train_corpus, stripWhitespace)
train_corpus <- tm_map(train_corpus, stemDocument)

#cleaning testing corpus
test_corpus <- tm_map(test_corpus, content_transformer(tolower))
test_corpus <- tm_map(test_corpus, removePunctuation)
test_corpus <- tm_map(test_corpus, removeWords, stopwords)
test_corpus <- tm_map(test_corpus, stripWhitespace)
test_corpus <- tm_map(test_corpus, stemDocument)

#creating document term matrix for traing and testing corpus
train1_dtm <- DocumentTermMatrix(train1_corpus)
train_dtm <- DocumentTermMatrix(train_corpus)
test_dtm <- DocumentTermMatrix(test_corpus)


#finding frequent words
frequent_words <- findFreqTerms(train1_dtm,7)
length(frequent_words)
frequent_words[1:13]


freq_words_train <- train_dtm[,frequent_words]
freq_words_test <- test_dtm[,frequent_words]
str(freq_words_test)

# creating a function to check whether frequent word is present or not
yes_or_no <- function(x){
  y<-ifelse(x > 0,1,0)
  y<- factor(y,levels = c(0,1),labels = c("NO","YES"))
  y
}

train <- apply(freq_words_train,2,yes_or_no)
test <- apply(freq_words_test,2,yes_or_no)

#creating a data frame of training labels
train_labels <- train_data_df$label
str(train_labels)
length(train_labels)

#applying naiveBayes classifier
library(e1071)

sms_classifier <- naiveBayes(train,train_labels,laplace = 1)

sms_test_pred <- predict(sms_classifier,newdata=test)

#output
sms_test_pred
length(sms_test_pred)

#adding label column to test data set
test_data_df$label <- sms_test_pred
View(test_data_df)

#saving output file in local disk
write.csv(sms_test_pred,file = "E:/test_predictions.csv")




