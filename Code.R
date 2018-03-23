library(twitteR)
api_key<- '######'
api_secret_key <- '######'
access_token <- '######'
access_token_secret <- '######'

setup_twitter_oauth(api_key,api_secret_key,access_token,access_token_secret)
library(purrr)
library(dplyr)
library(plyr)
library(stringr)
pos.words = scan('E:/positive_word_list.txt',what = 'character',comment.char = ';')
neg.words = scan('E:/negative_word_list.txt',what = 'character',comment.char = ';')

score.sentiment <- function(sentences,pos.words,neg.words,.progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences,function(sentence,pos.words,neg.words){
    sentence <- gsub("[[:punct:]]", "",sentence)
    sentence <- gsub("[[:cntrl:]]", "",sentence)
    sentence <- gsub("\\d+ ", "",sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence,'\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words,pos.words)
    neg.matches <- match(words,neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  },pos.words,neg.words,.progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}
tweets1<- userTimeline("@WBA",n=500)
tweets2<- userTimeline("@ChelseaFC",n=500)
tweet.df <- tbl_df(map_df(tweets1,as.data.frame) )
tweet2.df <- tbl_df(map_df(tweets2,as.data.frame) )
View(tweet.df)
View(tweet2.df)


cscore <- score.sentiment(tweet.df$text,pos.words,neg.words,.progress = 'text')
View(cscore$score[12])
wbascore <- score.sentiment(tweet2.df$text,pos.words,neg.words,.progress = 'text')

for(i in 1:302){
  
  if(cscore$score[i]<0)
  {
    cscore$score[i] =0
  }
}
View(cscore)
View(cscore$score[12])
for(i in 1:302){
  
  if(cscore$score[i]>1)
  {
    cscore$score[i] =1
  }
}
View(cscore$score[2])
hist(cscore$score)
hist(wbascore$score)






