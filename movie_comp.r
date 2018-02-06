library(twitteR)
library(ROAuth)
library(ggplot2)
library(curl)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
api_key <- "0wrXyqjkuzfP98pLvXMoWZnuz" # From dev.twitter.com
api_secret <- "myyUcaH2vBMqeiLzuM2wdtmz8Q3gBnu7ZDHPA0L3rFgHIWpqtp" # From dev.twitter.com
token <- "724636352709464065-K6HCJYz6Ib1JZccPCsHhMQkIHhdPi9f" # From dev.twitter.com
token_secret <- "hwntLEzL4c0E0n6O0hfnjic2Llm1PXkDW8phYE8JZtth3"


setup_twitter_oauth(api_key,api_secret,token,token_secret)


padmaavat.list <- searchTwitter('#padmaavat',n=1000 ) #, cainfo="cacert.pem")
padmaavat.df=twListToDF(padmaavat.list)

padman.list <-searchTwitter('#padman',n=1000) #,cainfo="cacert.pem")
padman.df=twListToDF(padman.list)

head(padman.list)
head(padmaavat.list)

#some more libraries
library(plyr)
library(stringr)

#generate function

score.sentiment=function(sentences,pos.words,neg.words , .progress ='none')
  {
  
  require(plyr)
  require(stringr)
  
  good.smiley <- c(":)")
  bad.smiley <- c(":(",";)",":'",":P")
  scores = laply( sentences , function( sentence , pos.words,neg.words)
  {
  #cleaning
  sentence = ''
  sentence=gsub( ":)","awsum",sentence )
  sentence=gsub('[[:punct:]]','', sentence)
  sentence=gsub('[[:cntrl:]]','', sentence)
  sentence=gsub('\\d+','', sentence)
  #convert lower case
  sentence=tolower(sentence)
  #split into words
  word.list=strsplit(sentence,'\\s+')
  #remove hierarchy
  
  words = unlist(word.list)
  
  #compare words to dictionary of pos and neg words
  pos.matches=match(words,pos.words)
  neg.matches=match(words,neg.words)
  
  #match() returns true or NA , just to know position
  
  pos.matches=!is.na(pos.matches)
  neg.matches=!is.na(neg.matches)
  
  # add all true and falses
  
  score = sum(pos.matches)-sum(neg.matches)
  
  return(score)
  
  
}, pos.words, neg.words)

scores.df= data.frame(score=scores , text = sentences)
return(scores.df)
}

#load sentiment word lists

hu.liu.pos = scan('D:/basics/positive-words.txt',what='character',comment.char = ';')
hu.liu.neg = scan('D:/basics/negative-words.txt',what='character',comment.char = ';')

#Add words to list

pos.words = c(hu.liu.pos,'royal','awsum','worthy','good','must watch')
neg.words = c(hu.liu.neg,'wtf','bad','pathetic','useless','waste')
movie = c("vs.","vs","versus")

#convert text to factor
padman.df$text <-as.factor(padman.df$text)
padmaavat.df$text <-as.factor(padmaavat.df$text)

#giving scores to each tweet
padman.scores = score.sentiment(padman.df$text,pos.words,neg.words, .progress='text')
padmaavat.scores = score.sentiment(padmaavat.df$text,pos.words,neg.words, .progress='text')

padman.scores$movie = 'padman'
padmaavat.scores$movie = 'padmaavat'

# check what made them negative tweets

padman.scores.2 = subset(padman.scores,padman.scores$score < 1)
padmaavat.scores.2 = subset(padmaavat.scores,padmaavat.scores$score < 1)
head(padman.scores.2)
head(padmaavat.scores.2)

# final output
hist(padman.scores$score)
hist(padmaavat.scores$score)

table(padman.scores$score)
table(padmaavat.scores$score)


all.scores = rbind(padman.scores,padmaavat.scores)
head(all.scores)

#SUMMARIZATION
table(all.scores$score,all.scores$movie)

ggplot(data=all.scores) +  #ggplot works on data frames
  geom_histogram(mapping=aes(x=score,fill=movie) , binwidth=1) + 
  facet_grid(movie~.) + #make separate plot for each hastag
  theme_bw()+ scale_fill_brewer() # plain display
