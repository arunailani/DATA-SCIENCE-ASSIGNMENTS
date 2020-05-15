library("twitteR")
#install.packages("ROAuth")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
setwd("C:\\Users\\arun\\Desktop\\DATA SCIENCE FOLDER\\Data Mining & NLP")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('@WWERomanReigns', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)

write.csv(TweetsDF, "Tweets_RR_WWE.csv",row.names = F)

getwd()

# ------------------------------------------------------------------------------------
# Sentiment Analysis
Roman <- read.csv(file.choose(), header = TRUE)
str(Roman)  
View(Roman)
roman <- as.data.frame(Roman)
a <- as.character(roman$text)
View(a)
length(a)

library(tm)

a <- Corpus(VectorSource(a))
inspect(a[1])
inspect(a[966])

#Data Cleansing
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
a1 <- tm_map(a, content_transformer(removeURL))
a1 <- tm_map(a, tolower)
a1 <- tm_map(a1, removePunctuation)
a1 <- tm_map(a1,removeNumbers)
a1 <- tm_map(a1, removeWords, stopwords('english'))
a1 <- tm_map(a1, stripWhitespace)
inspect(a1[1])

tdm <- TermDocumentMatrix(a1)
dtm <- t(tdm)

tdm <- as.matrix(tdm)
tdm[200:210, 1:10]

s <- rowSums(tdm)
s_sub <- subset(s, s>=25)
s_sub

length(s_sub)

barplot(s_sub, las = 2, col = rainbow(41))

#removing term romanreigns from s_sub

a1 <- tm_map(a1, removeWords, 'wweromanreigns')
tdm <- TermDocumentMatrix(a1)

library(wordcloud)
wordcloud(words = names(s_sub), frequency = s_sub)

wordcloud(words = names(s_sub), freq = s_sub, random.order = F, colors = rainbow(50), scale=c(5,2), rot.per = 0.4)

s1 <- subset(s, s>10)
s1 <- data.frame(names(s1), s1)
colnames(s1) <- c('words', 'frequency')

windows()
library(wordcloud2)
wordcloud2(s1, size = 0.5, shape = 'circle')

wordcloud(words = s1$words, freq = s1$frequency, random.order = F, colors = rainbow(50), scale=c(5,2), rot.per = 0.4)
