txt <- read.csv(file.choose(), header = TRUE)

str(txt)
View(txt)
class(txt)
dim(txt)
txt <- as.data.frame(txt)
#x <- as.character(txt)
x <- as.character(txt$review)
View(x)
length(x)

# Corpus
#install.packages("tm")
library(tm)

x <- Corpus(VectorSource(x))

inspect(x[1])
inspect(x[300])

#Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])

inspect(x1[300])
x1 <- tm_map(x1, removeNumbers)
inspect(x1[300])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

# Remove URL's from corpus
# removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
# x1 <- tm_map(x1, content_transformer(removeURL))
# inspect(x1[1])

#striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

#Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm
dtm <- t(tdm)

tdm <- as.matrix(tdm)
tdm[100:109, 1:10]

tdm[90:100, 1:20]
# View(tdm)

inspect(x[3])


# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 25)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Term mcdonalds repeats in all most all documents
x1 <- tm_map(x1, removeWords, 'mcdonalds')
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109, 1:20]

tdm <- as.matrix(tdm)
tdm[90:100, 1:20]

# Word cloud
install.packages("wordcloud")
library(wordcloud)
w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
w_sub1
#set.seed(222)
wordcloud(words = names(w_sub), freq = w_sub)
wordcloud(words = names(w_sub1), freq = w_sub1)

wordcloud(words = names(w_sub), freq = w_sub, random.order = F, colors = rainbow(50), scale=c(5,2), rot.per = 0.4)
?wordcloud

# wordcloud2 - shapes for word cloud
w

w_small <- subset(w, w >= 10)
w_small

barplot(w_small, las=2, col = rainbow(30))

installed.packages("wordcloud2")
library(wordcloud2)

w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')
#windows()
View(w1)
wordcloud2(w1, size=0.5, shape='circle')
?wordcloud2

wordcloud2(w1, size=0.5, shape = 'triangle')

####


# Letter cloud

####letterCloud(w1, word = "ABC", wordSize = 0.5)
##?letterCloud

#windows()
#letterCloud(w1, word = "w")


#### emotion mining

install.packages("syuzhet")
library("syuzhet")
library(lubridate)
library(ggplot2)
library(scales)
library(dbplyr)
library(reshape2)

txt = readLines(file.choose())
x <- iconv(txt, "UTF-8")

s <- get_nrc_sentiment(x)
head(s)

x[4]
get_nrc_sentiment('unfriendly')

# Bar plot for emotion mining

barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')
