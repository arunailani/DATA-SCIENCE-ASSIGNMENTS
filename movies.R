library(arules)
library(arulesViz)
#importing the dataset
movie <- read.csv(file.choose())
View(movie)

#the dataset contains the same data in two fromats
# 1. from column 1-5 in the transactions format
# 2. from column 6-15 in binary format
# so now using the binary format data and storing the data frame in new variable
movies <- movie[,6:15]
View(movies)

# Each row represents one transaction
# After converting the binary format of data frame from matrix to transactions
# Perform apriori algorithm by changing the values of support and confidence
movie_trans<-as(as.matrix(movies),"transactions")

inspect(movie_trans[1:10])

#building model with different values of support and confidence
# Apriori algorithm 
rules<-apriori(movie_trans,parameter = list(support=0.002,confidence=0.7))
inspect(rules[1:5])
plot(rules)
head(quality(rules))

rules1<-apriori(movie_trans,parameter = list(support=0.002,confidence=0.7, minlen = 2))
inspect(rules1[1:5])
plot(rules1)
head(quality(rules1))

rules2<-apriori(movie_trans,parameter = list(support=0.006,confidence=0.7, minlen = 3))
inspect(rules2[1:5])
plot(rules2)
head(quality(rules2))