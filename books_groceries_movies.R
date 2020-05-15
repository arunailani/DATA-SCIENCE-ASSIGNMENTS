library(arules)
library(arulesViz)
book<-read.csv(file.choose())
View(book)
class(book)
book_trans<-as(as.matrix(book),"transactions")
inspect(book_trans[1:100])
# If we inspect book_trans
# we should get transactions of items i.e.
# As we have 2000 rows ..so we should get 2000 transactions 
# Each row represents one transaction
# After converting the binary format of data frame from matrix to transactions
# Perform apriori algorithm by changing the values of support and confidence

rules<-apriori(book_trans,parameter = list(support=0.002,confidence=0.7))
inspect(rules[1:5])
plot(rules)
head(quality(rules))

rules1<-apriori(book_trans,parameter = list(support=0.002,confidence=0.7, minlen = 4))
inspect(rules1[1:5])
plot(rules1)
head(quality(rules1))

rules2<-apriori(book_trans,parameter = list(support=0.006,confidence=0.7, minlen = 4))
inspect(rules2[1:5])
plot(rules2)
head(quality(rules2))
# Whenever we have binary kind of data .....e this for forming 
# Association rules and changing the values of support,confidence, and minlen 
# to get different rules 


# Whenever we have data containing item names, then load that data using 
# read.transactions(file="path",format="basket",sep=",")
# use this to form association rules 

###################################################################################
#the groceries dataset is present in the form of transactions, so using the read.transactions
#to read the data
groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)

# Perform apriori algorithm by changing the values of support and confidence to get different rules
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
inspect(groceries_rules[1:10])
plot(groceries_rules)
head(quality(groceries_rules))

groceries_rules1<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.07,minlen=4))
inspect(groceries_rules1[1:10])
plot(groceries_rules1)
head(quality(groceries_rules1))

groceries_rules<-apriori(groceries,parameter = list(support = 0.003,confidence = 0.06,minlen=5))
inspect(groceries_rules2[1:10])
plot(groceries_rules2)
head(quality(groceries_rules2))

#########################################################################################################
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
