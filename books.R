library("recommenderlab")
library("caTools")
#importing the books dataset
book = read.csv(file.choose())
View(book)
dim(book)

#excluding the s.no. column from the dataset
book = book[2:6]

#changing the names of particular column
names(book)[1] <- "user_id"
names(book)[5] <- 'ratings'

str(book)

#calculating the number of each rating
table(book$ratings)

# barplot distribution for visualization
barplot(table(book$ratings))

#the datatype should be realRatingMatrix inorder to build recommendation engine
book_matrix <- as(book, 'realRatingMatrix')

dim(book_matrix)

# building fisrt a Popularity based model
book_model1 = Recommender(book_matrix, method = 'Popular')

#Predictions for two users 
recommended_items1 <- predict(book_model1, book_matrix[6:8], n=2)
as(recommended_items1, "list")


## Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering
#User Based Collaborative Filtering
book_recomm_model2 <- Recommender(book_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(book_recomm_model2, book_matrix[413:414], n=2)
as(recommended_items2, "list")

#this model gives the different output for the different users
