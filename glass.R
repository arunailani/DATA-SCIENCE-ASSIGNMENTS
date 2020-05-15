glass = read.csv(file.choose())
View(glass)
dim(glass)
str(glass)

# Here we need to randomly sample the data in train and test to avoid biasing
#glass_train <- glass[1:113, 1:9]
#glass_test <- glass[114:214, 1:9]

dt = sort(sample(nrow(glass), nrow(glass)*.7))
train <- glass[dt,]
test <- glass[-dt,]

View(train)
View(test)
dim(train)
dim(test)

glass_train <- train[,1:9]
glass_test <- test[,1:9]
glass_train_label <- train[,10]
glass_test_label <- test[,10]

glass_train_label <- factor(glass_train_label)
glass_test_label <- factor(glass_test_label)

library(class)
glass_test_pred <- knn(glass_train, glass_test, cl=glass_train_label, k=21)

#k=1, accuracy = 67%
#k=3, accuracy = 61%
#k=5, accuracy = 64%

#--------------------------------------------------------------------------------------------

test_acc <- NULL
train_acc <- NULL

for (i in seq(3,200,2))
{
  train_glass_pred <- knn(train=glass_train,test=glass_test,cl=glass_train_label,k=i)
  train_acc <- c(train_acc,mean(train_glass_pred==glass_train_label))
  test_glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_label, k=i)
  test_acc <- c(test_acc,mean(test_glass_pred==glass_test_label))
}

# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,200,2)))
# Plotting 2 different graphs on same co-ordinate axis
#install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))


#--------------------------------------------------------------------------------------------


#evaluating model performance
library(gmodels)
CrossTable(x=glass_test_label,y=glass_test_pred,prop.chisq=FALSE)


library(caret)
library("e1071")
confusionMatrix(table(glass_test_pred,glass_test_label))
