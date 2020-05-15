zoo = read.csv(file.choose())
View(zoo)
table(zoo$legs)
table(zoo$type)
dim(zoo)
zoo_train = zoo[1:70, 2:17]
zoo_test = zoo[71:101, 2:17]

zoo_train_label = zoo[1:70, 18]
zoo_test_label = zoo[71:101, 18]
class(zoo_test_label)
class(zoo_train_label)
zoo_test_label <- factor(zoo_test_label)
zoo_train_label <- factor(zoo_train_label)
summary(zoo_train_label)

#building the model

library(class)
zoo_test_pred <- knn(zoo_train,zoo_test,cl=zoo_train_label,k=1)
class(zoo_test_pred)

#K = 1, accuracy = 90%
#k = 3, accuracy = 74%
#k = 5, accuracy = 70%
#k = 7,9 accuracy = 64%
#--------------------------------------------------------------------------------------------

test_acc <- NULL
train_acc <- NULL

for (i in seq(3,200,2))
{
  train_zoo_pred <- knn(train=zoo_train,test=zoo_test,cl=zoo_train_label,k=i)
  train_acc <- c(train_acc,mean(train_zoo_pred==zoo_train_label))
  test_zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_label, k=i)
  test_acc <- c(test_acc,mean(test_zoo_pred==zoo_test_label))
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
CrossTable(x=zoo_test_label,y=zoo_test_pred,prop.chisq=FALSE)


library(caret)
library("e1071")
confusionMatrix(table(zoo_test_pred,zoo_test_label))
