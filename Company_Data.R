company <- read.csv(file.choose())
dim(company)
View(company)
company$variety <- ifelse(company$Sales>7,'good','bad')
library('caret')
library('C50')
partition2 <- createDataPartition(company$variety,p=0.7,list=F)
train_data2 <- company[partition2,]
test_data2 <- company[-partition2,]
prop.table(table(train_data2$variety))
prop.table(table(test_data2$variety))
dim(train_data2)
dim(test_data2)
train_data2$variety <- factor(train_data2$variety)
test_data2$variety <- factor(test_data2$variety)
#model building
mod <- C5.0(train_data2$variety~., data = train_data2, trails = 40)
plot(mod)
summary(mod)
pred1 <- predict.C5.0(mod,test_data2)
table(pred1)
mean(pred1==test_data2$variety)


library(tree)
mod2 <- tree(train_data2$variety~.,data = train_data2)
plot(mod2)
text(mod2,pretty = 0)
