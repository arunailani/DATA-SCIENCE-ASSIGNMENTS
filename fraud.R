fraud <- read.csv(file.choose())
dim(fraud)
View(fraud)
fraud$variety <- ifelse(fraud$Taxable.Income<=30000,"risky","good")
library('caret')
library('C50')
partition <- createDataPartition(fraud$variety,p=.7,list=F)
train_data <- fraud[partition,]
View(train_data)
prop.table(table(train_data$variety))
test_data <- fraud[-partition,]
prop.table(table(test_data$variety))
str(train_data)
train_data$variety <-  factor(train_data$variety)
test_data$variety <- factor(test_data$variety)
#model building
m <- C5.0(train_data$variety~., data = train_data, trails = 40)
summary(m)
plot(m)
pred1 <- predict.C5.0(m,test_data[,-7])
table(pred1)
mean(pred1==test_data$variety)

m2 <- C5.0(train_data[,-c(7)],train_data$variety)
plot(m2)


library(tree)
m3 <- tree(train_data$variety~.,data = train_data)
plot(m3)
text(m3,pretty = 0)
