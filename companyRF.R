company <- read.csv(file.choose())
dim(company)
View(company)
company$variety <- ifelse(company$Sales>7,'good','bad')
library('caret')
library('randomForest')
partition2 <- createDataPartition(company$variety,p=0.7,list=F)
train_data2 <- company[partition2,]
test_data2 <- company[-partition2,]
prop.table(table(train_data2$variety))
prop.table(table(test_data2$variety))
dim(train_data2)
dim(test_data2)
train_data2$variety <- factor(train_data2$variety)
test_data2$variety <- factor(test_data2$variety)

# Building a random forest model on training data 
fit.forest <- randomForest(train_data2$variety~.,data=train_data2, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree
# Training accuracy 
mean(train_data2$variety==predict(fit.forest,train_data2)) # 100% accuracy 

# Predicting test data 
pred_test <- predict(fit.forest,newdata=test_data2)
mean(pred_test==test_data2$variety)  #99.1% accuracy
library(gmodels)
# Cross table 
rf_perf<-CrossTable(test_data2$variety, pred_test,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))
