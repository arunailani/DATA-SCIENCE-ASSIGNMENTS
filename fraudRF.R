fraud <- read.csv(file.choose())
dim(fraud)
View(fraud)
fraud$variety <- ifelse(fraud$Taxable.Income<=30000,"risky","good")
library('caret')
library('randomForest')
partition <- createDataPartition(fraud$variety,p=.7,list=F)
train_data <- fraud[partition,]
View(train_data)
prop.table(table(train_data$variety))
test_data <- fraud[-partition,]
prop.table(table(test_data$variety))
str(train_data)
train_data$variety <-  factor(train_data$variety)
test_data$variety <- factor(test_data$variety)

# Building a random forest model on training data 
fit.forest <- randomForest(train_data$variety~.,data=train_data, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree
# Training accuracy 
mean(train_data$variety==predict(fit.forest,train_data)) # 100% accuracy 

# Predicting test data 
pred_test <- predict(fit.forest,newdata=test_data)
mean(pred_test==test_data$variety)  #99.4% accuracy
library(gmodels)
# Cross table 
rf_perf<-CrossTable(test_data$variety, pred_test,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))
