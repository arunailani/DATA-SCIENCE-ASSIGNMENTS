bank_data = read.csv(file.choose())
View(bank_data)
attach(bank_data)

str(bank_data)

#creation of dummy variables using caret package for the model building
library(caret)
dmy = dummyVars("~ .", data = bank_data)
bank_dmy = data.frame(predict(dmy, newdata = bank_data))
bank_dmy
str(bank_dmy)

#model building
bank_model1 = glm(y~age+job+marital+education+default+balance+housing+loan+contact+day+month+duration+campaign+pdays+previous+poutcome, data = bank_data, family = "binomial")
summary(bank_model1)

bank_model4 = glm(y~job+marital+education+balance+housing+loan+contact+day+month+duration+campaign+previous, data = bank_data, family = "binomial")
summary(bank_model4)

exp(coef(bank_model4))
table(bank_data$y)

#confusion matrix table
prob = predict(bank_model4,type = "response")
prob
table(prob>0.5)
confusion  = table(prob>0.5,bank_data$y)
confusion

#model accuracy
accuracy = sum(diag(confusion)/sum(confusion))
accuracy
error = 1-accuracy
error

#ROC curve
library(ROCR)
rocrpred = prediction(prob,bank_data$y)
rocrperf = performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
