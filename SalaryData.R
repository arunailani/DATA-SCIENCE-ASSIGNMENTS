salary_train <- read.csv(file.choose())
salary_test <- read.csv(file.choose())

View(salary_train)
View(salary_test)

# checking that the proportion of salary distribution data is similar in train and test data
prop.table(table(salary_train$Salary))
#0.7510693 0.2489307 

prop.table(table(salary_test$Salary))
#0.7543161 0.2456839 

#checking the total number of na values in train and test data
sum(is.na(salary_train))
# 0 na values

sum(is.na(salary_test))
# 0 na values

# barplot representations for salary distributions for the train and test data
barplot(table(salary_train$Salary))
barplot(table(salary_test$Salary))

dim(salary_train)
dim(salary_test)

library(kernlab)

# kernel = vanilladot

salary_vanilladot <- ksvm(salary_train$Salary~., data = salary_train, kernel = "vanilladot")
pred_vanilladot <- predict(salary_vanilladot, newdata = salary_test)
mean(pred_vanilladot==salary_test$Salary)#84.6%

# kernel = rbfdot

sal_mod_rbfdot <- ksvm(salary_train$Salary~. , data = salary_train, kernel = "rbfdot")
sal_pred_rbfdot <- predict(sal_mod_rbfdot, newdata = salary_test)
mean(sal_pred_rbfdot==salary_test$Salary) #85.4%

# kernel = besseldot

sal_mod_bessel <- ksvm(salary_train$Salary~. , data = salary_train, kernel = "besseldot")
sal_pred_bessel<-predict(sal_mod_bessel,newdata=salary_test)
mean(sal_pred_bessel==salary_test$Salary)

# kernel = polydot

sal_mod_poly<-ksvm(salary_train$Salary~. , data = salary_train,kernel = "polydot")
sal_pred_poly<-predict(sal_mod_poly,newdata = salary_test)
mean(sal_pred_poly==salary_test$Salary)
