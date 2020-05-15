salary_train_data = read.csv(file.choose())
salary_test_data <- read.csv(file.choose())

View(salary_train_data)
View(salary_test_data)

dim(salary_test_data)
table(salary_train_data$Salary)
table(salary_test_data$Salary)

# checking that the proportion of salary distribution data is similar in train and test data
prop.table(table(salary_train_data$Salary))
# 0.7510693 0.2489307 

prop.table(table(salary_test_data$Salary))
# 0.7543161 0.2456839 

#checking the total number of na values in train and test data
sum(is.na(salary_train_data))
# 0 na values

sum(is.na(salary_test_data))
# 0 na values

# barplot representations for salary distributions for the train and test data
barplot(table(salary_train_data$Salary))
barplot(table(salary_test_data$Salary))

#salary is the dependent variable for building the model and rest are independent variables

#model building
library(e1071)

#building the model on training data
m1 = naiveBayes(salary_train_data$Salary~. , data = salary_train_data)

#prdeicting on test data
pred <- predict(m1, newdata = salary_test_data[,-14])

#calculating the accuracy of the model
mean(pred==salary_test_data[,14])
#81.9%


