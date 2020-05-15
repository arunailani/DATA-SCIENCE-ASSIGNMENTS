setwd("C:\\Users\\arun\\Desktop\\DATA SCIENCE FOLDER\\Simple Linear Regression\\Question3")

emp_data = read.csv("emp_data.csv")
View(emp_data)
mean(emp_data$Salary_hike)
median(emp_data$Salary_hike)
sd(emp_data$Salary_hike)
skewness(emp_data$Salary_hike)
kurtosis(emp_data$Salary_hike)
boxplot(emp_data$Salary_hike)
hist(emp_data$Salary_hike)
qqnorm(emp_data$Salary_hike)
qqline(emp_data$Salary_hike)
shapiro.test(emp_data$Salary_hike)
plot(emp_data$Salary_hike,emp_data$Churn_out_rate)
cor(emp_data$Salary_hike,emp_data$Churn_out_rate)
#r = -0.91
emp_model = lm(emp_data$Churn_out_rate~emp_data$Salary_hike)
summary(emp_model)
#y=-.0.10154x + 244.36491

confint(emp_model,level = 0.95)
