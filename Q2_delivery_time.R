setwd("C:\\Users\\arun\\Desktop\\DATA SCIENCE FOLDER\\Simple Linear Regression\\Question2")

delivery_time = read.csv("delivery_time.csv")

View(delivery_time)
mean(delivery_time$Sorting.Time)
median(delivery_time$Sorting.Time)
skewness(delivery_time$Sorting.Time)
kurtosis(delivery_time$Sorting.Time)
sd(delivery_time$Sorting.Time)
boxplot(delivery_time$Sorting.Time)
hist(delivery_time$Sorting.Time)
qqnorm(delivery_time$Sorting.Time)
qqline(delivery_time$Sorting.Time)
shapiro.test(delivery_time$Sorting.Time)

plot(delivery_time$Sorting.Time,delivery_time$Delivery.Time)
#linear, moderate and positive relationship

cor(delivery_time$Delivery.Time,delivery_time$Sorting.Time)
cor(delivery_time$Sorting.Time, delivery_time$Delivery.Time)
#r = 0.825

delivery_model = lm(delivery_time$Delivery.Time ~delivery_time$Sorting.Time)
summary(delivery_model)
#y=1.64x + 6.58
#Rsquared - 0.68

delivery_model2 = lm(delivery_time$Delivery.Time ~log(delivery_time$Sorting.Time))
summary(delivery_model2)
#Rsquared - 0.69

delivery_model3 = lm(log(delivery_time$Delivery.Time) ~delivery_time$Sorting.Time)
summary(delivery_model3)
#Rsquared = 0.71


confint(delivery_model,level = 0.95)
predict(delivery_model, interval = "predict")
