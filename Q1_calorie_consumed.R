setwd("C:\\Users\\arun\\Desktop\\DATA SCIENCE FOLDER\\Simple Linear Regression\\Question1")

calorie_cons = read.csv("calories_consumed.csv")

View(calorie_cons)

mean(calorie_cons$Calories.Consumed)

median(calorie_cons$Calories.Consumed)

library(moments)

sd(calorie_cons$Calories.Consumed)

var(calorie_cons$Calories.Consumed)

qqnorm(calorie_cons$Calories.Consumed)

qqline(calorie_cons$Calories.Consumed)

skewness(calorie_cons$Calories.Consumed)

kurtosis(calorie_cons$Calories.Consumed)

boxplot(calorie_cons$Calories.Consumed)

hist(calorie_cons$Calories.Consumed)

shapiro.test(calorie_cons$Calories.Consumed)

plot(calorie_cons$Calories.Consumed,calorie_cons$Weight.gained..grams.)
#plot represents a linear, positive and strong relationship

cor(calorie_cons$Weight.gained..grams.,calorie_cons$Calories.Consumed)
#correlation coefficient is very high i.e. 0.946

cal_model = lm(calorie_cons$Weight.gained..grams. ~calorie_cons$Calories.Consumed)
#buliding the linear regression model

summary(cal_model)
#y = -652.75236 + 0.42016(x) <- is the best fit line equation
#r-squared value is 0.8968 which is quite high. So the given model is a acceptable model
#p-value<0.05

confint(cal_model, levl=0.95)
#(Intercept)                    -845.4266546 -406.0780569
#calorie_cons$Calories.Consumed    0.3305064    0.5098069

predict(cal_model, interval = "predict")
