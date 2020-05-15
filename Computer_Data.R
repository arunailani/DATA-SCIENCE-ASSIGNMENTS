computer_data = read.csv(file.choose())
View(computer_data)
attach(computer_data)
mean(speed)
median(speed)
sd(speed)
library(moments)
skewness(speed)
kurtosis(speed)
qqnorm(speed)
qqline(speed)
shapiro.test(speed)
hist(speed)
boxplot(speed)
mean(hd)
median(hd)
sd(hd)
skewness(hd)
kurtosis(hd)
qqnorm(hd)
qqline(hd)
shapiro.test(hd)
hist(hd)
boxplot(hd)
# similarly we can continue with the EDA for each variable

summary(computer_data)

plot(computer_data)

cor(computer_data)

# cor function gives the strength of relationship between different variables.
# Since the dataset contains the columns with categorical values, so cor function will result into error
# Converting the categorical to numerical values using dummy variables with caret package
install.packages("caret")
library(caret)
dmy = dummyVars("~ .", data = computer_data)
class(dmy)
dmy
comp_dmy = data.frame(predict(dmy, newdata = computer_data))
comp_dmy

# now we can use cor function on comp_dmy
cor(comp_dmy)
# All the values are below 0.85, so it shows either moderate or a weak relationship between variables.
# Hence we can continue with the model building

library(psych)
pairs.panels(computer_data)
# This function gives the scatterplot, histogram and r values at a same time for analysis.

#model
model.comp = lm(price~speed+hd+ram+screen+cd.no+cd.yes+multi.no+multi.yes+premium.no+premium.yes+ads+trend, data = comp_dmy)
summary(model.comp)
# All the p-values are significant for each variable
#Price = 9.32028(speed) + 0.78178(hd) + 48.25596(ram) + 123.08904(screen) + (-60.91671)(cd.no) + (-104.32382)(multi.no) + 509.22473(premium.no) + 0.65729(ads) + (-35.99622) 
