library(readxl)
airlines = read_excel("C:\\Users\\arun\\Desktop\\DATA SCIENCE FOLDER\\Clustering\\EastWestAirlines.xlsx",2)
View(airlines)
str(airlines)
attach(airlines)
table(airlines$cc1_miles)
table(airlines$cc2_miles)
table(airlines$cc3_miles)
table(airlines$Award)

#converting certain variables to factor from number
airlines$cc1_miles = as.factor(airlines$cc1_miles)
airlines$cc2_miles = as.factor(airlines$cc2_miles)
airlines$cc3_miles = as.factor(airlines$cc3_miles)
airlines$Award = as.factor(airlines$Award)

#making the dummy variable
library(caret)
dmy1 = dummyVars("~ .", data = airlines)
airline_dmy = data.frame(predict(dmy1, newdata = airlines))
airline_dmy
View(airline_dmy)
str(airline_dmy)

#normalizing the other numerical variables to the range of (0,1) because the dummy variables are either 0 or 1 
normalize = function(x){
  return((x - min(x))/(max(x)-min(x)))
}

#selecting the numerical values to a df dataframe from airlines
df = as.data.frame(airlines[c(2,3,7,8,9,10,11)])
dfnorm = as.data.frame(lapply(df, normalize))
summary(dfnorm)
df1 = as.data.frame(airline_dmy[c(4:16,22:23)])
#combining all to form the final data
final_data = cbind(dfnorm,df1)
View(final_data)

#Hierarichal clustering
d = dist(final_data, method = "euclidean")
model1 = hclust(d, method = "complete")
summary(model1)
str(model1)
plot(model1)
plot(model1, hang = -1)
group = cutree(model1, k=5)
class(group)
rect.hclust(model1,k=5,border = "red")
membership = as.matrix(group)
table(membership)
final1 = data.frame(final_data,membership)
#install.packages("data.table")
library(data.table)
setcolorder(final1,"membership")
View(final1)

getwd()
setwd("C:\\Users\\arun\\Desktop\\DATA SCIENCE FOLDER\\Clustering")
write.csv(final1, file = "final1.csv")

#K Means Clustering
library(plyr)

#finding the k value
wss = (nrow(final_data)-1)*sum(apply(final_data,2,var))
for (i in 1:22) wss[i] = sum(kmeans(final_data, centers = i)$withinss)
plot(1:22, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(main = "K-Means Clustering Scree-Plot")

#model building
library(animation)
fi = kmeans.ani(final_data,4)
str(fi)
table(fi$cluster)
final2 = data.frame(final_data,fi$cluster)
library(data.table)
setcolorder(final2, neworder = "fi.cluster")

aggregate(final_data, by=list(fi$cluster), FUN=mean)
