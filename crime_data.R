crime = read.csv(file.choose())
View(crime)

#normalising the data to calculate the distance matrix
crime_normalised = scale(crime[,2:5])
View(crime_normalised)

#calculating the eculidean distance matrix
eucd = dist(crime_normalised, method = "euclidean")
eucd

#finding the k-value using scree plot or elbow curve
wss1 = (nrow(crime_normalised)-1)*(sum(apply(crime_normalised,2,var)))
for (i in 2:5) wss1[i] = sum(kmeans(crime_normalised, centers = i)$withinss)
plot(1:5, wss1, type = 'b', xlab = "number of clusters", ylab = "within sum of squares")
# k = 4

#model building
model1 = kmeans(crime_normalised, 2)
str(model1)
model1 = kmeans.ani(crime_normalised,2)

model2 = kmeans(crime_normalised,4)
str(model2)
model2 = kmeans.ani(crime_normalised,4)
table(model2$cluster)
final_data = data.frame(crime, model2$cluster)
library(data.table)
setcolorder(final_data, neworder = "model2.cluster")
View(final_data)
aggregate(crime[,2:5], by = list(model2$cluster), FUN = mean)
aggregate(crime[,2:5], by = list(model2$cluster), FUN = median)

setwd("C:\\Users\\arun\\Desktop\\DATA SCIENCE FOLDER\\Clustering")
write.csv(final_data, file = "final_crime_report.csv")

#INFERENCES FROM THE CLUSTERS
#From the aggregate function group wise, we can say that the murder rate, assault rate 
#and rape rate are least in group 3 when compared to other groups. Group3 consist of 11 places
#of United States. Group 1 and Group 2, both have high crime rates and are nearly equal. 