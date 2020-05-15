wine = read.csv("C:\\Users\\arun\\Desktop\\DATA SCIENCE FOLDER\\PCA\\wine.csv")
View(wine)
cor(wine)

#finding the Principle Components
pc = princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pc)
str(pc)                 
#                         Comp.1    Comp.2    Comp.3     Comp.4     Comp.5    Comp.6     Comp.7     Comp.8     Comp.9    Comp.10    Comp.11    Comp.12     Comp.13     Comp.14
#Cumulative Proportion  0.3954249 0.5737874 0.6770785 0.74335831 0.80603706 0.8540927 0.89364973 0.91865217 0.93969088 0.95842703 0.97455906 0.98661596 0.995870549 1.000000000
#It is clearly shown that for getting the 91% of data we have to take the first 8 principal componets

plot(pc)
#plot shows the greatest contribution by PC1
biplot(pc)
pc$scores[,1:8]

#Hierarichal clustering
wine = cbind(wine, pc$scores[,1:8])
clust_data = pc$scores[,1:8]
norm_clust = scale(clust_data)
dist1 = dist(norm_clust, method = "euclidean")
model1 = hclust(dist1, method = "complete")
plot(model1, hang =-1)
group1 = cutree(model1, 5)
membership1 = as.matrix(group1)
final_1 = cbind(membership1, clust_data)
library(data.table)
setcolorder(final_1,"membership1")
View(final_1)
View(clust_data)

#k-means clustering
library(plyr)

#finding optimum k-value using scree plot or elbow curve
wss = (nrow(norm_clust)-1)*sum(apply(norm_clust, 2, var))
for (i in 1:8) wss[i] = sum(kmeans(norm_clust, centers = i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(main = "K-Means Clustering Scree-Plot")

#model building
library(animation)
model2 = kmeans.ani(norm_clust, 5)
str(model2)
table(model2$cluster)
final2 = data.frame(clust_data, model2$cluster)
setcolorder(final2, neworder = "model2.cluster")
View(final2)

aggregate(final2, by = list(model2$cluster), FUN = mean)

