install.packages("plyr")
library(plyr)

x <-  runif(50) # generating 50 random numbers
plot(density(x))
x
range(x)

y <-  runif(50) # generating 50 random numbers 
y
range.default(y)

data <- cbind(x,y) 
data
View(data)

plot(data)

plot(data, type="n")
text(data, rownames(data))

km <- kmeans(data,4) #kmeans clustering
str(km)
km$ifault

install.packages("animation")
library(animation)

km <- kmeans.ani(data, 4)
km$size

#elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
z = sum(apply(normalized_data, 2, var))
wss
for (i in 2:8) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(main = "K-Means Clustering Scree-Plot")

# selecting K for kmeans clustering using kselection
install.packages("kselection")
library(kselection)
data(iris)
View(iris)
k <- kselection(iris[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)
k$max_centers
?kselection

install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
k <- kselection(iris[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)
k

#input<-read.xlsx("E:/Excelr Data/R Codes/Clustering/University_Clustering.xlsx",1)
library(readxl)
input <- read_excel(file.choose())
View(input)
mydata<- input[1:25,c(1,3:8)]
View(mydata)
normalized_data<-scale(mydata[,2:7])
View(normalized_data)

# model Building
fit <- kmeans(normalized_data, 5) # 5 cluster solution
str(fit)
table(fit$cluster)
final2<- data.frame(mydata, fit$cluster) # append cluster membership
View(final2)
library(data.table)
setcolorder(final2, neworder = c("fit.cluster"))
View(final2)
aggregate(mydata[1:25,2:7], by=list(fit$cluster), FUN=mean)
fit$size
--------------------------------------------------------------------------------

# k clustering alternative for large dataset - Clustering Large Applications (Clara)
install.packages("cluster")
library(cluster)
xds <- rbind(cbind(rnorm(5000, 0, 8), rnorm(5000, 0, 8)), cbind(rnorm(5000, 50, 8), rnorm(5000, 50, 8)))
xcl <- clara(xds, 2, sample = 100)
clusplot(xcl)

#Partitioning around medoids
xpm <- pam(xds, 2)
clusplot(xpm)
