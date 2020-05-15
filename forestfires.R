forest <- read.csv(file.choose())
dim(forest)
View(forest)

# partitioning the data in train and test using function createDataPartition from caret package 
# for the equal proportional distribution
library(caret)
part <- createDataPartition(forest$area, p=.7, list=FALSE)
train_data <-  forest[part,]
test_data <- forest[-part,]
View(train_data)

library(kernlab)

# kernel = vanilladot

mod1 <- ksvm(train_data$area~., data = train_data, kernel = "vanilladot")
pred_vanilladot <- predict(mod1, newdata = test_data)
mean(pred_vanilladot==test_data$area)

# kernel = rbfdot

mod2 <- ksvm(train_data$area~., data = train_data, kernel = "rbfdot")
pred_rbfdot <- predict(mod2, newdata = test_data)
mean(pred_rbfdot==test_data$area)

# kernel = besseldot

mod3 <- ksvm(train_data$area~. , data = train_data, kernel = "besseldot")
pred_bessel<-predict(mod3,newdata=test_data)
mean(pred_bessel==test_data$area)

# kernel = polydot

mod4 <-ksvm(train_data$area~. , data = train_data,kernel = "polydot")
pred_poly<-predict(mod4,newdata = test_data)
mean(pred_poly==test_data$area)

