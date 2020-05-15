affairs = read.csv(file.choose())
View(affairs)
attach(affairs)
detach(affairs)
summary(affairs)
str(affairs)
factor(affairs$affairs)

#since the affairs is dependent column so making its value to 0 and 1 for building the confusion matrix
affairs$affairs = ifelse(affairs$affairs > 0, 1, 0)

#model building
model1 = glm(factor(affairs)~gender+age+yearsmarried+children+factor(religiousness)+factor(rating), family = "binomial", data = affairs)
summary(model1)

model2 = glm(affairs~gender+age+yearsmarried+children+religiousness+education+occupation+rating, family = "binomial", data = affairs)
summary(model2)

prob1 = predict(model1,type="response")
confusion1 = table(prob1>0.5,affairs$affairs)
confusion1


Accuracy<-sum(diag(confusion2)/sum(confusion2))
Accuracy

prob2 = predict(model2,type="response")
confusion2 = table(prob2>0.5,affairs$affairs)
confusion2
