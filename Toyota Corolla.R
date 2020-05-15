toyota = read.csv(file.choose())
View(toyota)
attach(toyota)

summary(toyota)

#bifurcating the dataset as per the required columns
new_toyota = toyota[, c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
summary(new_toyota)
attach(new_toyota)

#finding the corelation values between variables
cor(new_toyota)
#Price and Age_08_04 have strong relationship while others ahve either moderate or weak relationship

#building the model
model_toyota = lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(model_toyota)
#based on model only the variables cc abd doors does not have a significant p-value

#building model only for CC gives a significant p-value
model_toyotaS = lm(Price~cc)
summary(model_toyotaS)

#building model only for doors gives a significant p-value
model_toyotaD = lm(Price~Doors)
summary(model_toyotaD)

#both are significant individually

model_toyotaDS = lm(Price~cc+Doors)
summary(model_toyotaDS)
#the model is significant even when combined

#finding the influential observations to omit them
influence.measures(model_toyota)
influenceIndexPlot(model_toyota, id.n=3)
influencePlot(model_toyota, id.n=5, id.cex=0.7)

#from the cook's plot, eliminating the 81st observation and building the model
model_toyota1 = lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = toyota[-81,])
summary(model_toyota1)
#After the elimination of 81st observation the p-value of cc became significant

#eliminating 961 and 222 observations also and building the model
model_toyota2 = lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = toyota[-c(81,961,222),])
summary(model_toyota2)
#so all the values are significant in this model


plot(model_toyota2)

