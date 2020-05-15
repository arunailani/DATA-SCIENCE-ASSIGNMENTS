startups = read.csv(file.choose())
View(startups)

summary(startups)
plot(startups)
#plot function gives all the scatter plots between various variables in the form of matrix

cor(startups)
# cor function gives the strength of relationship between different variables.
# Since the dataset contains the columns with categorical values, so cor function will result into error
# Converting the categorical to numerical values using dummy variables with caret package

library(caret)
dmy1 = dummyVars("~ .", data = startups)
startup_dmy = data.frame(predict(dmy1, newdata = startups))
startup_dmy

# now we can use cor function on startup_dmy
cor(startup_dmy)
# Most of the values are below 0.85, so it shows either moderate or a weak relationship between variables except the relationship between R.D.Spend and Profit.

# evaluating the partial corelation matrix
library(corpcor)
cor2pcor(cor(startup_dmy))

attach(startups)
attach(startup_dmy)
#building the model
model_startup = lm(Profit~R.D.Spend+Administration+Marketing.Spend+State.California+State.Florida+State.New.York)
summary(model_startup)
# Only the R.D.Spend has a significant value for p, other have >0.05

#model using only administration
model_startupA = lm(startups$Profit~startups$Administration)
summary(model_startupA)
#does not have a significant p value

#model using only marketing.spend
model_startupMS = lm(startups$Profit~startups$Marketing.Spend)
summary(model_startupMS)
#has a significant p value

#model using only state.california
model_startupS = lm(startups$Profit~State.California)
summary(model_startupS)
#does not have a significant p value

#model using only state.florida
model_startupSF = lm(startups$Profit~State.Florida)
summary(model_startupSF)
#does not have a significant p value

#model using only state.NewYork
model_startupSNY = lm(startups$Profit~State.New.York)
summary(model_startupSNY)
#does not have a significant p value

library(psych)
pairs.panels(startups)

#finding the influential observations
influence.measures(model_startup)
library(car)
influenceIndexPlot(model_startup, id.n = 3)
influencePlot(model_startup,id.n = 3)

#building the model by omiting the influential observations
model_startup1 = lm(Profit~R.D.Spend+Administration+Marketing.Spend+State.California+State.Florida+State.New.York, data = startup_dmy[-47,])
summary(model_startup1)

model_startup2 = lm(Profit~R.D.Spend+Administration+Marketing.Spend+State.California+State.Florida+State.New.York, data = startup_dmy[-c(47,49),])
summary(model_startup2)

model_startup3 = lm(Profit~R.D.Spend+Administration+Marketing.Spend+State.California+State.Florida+State.New.York, data = startup_dmy[-c(49,50),])
summary(model_startup3)
#by building all the models the pvalue has not became significant, so moving on with deleting the columns

library(car)
model_start = lm(Profit~R.D.Spend+Administration+Marketing.Spend+State)
vif(model_start)

avPlots(model_start, id.n = 2, id.cex =0.7)

#removing the column state to achieve the significancy
startup_final1 = lm(startups$Profit~startups$R.D.Spend+startups$Administration+startups$Marketing.Spend)
summary(startup_final1)
#p value does not became significant

#removing the administration column and building the model to achieve significancy
startup_final2 = lm(Profit~R.D.Spend+Marketing.Spend)
summary(startup_final2)
#p-values has not became significant

#removing the marketing spend and building the model
startup_final3 = lm(Profit~R.D.Spend)
summary(startup_final3)
#the model has finally became significant
#profit = 8.543e-01(R.D.spend) + 4.903e+04 

plot(startup_final3)

