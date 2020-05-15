library(forecast)
library(fpp)
library(smooth)
# Loading coco cola sales Data
library("readxl")
coke<-read_xlsx(file.choose())
View(coke)
library(tseries)
class(coke)
cokets<-ts(coke$Sales,frequency = 4,start=c(86))
View(cokets)
train<-cokets[1:38]
test<-cokets[39:42]
train<-ts(train,frequency = 4)
test<-ts(test,frequency = 4)
plot(cokets)

#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F) # simple exponential smooting#
hw_a
hwa_pred<-forecast(hw_a)
hwa_pred<-data.frame(predict(hw_a,n.ahead=4))
plot(forecast(hw_a,h=4))
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape
# with alpha = 0.2, beta = 0.1

hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 4))
plot(forecast(hw_ab,h=4))
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape
# with alpha = 0.2, beta = 0.1, gamma = 0.1 

hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 4))
plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
hwabg_mape
# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape
hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape

hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape
############################## STOP HERE ###############################

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)
