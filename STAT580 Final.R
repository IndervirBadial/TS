#STAT580 final
install.packages("remotes")
remotes::install_github("nickpoison/astsa/astsa_build")
install.packages('forecast', dependencies = TRUE)
install.packages('MLmetrics')
#1
#a
plot.ts(birth)
acf2(birth,max.lag=84)
#not stationary because nonconstant mean and the slow decay of ACF. 
plot.ts(diff(birth))
acf2(diff(birth),max.lag = 84)
#ACF plot above indicates seasonal nonstationarity with s=12
#Thus, we can try accounting for seasonality
#Below we find the seasonal difference of the first difference
plot.ts(diff(diff(birth,lag=12)))
acf2(diff(diff(birth,lag=12)))
#This series appears to be stationary by looking at the time
#series plot and the fact that the ACF decays quickly. Thus, current model is 
#ARIMA(p=0,1,q=0)x(P=0,1,Q=0)_(12)
#Now, we can try to remove the ACF peak at h=12 using a
#(notseasonal) moving average term with s=12 ie)ARIMA(p=0,1,q=1)x(P=0,1,Q=0)_(12)
trans<-diff(diff(birth,lag=12))
sarima(birth,p=0,d=1,q=1,P=0,D=1,Q=0,S=12)
#By the ACF of the residuals, we see that a peak still exists
#at lag=12 so we using a seasonal moving average model to adjust.
sarima(birth,p=0,d=1,q=1,P=0,D=1,Q=1,S=12)
#The ACF and standardized plot of the residuals indicate that the residuals are white noise, because
#ACF decays quickly and stays within the significance limits.
#Also, this model has a lower AIC, AICc, and BICc than the previous model
#which indicates improvement.Lastly, the moving average and seasonal
#estimates coefficients are significant with p-values approximately 0.

#For the 12 month forecast and prediction limits:
#Note: forecast plot and prediction limits are the last 12 points of plot
sarima.for(birth,n.ahead=12,p=0,d=1,q=1,P=0,D=1,Q=1,S=12)
as.data.frame(table(f$pred))


#b)
sarima(birth,p=3,d=0,q=0,P=3,D=0,Q=0,S=12)
#The lack of decay of the ACF of residuals indicate that residuals are not white noise.
#Also, this model has higher AIC,AICc,BIC than our model, so it is not preferred.
#Also, his model has a higher estimated error variance of 51.95 as opposed to our model's 47.4.
sarima.for(birth,n.ahead = 12,p=3,d=0,q=0,P=3,D=0,Q=0,S=12)$se

#validation section
birth_test<-birth[338:373] #last 3 years
birth_train<-birth[1:337] #original minus last 3 years
my_pred<-sarima.for(birth_train,n.ahead=36,p=0,d=1,q=1,P=0,D=1,Q=1,S=12)$pred
other_pred<-sarima.for(birth_train,n.ahead = 36,p=3,d=0,q=0,P=3,D=0,Q=0,S=12)$pred
mymodel_mse<-MSE(my_pred,birth_test)
othermodel_mse<-MSE(other_pred,birth_test)
print(mymodel_mse) 
print(othermodel_mse)
#clearly the multiplicative seasonal moving average model performs better.

#2
#a
oilgas=read.table("https://home.csulb.edu/~skim43/STAT580/OIL.DAT", 
                  header=TRUE)
print(gas1)
plot(1:180,gas1)
yt<-ts(diff(log(gas1)))
xt=ts(diff(log(oil1)))
plot.ts(yt)
acf2(yt,max.lag = 178) #ACF indicates stationarity PACF indicates no seasonality
sarima(yt,p=1,d=0,q=2,no.constant = TRUE) #ACF of residuals indicate white noise
#also, above model has low AIC, so it is sufficient.
sarima.for(yt,n.ahead = 12,p=1,d=0,q=2,no.constant = FALSE)#run this to see prediction interval
transformed_pred<-sarima.for(yt,n.ahead = 12,p=1,d=0,q=2,no.constant = FALSE)$pred
untransformed_pred<- c(341.06)
for (i in 2:length(transformed_pred)){
  untransformed_pred[i]<-exp(transformed_pred[i])*untransformed_pred[i-1]
}
untransformed_pred_ts<-ts(untransformed_pred)
full<-ts(c(gas1,untransformed_pred_ts), start=start(gas1), frequency=frequency(gas1))
plot.ts(full) #plot of time series with forecasted values

#b
#2D scatterplot
lag2.plot(xt,yt,smooth=FALSE,corr=FALSE,max.lag = 10) #2D scatterplot shows linear relationship between
#yt and xt
#CCF of prewhitten
(fit = arima(yt, xreg=time(yt), order=c(1, 0, 2)))
ar1 = as.numeric(fit$coef[1])
ma1=as.numeric(fit$coef[2])
ma2=as.numeric(fit$coef[3])
gas.pw = resid(fit)
oil.d = resid(lm(xt~time(xt), na.action=NULL))
oil.fil = filter(oil.d, filter=c(1, -ar1,-ma1,-ma2), method="conv", sides=1)
ccf(gas.pw, oil.fil, main="", ylab="CCF", na.action=na.omit,lag.max = 40)


#c
#Rregression model
gas.d = resid(lm(gas1~time(gas1), na.action=NULL))
intersection = ts.intersect(oil.d, oil.d1=lag(oil.d,-1), gas.d5=lag(gas1,-5),
                      dframe=TRUE)
summary(intersection.fit <- lm(oil.d~0+oil.d1+gas.d5, data=intersection))

error = filter(resid(intersection.fit), filter=c(1,-om1), method="recur",
                   sides=1)
om1 = as.numeric(intersection.fit$coef[1])
acf2(error)
(eta.fit <- arima(error, order=c(1,0,0)))
perc_change<-(forecast(eta.fit,h=12)$mean)
print(perc_change)
#d
train_gas<-gas1[1:168]
val_gas<-gas[169:180]
a_pred<-sarima.for(train_gas,n.ahead=12,p=1,d=0,q=2,P=0,D=0,Q=0)$pred
c_pred<-sarima.for(train_gas,n.ahead = 12,p=1,d=0,q=2,P=0,D=0,Q=0)$pred
a_mse<-MSE(a_pred,val_gas)
c_mse<-MSE(perc_change,val_gas)
print(a_mse) 
print(c_mse)
