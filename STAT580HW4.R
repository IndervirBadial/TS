#2.6
la_reg<-as.data.frame(lap)
col2<-rep(1,508)
col3<-1:508
df<-data.frame(la_reg$cmort,col2,col3,la_reg$tempr,la_reg$part)
#a
pairs(~ cmort + tempr + part, data = df)
ccf(la_reg$cmort,la_reg$part,lag.max = 24)
ccf(la_reg$cmort,la_reg$tempr,lag.max=24)
#b
temprcenter = tempr-mean(tempr) #center temperature
temp2 = (temprcenter)^2
fit = lm(cmort~ col3 + col2 + temprcenter + temp2 + part -1, na.action=NULL)
summary(fit) # regression results

 #c
acf2(resid(fit),max.lag=52) #indicates we should fit an AR(2) model for the residuals
#because PACF after h=2 is basically 0. Note:AR(2)=ARIMA(2,0,0)
fit2=arima(resid(fit),order = c(2, 0, 0))
acf2(resid(fit2),max.lag = 40)

#d
finalarima=arima(cmort,order=c(2,0,0),xreg=cbind(col3,temprcenter,temp2,part))
finalarimacoef=finalarima[["coef"]]
finalarimacoef[c("ar1","ar2","trend","temprcenter","temp2","part")]
sarima.for(cmort,12,2,0,0)

#2.7
phi<-numeric(10) 
theta<-numeric(10)
sigmasquare<-numeric(10)
set.seed(2837)
for(i in 1:10){
  x<-arima.sim(n=200,list(ar=.9,ma=.2),sd=sqrt(.25))
  fit<-arima(x,order=c(1,0,1))
  phi[i]<-fit$coef[1]
  theta[i]<-fit$coef[2]
  sigmasquare[i]<-fit$sigma2
}
print(mean(phi))
print(sd(phi))
print(mean(theta))
print(sd(theta))
print(mean(sigmasquare))
print(sd(sigmasquare)) 

#2.8
plot.ts(prodn)
plot.ts(unemp)
acf2(unemp)
acf2(prodn) #indicates nonstationarity
acf2(diff(prodn)) #indicates seasonality at lag=12,24,36...
acf2(diff(diff(prodn), 12), 48)
sarima(prodn, 2, 1, 0, 2, 1, 1, 12) #fit model 
sarima.for(prodn, 12, 2, 1, 0, 2, 1, 1, 12)$pred #forecast



