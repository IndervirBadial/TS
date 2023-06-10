install.packages("remotes")
remotes::install_github("nickpoison/astsa/astsa_build")
#1.6a
set.seed(5766)
x10<-arima.sim(list(order = c(0,0,1), ma=0.9),n=10)
x100<-arima.sim(list(order = c(0,0,1), ma=0.9),n=100)
x1000<-arima.sim(list(order = c(0,0,1), ma=0.9),n=1000)
acf(x10,ci = 0.95,lag.max = 20)
acf(x100,ci=.95,lag.max=20)
acf(x1000,ci=.95,lag.max=20)

#1.6b
print(acf(x10,type=c('covariance'))) #autocovariancess at h=0,1...9
print(acf(x10,lag.max=10)) #autocorrelations at h=0,1...9
#1.6c
lag1.plot(x100,max.lag=20,smooth =FALSE,corr=FALSE)

#1.7a
plot.ts(varve)
acf2(varve)

#1.7b
hist(varve,main='Untransformed')
hist(log(varve),main='Log transformed',)
lag1.plot(varve,max.lag=1,smooth =FALSE,corr=TRUE)
lag1.plot(log(varve),max.lag=1,smooth =FALSE,corr=TRUE)
plot.ts(log(varve))
acf2(log(varve),max.lag = 30)
print(acf(varve,type=c("covariance"),lag.max=20)[0])
print(acf(log(varve),type=c("covariance"),lag.max=20)[0])

#1.7c
plot(diff(log(varve)))
acf(diff(log(varve)))
#1.7d
print(acf(diff(log(varve)),type=c('covariance')))
print(acf(diff(log(varve))))

#1.8a
oilgas=read.table("https://home.csulb.edu/~skim43/STAT580/OIL.DAT", 
           header=TRUE)
gas1<-oilgas$GAS
oil1<-oilgas$OIL
plot.ts(gas1)
plot.ts(oil1)
print(acf(gas1,lag.max = 30))
print(acf(oil1,lag.max = 30))
#since ACF has peaks at every lag (h) value, we know that
#both oil and gas are nonstationary

#1.8c
acf(diff(log(gas1)),lag.max = 40) #autocorrelation function for gas
acf(diff(log(oil1)),lag.max = 40) #autocorrelation function for oil
#note that both ACF plots indicate stationarity because peaks die
#out as lag increases
ccf(diff(log(gas1)), diff(log(oil1)),lag.max = 40) #cross correlation
lag2.plot(diff(log(gas1)),diff(log(oil1)),max.lag=10,smooth =FALSE,corr=FALSE) #max lag set to 10 for clarity
xgas=ts(diff(log(gas1)))
yoil=ts(diff(log(oil1)))
ts.plot(xgas,yoil,col=1:2) #gas is red, oil is black