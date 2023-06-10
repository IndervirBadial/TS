install.packages("remotes")
remotes::install_github("nickpoison/astsa/astsa_build")
#3.6a
plot.ts(prodn)
plot.ts(unemp)
par(mfrow=c(2,2))
spectrum(prodn,plot=TRUE,log='no')
spectrum(prodn,plot=TRUE) #log spectrum
spectrum(unempn,plot=TRUE,log="no")
spectrum(unempn,plot=TRUE) #log spectrum


#smoothing
par(mfrow=c(2,2))
spec.pgram(prodn, 3,log="no")
spec.pgram(prodn, 5,log="no")
spec.pgram(unemp, 3,log="no")
spec.pgram(unemp, 5,log="no")
par(mfrow=c(2,2))
spec.pgram(prodn, 3)
spec.pgram(prodn, 5)
spec.pgram(unemp, 3)
spec.pgram(unemp, 5)
#coherence
sr=spec.pgram(cbind(prodn, unemp),5,log='yes',plot=FALSE)
sr$df # df = 35.8625
f = qf(.95, 2, sr$df-2) # = 8.529792
C = f/(4+f) # = 0.318878
plot(sr, plot.type = "coh",ci=0)


#high coherence=frequency ranges where we can predict one series, say prodn, with unemp using a filter.

#b
k2 = kernel("daniell",7)
spec.pgram(diff(prodn), k2, taper=0, log="no", main = "Production with smoothing 7")

# 12 month seasonal difference:
par(mfrow=c(1,1))
k = kernel("modified.daniell", 6) # filter weights
spec.pgram(kernapply(diff(prodn), k), taper=0, log="no")


#3.7a
sr=spec.pgram(cbind(sales, lead),log='yes',kernel("daniell",5),taper=0,plot=FALSE)
plot(sr, plot.type = "coh",ci=0)
print(mean(sr$coh))
par(mfrow=c(1,1))
#b
LagReg(sales, lead, L=3, M=16, threshold=1) #sales input, lead output
LagReg(lead, sales, L=3, M=16, threshold=1) #lead input, sales output

#3.8b
LagReg(inflow, precip, L=5, M=16,threshold=1) #inflow input, precip output


auto.arima(diff(log(prodn)))
sarima(diff(log(prodn)), 3, 0, 2, 0, 0, 0, 12)
sarima.for(diff(log(prodn)), 12, 3, 0, 2, 0, 0, 0, 12)




