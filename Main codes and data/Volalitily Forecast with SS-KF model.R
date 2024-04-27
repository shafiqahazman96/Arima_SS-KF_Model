
# Set Directory
setwd("C:/Users/HP/OneDrive - Universiti Malaya/2020")

# Call for packages
require("tseries") # load the tseries library
require("zoo")
require("moments")
require("car")
require("MASS")
require("stats")  
require("fGarch")
require("PerformanceAnalytics") # To draw Timeseries graphs
require("rugarch")
library(tidyverse)
library(xts)
library(forecast)
library(scales)
library(e1071)
library(xtable)
library(quantmod)
library(KFAS)
library(fpp2)
library(tidyquant)

rm(list=ls(all=TRUE)) # Remove objects from environment
options(warn=-1) # To ignore any warning messages


## Import data 
data_hourly <- read.csv("BTC.csv")
data_hourly$datetime <- as.Date(as.POSIXct(data_hourly$unix, origin="1970-01-01", tz="GMT"))

dataset <- xts(data_hourly$close, order.by=data_hourly$datetime)
# Subset data to date 26 Feb 2020 to 18 April 2021
dataset <- dataset["2020-02-26::2021-04-18"]

## differencing (log return)
return.cc = diff(log(dataset))

# Set 10,000 hours 
ret.cc=tail(return.cc,10000)

## ugarchspec: Univariate GARCH specification object prior to fitting
garch.t = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                     variance.model=list(garchOrder=c(1,1)),
                     distribution.model = "std")
obs.vol<-rep(0, 10000)
sd.ret<-sd(ret.cc[,1])
ret.abs<-abs(ret.cc[,1]-mean(ret.cc[,1]))
mean.abs<-mean(ret.abs)
rho.hat<-mean.abs/sd.ret

# Observed volatility = mean absolute/std deviation
obs.vol<-as.data.frame(ret.abs/rho.hat)  

# Tab. 2: Descriptive Statistics
skew <-skewness(ret.cc[, 1], na.rm =TRUE,type = 1)
kur<-kurtosis(ret.cc[, 1], na.rm =TRUE, type=1)
maximum<-max(ret.cc[, 1], na.rm =TRUE, type=1)
minimum<-min(ret.cc[, 1], na.rm =TRUE, type=1)
Mean<-mean(ret.cc[, 1], na.rm =TRUE, type=1)
Median <-median(ret.cc[, 1], na.rm =TRUE, type=1) 


######### rugarch##############################################################

# Fit univariate GARCH model

rr.garch.t = ugarchfit(data=ret.cc, spec=garch.t)  # garch.t from ugarchspec
pred = ugarchforecast(rr.garch.t, data=ret.cc, n.ahead=1) # forecast ugarch model


######### kalman filter ##############################################################

# Fit ARIMA SS model w/ Kalman

# 1) Fit ARIMA model
model=auto.arima((as.vector(dataset))) 
model

# 2) Extract arima coefficient

# View coefficient
model$coef

# Coefficient AR
ar.coef=model$coef[1:5]

# Coefficitent MA
#ma.coef=model$coef[4:5]

# 3) Fit SSmodel

# Create SS model
model_arima <- SSModel(obs.vol$V1 ~ SSMarima(d=2, ar=ar.coef))

# Kalman Filtering & Smoothing
out <- KFS(model_arima)  

# One step ahead estimation
fit.kf <- as.matrix(out$a) 
fit.kf <- fit.kf[2:10001,1]

######### nnetar ##############################################################
train<-as.ts(obs.vol)
nnetar.fit<-nnetar(train)


##### Forecasted Volatility ############################################################

## Fig. 3: Plot of observed and forecasted volatility 

time<-seq(from=1, to=nrow(obs.vol), by=1)

plot(as.ts(obs.vol), lwd=5, col="yellow", ylab=" ", xlab="")
lines(time, fit.kf, lwd=2,col="blue")                 # kfs 
lines(time, nnetar.fit$fitted, lwd=2, col="red")      # nnetar
lines(time,sigma(rr.garch.t), lwd=3,col="green")      # rugarch
mtext("Time", side=1, line=2.5, cex=0.8)
mtext(expression(hat(sigma)), side=2, line = 2.5, cex=0.8)



######## mean of fitted value and root mean square error ###########################

## Tab. 3: Error comparison 

residuals.tgarch<-obs.vol[,1]-sigma(rr.garch.t)
residuals.kf<-obs.vol[,1]-fit.kf

## MAE
mae.tgarch<-mean(abs(((residuals.tgarch))))
mae.nnetar<-mean(abs(na.omit(nnetar.fit$residuals)))
mae.kf<-mean((abs(residuals.kf)))
mae.tgarch;mae.nnetar;mae.kf

## RMSE
rmse.tgarch<-sqrt(mean(residuals.tgarch^2))
rmse.nnetar<-sqrt(mean(na.omit(nnetar.fit$residuals)^2))
rmse.kf<-sqrt(mean(residuals.kf^2))
rmse.tgarch;rmse.nnetar;rmse.kf


######## residual check ###############################################################

## Fig. 4-8: Residual, ACF and Histogram 

## GARCH model

#Residuals
Residuals=as.vector(residuals.tgarch)
plot(Residuals, type = "l", xlab="Time")

#ACF
par(mar=c(5,6,4,1)+.1)
acf(Residuals, cex.axis=2, cex.lab=1, cex.axis=1)

#Histogram
par(mar=c(5,6,4,1)+.1)
hist(Residuals, breaks=50, col="black", prob=TRUE, xlab="",ylab="")
mtext("Frequency", side=2, line = 3.5, cex=1)
mtext("Residual", side=1, line = 3.5, cex=1, outer = F)


## NNETAR model

#Residuals
Residuals=as.vector(na.omit(nnetar.fit$residuals))
plot(Residuals, type = "l", xlab="Time")

#ACF
par(mar=c(5,6,4,1)+.1)
acf(Residuals, cex.axis=2, cex.lab=1, cex.axis=1)

#Histogram
par(mar=c(5,6,4,1)+.1)
hist(Residuals, breaks=50, col="black", prob=TRUE, xlab="",ylab="")
mtext("Frequency", side=2, line = 3.5, cex=1)
mtext("Residual", side=1, line = 3.5, cex=1, outer = F)


## SSM-KF model

#Residuals
Residuals=as.vector(residuals.kf)
plot(Residuals, type = "l", xlab="Time")

#ACF
par(mar=c(5,6,4,1)+.1)
acf(Residuals, cex.axis=2, cex.lab=1, cex.axis=1)

#Histogram
par(mar=c(5,6,4,1)+.1)
hist(Residuals, breaks=50, col="black", prob=TRUE, xlab="",ylab="")
mtext("Frequency", side=2, line = 3.5, cex=1)
mtext("Residual", side=1, line = 3.5, cex=1, outer = F)

dev.off()



##### alpha cut for VaR######################################################################


garch.t = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                     variance.model=list(garchOrder=c(1,1)),
                     distribution.model = "std")

rr.garch.t = ugarchfit(data=ret.cc, spec=garch.t)  # garch.t from ugarchspec
df.c<-as.numeric(coef(rr.garch.t))[5]  # shape
alpha=0.01

#c.v is the critical value
c.v.df=qt(alpha, df=df.c)  # quantile t-distn, alpha is the probability  


## GARCH model

sigma.rugarch<-as.vector(sigma(pred)) # extract conditional sigma values of the forecast from ugarch model
VaR.rugarch<--10000*sqrt((df.c-2)/df.c)*sigma.rugarch*c.v.df


## NNETAR model

sigma.nnetar<-coredata(forecast(nnetar.fit, h=1)$mean)  # extracting forecasted sigma
VaR.nnetar<--10000*sqrt((df.c-2)/df.c)*sigma.nnetar*c.v.df


## KF

sigma.kf=coredata((predict(out$model, n.ahead=1)))
VaR.kf<--as.vector(10000*sqrt((df.c-2)/df.c)*sigma.kf*c.v.df)

# Std Error 
SE.res.nn=rmse.nnetar/sqrt(10000)
SE.res.kf=rmse.kf/sqrt(10000)
SE.res.tgarch=rmse.tgarch/sqrt(10000)

# Alpha for Confidence Interval
alpha.CI=seq(from=0.01, to=1, by=0.01)
l=length(alpha.CI)

## cv.norm is critical value for normal distn
cv.norm<-rep(0, l)
for(i in 1:l) cv.norm[i]=qnorm(1-alpha.CI[i]/2, mean=0, sd=1)

intervalGC<-as.data.frame(matrix(0, ncol=2, nrow=l))
colnames(intervalGC)<-c("lower", "upper")
rownames(intervalGC)<-as.character(percent(1-alpha.CI))

intervalNN<-as.data.frame(matrix(0, ncol=2, nrow=l))
colnames(intervalNN)<-c("lower", "upper")
rownames(intervalNN)<-as.character(percent(1-alpha.CI))

intervalKF<-as.data.frame(matrix(0, ncol=2, nrow=l))
colnames(intervalKF)<-c("lower", "upper")
rownames(intervalKF)<-as.character(percent(1-alpha.CI))

mean<-(sigma.nnetar+sigma.kf+sigma.rugarch)/3

for(i in 1:l){
  intervalGC[i, ]<-c(mean-cv.norm[i]*SE.res.tgarch, mean+cv.norm[i]*SE.res.tgarch)
  intervalNN[i, ]<-c(mean-cv.norm[i]*SE.res.nn, mean+cv.norm[i]*SE.res.nn)
  intervalKF[i, ]<-c(mean-cv.norm[i]*SE.res.kf, mean+cv.norm[i]*SE.res.kf)
}

## cv.norm is critical value for normal distn
cv.norm<-rep(0, l)
for(i in 1:l) cv.norm[i]=qnorm(1-alpha[i]/2, mean=0, sd=1)  

intervalGC.VaR<-as.data.frame(matrix(0, ncol=2, nrow=l))
colnames(intervalGC.VaR)<-c("lower", "upper")
rownames(intervalGC.VaR)<-as.character(percent(1-alpha.CI))
intervalNN.VaR<-as.data.frame(matrix(0, ncol=2, nrow=l))
colnames(intervalNN.VaR)<-c("lower", "upper")
rownames(intervalNN.VaR)<-as.character(percent(1-alpha.CI))
intervalKF.VaR<-as.data.frame(matrix(0, ncol=2, nrow=l))
colnames(intervalKF.VaR)<-c("lower", "upper")
rownames(intervalKF.VaR)<-as.character(percent(1-alpha.CI))

mean<-(sigma.nnetar+sigma.kf+sigma.rugarch)/2*(-1000)*c.v.df

for(i in 1:l){
  intervalGC.VaR[i, ]<-intervalGC[i, ]*(-10000)*c.v.df*sqrt((df.c-2)/df.c)
  intervalNN.VaR[i, ]<-intervalNN[i, ]*(-10000)*c.v.df*sqrt((df.c-2)/df.c)
  intervalKF.VaR[i, ]<-intervalKF[i, ]*(-10000)*c.v.df*sqrt((df.c-2)/df.c)
}

######## VAR ###############################################################

## Fig. 9: Value-at-Risk forecast Confidence Interval

plot(intervalGC.VaR[,1], alpha.CI, xlab="VaR", type="l",
     ylab=expression(alpha), xlim=c(min(intervalGC.VaR)-1, max(intervalGC.VaR)+1), col="yellow", main="", lwd=3)
lines(intervalGC.VaR[, 2], alpha.CI, col="yellow", lty=1, lwd=3)
lines(intervalNN.VaR[, 1], alpha.CI, col="red", lty=2, lwd=2)
lines(intervalNN.VaR[, 2], alpha.CI, col="red", lty=2, lwd=2)
lines(intervalKF.VaR[, 1], alpha.CI, col="purple", lty=2, lwd=3)
lines(intervalKF.VaR[, 2], alpha.CI, col="purple", lty=2, lwd=3)
legend("topright",c("GARCH (1,1)","NNAR", "SS"),
       lty=c(1,1,2),lwd=c(3,3,3),pt.cex=0.9, col=c("yellow","red","purple"), cex=0.5)


######## Volatility #############################################################

## Fig. 10: Volatility forecast Confidence Interval

plot(intervalGC[,1], alpha.CI, xlab=expression(sigma), type="l",
     ylab=expression(alpha), xlim=c(min(intervalGC)-0.001, max(intervalGC)+0.001), col="yellow", main="", lty=1, lwd=3)
lines(intervalGC[, 2], alpha.CI, col="yellow", lty=1, lwd=3)
lines(intervalNN[, 1], alpha.CI, col="red", lty=2, lwd=2)
lines(intervalNN[, 2], alpha.CI, col="red", lty=2, lwd=2)
lines(intervalKF[, 1], alpha.CI, col="purple", lty=2, lwd=3)
lines(intervalKF[, 2], alpha.CI, col="purple", lty=2, lwd=3)

legend("topright",c("GARCH (1,1)","NNAR", "SS"),
       lty=c(1,1,2),lwd=c(3,3,3),pt.cex=0.9, col=c("yellow","red","purple"), cex=0.5)
