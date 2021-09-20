library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(ggfortify)
library(corrplot) #Plot Correlations
library(lubridate)
library(tseries)
library(forecast)
library(lmtest)
library(fGarch)
source("eacf.R")
source("backtest.R")
source("backtestGarch.R")


hdfc <- read.csv(file="HDFC.csv", header=TRUE, sep=",")

dim(hdfc)
head(hdfc)
names(hdfc)
class(hdfc$Date)
hdfc$Date <- ymd(hdfc$Date)
hdfc

sum(is.na(hdfc))

#remove missing in columns
hdfc = hdfc[,1:12]
str(hdfc, list.len=ncol(hdfc))
#put all num in subset
stock = hdfc[,1:9]
head(stock)
tail(hdfc)

library(psych)
describe(hdfc)
describe(stock)

ggpairs(stock)
M<-cor(stock, method="spearman")
M

qqnorm(hdfc$Close)
qqline(hdfc$Close, col="red", lw=2)

hist(hdfc$Close, xlab="HDFC Stock Close Price", prob=TRUE, main="Histogram")
xfit = seq(min(hdfc$Close),max(hdfc$Close),length=40)
yfit = dnorm(xfit,mean=mean(hdfc$Close),sd=sd(hdfc$Close))
lines(xfit, yfit, col="blue", lwd=2) 



#hdfcTS = ts(hdfc$Close, start = c(2000, 1, 3), end = c(2020, 9, 30), frequency = 365)
#trading days
hdfcTS = ts(log(hdfc$Close), start = c(2000, 1, 3), end = c(2020, 9, 30), frequency=12)
hdfcTS
autoplot(hdfcTS, main="Time Plot of HDFC Close Log Price", ylab="ln(Close Price)", xlab="Date")
autoplot(log(hdfcTS), main="Time Plot of HDFC Close Log Price", ylab="ln(Close Price)", xlab="Date")

r = diff(hdfcTS)  #this gets rid of unit root
autoplot(r, xlab="Date", ylab="Close Daily Return", main="HDFC Daily Log-Returns")

#par(mfrow=c(1,2))

qqnorm(r)
qqline(r, col="red", lw=2)

hist(r, xlab="HDFC Stock Close Price", prob=TRUE, main="Histogram")

jarque.bera.test(r)

Acf(r, main = "ACF of Returns", lag.max = 100)
Box.test(r, lag=10, type="Ljung")

#qplot(data=hdfcTS, Date, Close, geom="line", main="Time Plot of HDFC Close Price", xlab = "Date", ylab = "Stock Close Price")


#log return
n <- length(hdfcTS)
logaus <- log(hdfcTS[-1]/hdfcTS[-n])
logaus
nrow(hdfcTS)
hdfc$log = logaus

logreturn = data.frame(logaus)
head(logreturn)
logreturn[date] <- hdfc$Date

qplot(data=logreturn, hdfc$Date, Close, geom="line", 
      main="Time Plot of LOG Return of HDFC Close Price", xlab = "Date", ylab = "Stock Close Price")


#diff of series
n = nrow(hdfc)
diffhdfc = diff(hdfc$Close) / hdfc$Close[-n] * 100
diffhdfc

returns = data.frame(hdfc$Date[-n], hdfc$Close[-n], diffhdfc)
returns
colnames(returns)[1] = "date" 
qplot(data=returns, date, diffhdfc, geom="line",  
      main="Time Plot for Rate of Stock Close Price", xlab = "Date", ylab = "% change rate of Close Price")


########################################################################################################################
adf.test(hdfcTS)
Acf(r)
pacf(r) 
eacf(r) #AR 2 or MA 2 mostly(for 252 freq also)

fit1 = auto.arima(r)
fit1         #2,0,3
coeftest(fit1)
autoplot(fit1$residuals)        #check for heteroscedacity if yes then garch
Acf(fit1$residuals)
Box.test(fit1$residuals, lag=10, type = "Ljung-Box")

fit0 = Arima(r, order=c(1, 0, 2), include.drift = F)  #better than auto
fit0
coeftest(fit0)
autoplot(fit0$residuals)
Acf(fit0$residuals)
Box.test(fit0$residuals, lag=10, type = "Ljung-Box")

fit2 = Arima(r, order=c(2, 0, 2))
fit2
coeftest(fit2)
autoplot(fit2$residuals)
Acf(fit2$residuals)
Box.test(fit2$residuals, lag=10, type = "Ljung-Box")

#forecast
autoplot(forecast(fit0), h = 20, xlim = c(2018,2022))

#backtest for fit 1 and 0
backtest(fit1, r, h=1, orig=.8*length(r))#auto  MAPE:1.73
backtest(fit0, r, orig=.8*length(r), h=1)#our pick 1.42 
backtest(fit2, r, orig=.8*length(r), h=1)#        1.94


#test for arch effects
autoplot(fit0$residuals^2)
acf(fit0$residuals^2)

archFit = garch(r, order=c(0, 2))  # Notice that the ARCH parameter is the second one here!
coeftest(archFit) # All significant
autoplot(archFit$residuals, main="ARCH(2) Residuals")  # Getting better
Acf(archFit$residuals^2, main="ACF of ARCH(0, 2) residuals", ylim=c(-.05, .25), lag.max=25)

#do a t test for change in mean
t.test(r)

# Change in overall mean is clearer, but peaks and spikes in variance are not
autoplot(abs(fit0$residuals))   
acf(abs(fit0$residuals))

#garch
fit2=garchFit(~arma(2,2)+garch(1,1),data=r,trace=F)
summary(fit2)
fit2

fit3=garchFit(~arma(1,2)+garch(1,1),data=r,trace=F)
fit3

f = predict(fit3, n.ahead=15) #going to zero
f
mean(r)

#backtest GARCH

length(r)
testLen = floor(length(r) * .98)   # Train on 98% of thes 
testLen

backtestGarch(fit2, r, testLen, 1)  #1.46

#dummy
dummy = ifelse(time(hdfc$Close)<2015, 0,1 )
fit3 = Arima(bef, xreg=cbind(time(bef)), dummy, time(bef)*dummy) #check if time temp sig can check time temp ^2
Arima(v, order=c(1, 0, 2), include.drift = T)

auto.arima(v, allowdrift = TRUE)
auto.arima(hdfcTS)

hdfcTS = ts((hdfc$Close), start = c(2000, 1, 3), end = c(2020, 9, 30), frequency=12)
length(hdfcTS)
bef = subset(hdfcTS, start = 1, end = 180)
aft = subset(hdfcTS, start = 187)


range(time(bef))
autoplot(bef)
autoplot(aft)

v = as.ts(bef,aft)

