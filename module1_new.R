# R code Module 1
##############################################################################
# Import data from a file and perfom summary statistics

library(quantmod)  # or require(quantmod)  <=== load the package "quantmod"
getSymbols("^DJI", from="1985-01-01",to="2018-07-25") # <=== get daily SP500 stock data from Yahoo Finance
DJ=DJI$DJI.Adjusted # <==Column 6 of the object "AAPL" in R
ret_dj=diff(log(DJ))
plot(DJ, col="blue")
plot(ret_dj, col="red")

# Compute annualized standard deviation for every year
# Apply function by calender time using xts time series of price
# apply.daily, weekly,monthly,quarterly,yearly

#apply.monthly(DJ, mean) # average price by month
#apply.monthly(ret_dj, sd) # standard deviation month by month

plot(100*sqrt(252)*apply.yearly(ret_dj, sd), main ="DJ: Yearly Volatility", ylab="Standard Deviation, annualized", col="blue")

library(quantmod)  # or require(quantmod)  <=== load the package "quantmod"
getSymbols("^GSPC", from="2005-01-03",to="2018-07-25") # <=== get daily SP500 stock data from Yahoo Finance
#View(GSPC)
head(GSPC)
dim(GSPC)
SP500=GSPC$GSPC.Adjusted # <==Column 6 of the object "GSPC" in R
ret_sp=diff(log(SP500))
plot(SP500)
plot(ret_sp)
ret=ret_sp


hist(ret,nclass=50) #<== Obtain histogram of returns with 50 bins.
library(fBasics)  #<== Load the package "fBasics"
basicStats(ret)  #<== Compute descriptive statistics of log returns. NOTE that Excess Kurtosis is reported. 
                    #<==(Kurtosis-3)
##<==notice that there is NA 1st observation in returns. Let's remove it
#ret=removeNA(ret)
ret=na.omit(ret)
length(ret) #<===now we have 3412 observations for returns after dropping NA obs

library(xts)
#y=as.xts(ret)
#dates=index(y)
dates=index(ret)#<===extract dates from xts object

ret0=coredata(ret)#<==remove time series property from returns

#Let's compute each desriptive statistics seperately
mean(ret) #mean
var(ret) #variance
stdev(ret) # standard deviation
sd(ret) #standard deviation
skewness(ret) #skewness
kurtosis(ret)  #<==(Kurtosis-3)

# Let's perform text of normality
normalTest(ret0,method='jb') # #<== Perform normality test. JB-test

qqnorm(ret)
qqline(ret)

#Correlogram
q = acf(ret,10) #<== Compute and plot autocorrelation for 10 lags starting from lag 0.
plot(q[1:10]) #<== plot autocorrelation for 10 lags starting from lag 1.
Box.test(ret,lag=10,type="Ljung-Box") #<==Q test

q2 = acf(ret^2,10) #<== Compute and plot autocorrelation of squared returns for 10 lags starting from lag 0.
plot(q2[1:10]) #<== plot autocorrelation for 10 lags starting from lag 1.
Box.test(ret^2,lag=10,type="Ljung-Box") #<==Q test

getSymbols("^GSPC", from="2012-01-03",to="2018-07-25") # <=== get daily SP500 stock data from Yahoo Finance
price_2012_2018=GSPC[,6]
ret1=diff(log(price_2012_2018))
ret1=na.omit(ret1)
plot(ret1)
q = acf(ret1,10) #<== Compute and plot autocorrelation for 10 lags starting from lag 0.
plot(q[1:10]) #<== plot autocorrelation for 10 lags starting from lag 1.
Box.test(ret1,lag=10,type="Ljung-Box") #<==Q test with 10 lags
Box.test(ret1,lag=20,type="Ljung-Box") #<==Q test with 20 lags
Box.test(ret1^2,lag=10,type="Ljung-Box") #<==Q test


# Monthly Historical Standard Deviation (Rolling function)

vol=rollapply(ret,22,sd) # Daily moving average of 22 observations standard deviations
vol_22=100*sqrt(252)*vol #Annualized volatility
plot(vol_22,main ="Historical Monthly Volatility", ylab="Standard Deviation, annualized", col="red")

# Annual Historical Standard Deviation (Rolling function)

vol=rollapply(ret,252,sd) # moving average of 252 observations standard deviations
vol_252=100*sqrt(252)*vol #Annualized volatility
plot(vol_252,main ="Historical Annual Volatility", ylab="Standard Deviation, annualized", col="red")

##Plot two graphs on one plot
vol=cbind(vol_22,vol_252) # combine two time series in a matrix
plot(vol,  col=c(1,2))
title("Volatility plots: Historical Rolling Standard Deviations")
legend("right", inset=0.03, legend=c("monthly", "annual"),pch=1,  col=c(1,2), horiz=F)

##Another way to plot multiple graphs on one plot
plot(vol_22, ylab="volatility", 'col'="2", type='l')
lines(vol_252, 'col'="1", type='l')
title("Volatility plots: Historical Rolling Standard Deviations")
legend("right", inset=0.03, legend=c("annual", "monthly"),pch=1,  col=c(1,2), horiz=F)


# EWMA Riskmetrics model
library("MTS")
m11=EWMAvol(ret, lambda = 0.99) # this is RISKMETRICS model with smoothing .99
#str(m11) #<--find out all contents of object m11
sig2_ewma=m11$Sigma.t #estimated daily variance from object m11
#plot(sig2_ewma,type='l') # <== Plot estimated daily variance from object m11
#plot(m11$return,type='l') # <== Plot daily return from object m11
# Annualized EWMA volatility
s_smooth=sqrt(252*sig2_ewma)*100
library(xts)
vol_s99=as.xts(s_smooth, dates)
plot(vol_s99)
title("Riskmetrics Volatility with lambda=.99")

m12=EWMAvol(ret, lambda = 0.94) #<--smoothing with lambda .94
# Annualized EWMA volatility
s_smooth=sqrt(252*m12$Sigma.t)*100
vol_s94=as.xts(s_smooth, dates)
plot(vol_s94)
title("Riskmetrics Volatility with lambda=.94")


vol_smooth=cbind(vol_s94,vol_s99) # combine two time series in a matrix
plot(vol_smooth,  col=c(1,2))
title("Volatility plots: EWMA Riskmetrics")
legend("right", inset=0.03, legend=c("vol_s94","vol_s99"),pch=1,  col=c(1,2), horiz=F)

#plot(vol_s94, ylab="volatility", 'col'="3", type='l')
#lines(vol_s99,'col'="4")
#title("Volatility plots: EWMA Riskmetrics")
#legend("topright", inset=0, legend=c("vol_s94","vol_s99"),pch=1,  col=c(3,4), horiz=TRUE)


# All Four Plots on one graph
vol_all=cbind(vol,vol_smooth)
plot(vol_all,  col=c(1,2,3,4))
title("4 Volatility plots: Historical and EWMA ")
legend("right", inset=0.03, legend=c("monthly","annual","vol_s94","vol_s99"),pch=1,  col=c(1,2,3,4), horiz=F)

