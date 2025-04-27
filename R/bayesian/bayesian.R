################################################
################# Bayesian Analysis#############
#https://cran.r-project.org/web/views/Bayesian.html
################################################

##############
# Bayesian Linear Regression
##############

###Load Data
library(ggplot2)
da = read.csv("multifactor.csv",header=T)  #<== Load data with header into R
dim(da)  #<== Check dimension of the data (row = sample size, col = variables)
head(da)  # <== Print out the first 6 rows of the data object "da".
tail(da) #<== Print out the last 6 rows of the data object "da".
library(lubridate)
JANDUM = as.integer(month(da$Date) == 1) # Create a JANUARY Dummy Variable
attach(da)
########GLCGX Fund##########
r_glcgx=GLCGX-TBills # excess return for GLCGX
rm=Market-TBills #excess return for the market


###Check Classical Linear Regression
require(graphics)
m1=lm(r_glcgx~rm)  # CAPM MODEL with one regressor
summary(m1)
m2=lm(r_glcgx~rm+HML+SMB+MOM) #Multifactor model
summary(m2)

#January Effect
m3=lm(r_glcgx~rm+JANDUM)  # CAPM MODEL with January Effect
summary(m3)

### Bayesian MCMC for Linear Regression
library(MCMCpack)
?MCMCregress

posterior1=MCMCregress(r_glcgx~rm,data=da) # CAPM MODEL with one regressor
summary(posterior1)
plot(posterior1)
raftery.diag(posterior1)

posterior2=MCMCregress(r_glcgx~rm+HML+SMB+MOM,data=da) #Multifactor model
summary(posterior2)
plot(posterior2)
raftery.diag(posterior2)


##############
# Bayes GARCH
##############

library(bayesGARCH)
## LOAD DATA
library(quantmod)  # or require(quantmod)  <=== load the package "quantmod"
getSymbols("^GSPC", from="2005-01-03",to="2018-07-25") # <=== get daily SP500 stock data from Yahoo Finance
SP500=GSPC$GSPC.Adjusted # <==Column 6 of the object "GSPC" in R
ret_sp=diff(log(SP500))*100
plot(SP500)
plot(ret_sp)
y=ret_sp # Name y for returns of SP500
y=y[-1] #remove the first observation which is NA
length(y) #<===now we have 3412 observations for returns after dropping NA obs

library(xts)
dates=index(y)#<===extract dates from xts object

y0=coredata(y)#<==remove time series property from returns
y0=y0[1:3412]

## RUN THE SAMPLER (2 chains)
## NB: CONSIDER LARGER l.chain!
MCMC= bayesGARCH(y0, control = list(n.chain = 2, l.chain = 5000))
## MCMC ANALYSIS (using coda)
plot(MCMC)
autocorr.diag(MCMC)
gelman.diag(MCMC)
1-rejectionRate(MCMC)
## FORM THE POSTERIOR SAMPLE
smpl= formSmpl(MCMC, l.bi = 1000, batch.size=4)
## POSTERIOR STATISTICS
summary(smpl)
smpl= as.matrix(smpl)
pairs(smpl)


library(bayesDccGarch)
### GARCH(1,1) ###
MCMC2 = bayesDccGarch(y)
summary(MCMC2$MC)
plotVol(y, MCMC2$H, "SP500") # built in the package plotVol function

vol_bgarch=sqrt(252*MCMC2$H) # convert to annualized volatility

#Plot volatility using xts time series object
vol_bgarch=as.xts(vol_bgarch, dates)
plot(vol_bgarch,xlab='year',ylab='volatility',type='l') # notice that initial values have effect for vol for the few obs in the beginning of the sample

########################
# Stochastic Volatility#
########################
library("stochvol")

set.seed(123)

library(quantmod)  # or require(quantmod)  <=== load the package "quantmod"
getSymbols("^GSPC", from="2005-01-03",to="2018-07-25") # <=== get daily SP500 stock data from Yahoo Finance

SP500=GSPC$GSPC.Adjusted # <==Column 6 of the object "GSPC" in R
ret_sp=100*logret(SP500, demean=TRUE) #<--this log differencing function (in library stochvol) removes missing values as well as allows to demean returns if TRUE
dates=index(ret_sp)#<===extract dates from xts object
y=ret_sp


par(mfrow = c(2, 1), mar = c(1.9, 1.9, 1.9, 0.5), mgp = c(2, 0.6, 0))
plot(SP500, type = "l",main = "SP500")
plot(y, type = "l", main = "Demeaned log returns")
#if returns are in %
res = svsample(y, priormu = c(0, 10), priorphi = c(10, 1), priorsigma = 0.2)
# if returns are in decimals mu is -10 since exp(-10)=.00005 will correpsond to constant term in variance equation
#res = svsample(y, priormu = c(-10, 1), priorphi = c(20, 1.1), priorsigma = 0.1)

summary(res, showlatent = FALSE)
volplot(res, dates = dates) # built in the package median volatility graph with 5% and 95% quantiles
volplot(res, forecast = 100, dates = dates) # 100 day forecast

vol_SV=colMeans(res$latent) # posterior mean (column average) of logvariance matrix (10,000 draws by 3412 obs)
vol_SV=sqrt(252)*exp(vol_SV/2) # convert log variance to annualized volatility
length(vol_SV)

#Plot volatility using xts time series object
par(mfrow = c(1, 1))
library(xts)
vol_SV=as.xts(vol_SV, dates)
plot(vol_SV,xlab='year',ylab='volatility',type='l')




#?volplot
#res = updatesummary(res, quantiles = c(0.01, 0.1, 0.5, 0.9, 0.99))
#volplot(res, forecast = 100, dates = dates)

#Trace Plots
par(mfrow = c(3, 1))
paratraceplot(res)

par(mfrow = c(1, 3))
paradensplot(res, showobs = FALSE)
plot(res, showobs = FALSE)
myresid = resid(res)
plot(myresid, y, type='l')

########################
# VIX versus other models#
########################
library(quantmod)  # or require(quantmod)  <=== load the package "quantmod"
getSymbols("^VIX", from="2005-01-03",to="2018-07-25") #<== load daily VIX index
VIX_index=VIX$VIX.Adjusted

VIX_index=VIX_index[-1] # remove the first observation to make it compatible with GARCH and SV volatility sample
length(VIX_index)
par(mfcol=c(1,1))
plot(VIX_index, type = "l",main = "VIX")


# Plot GARCH-tdist, Stock Volatility and VIX on the same Graph
par(mfrow = c(1, 1))
vol_all=cbind(vol_bgarch,vol_SV,VIX_index)
head(vol_all,20)
vol_all=vol_all[11:3412,] # remove the first 10 obs to avoid effect of initial values
plot(vol_all,  col=c(1,2,3), main = "Volatility Models")
title("Bayesian GARCH-tdist, Stochastic Volatility and VIX ")
legend("right", inset=0.03, legend=c("GARCH-tdist","SV","VIX"),pch=1,  col=c(1,2,3), horiz=F)

