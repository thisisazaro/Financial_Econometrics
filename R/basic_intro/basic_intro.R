
# Install packages
#install.packages("tseries")
#install.packages("ggplot2")
#install.packages("quantmod")
#install.packages("MTS")
#install.packages("readxl")

rm(list = ls()) # clear memory

library(quantmod)  # or require(quantmod)  <=== load the package "quantmod"

getSymbols("AAPL") # <=== get daily Apple stock data from Yahoo Finance
dim(AAPL)         # <=== find the size of the data downloaded
head(AAPL,10)        # <=== show the first 6 rows of data
tail(AAPL)        # <=== show the last 6 rows of data
chartSeries(AAPL) # <=== plot Apple daily closing stock prices with trading volume
# <== Daily closing prices do not adjusted for stock split. You can use adjusted closing price.
chartSeries(AAPL[,6]) # <== Column 6 of the object "AAPL" in R.
chartSeries(AAPL[,6],theme="white") # <== Same as the previous command, but use "white" background for the plot.

getSymbols("AAPL",from="2005-01-03",to="2015-08-25") # <== specify the data span
price=AAPL[,6]
price=AAPL$AAPL.Adjusted

ret=diff(log(price)) # log return
ret=diff(log(AAPL$AAPL.Adjusted))

plot(ret)
getSymbols("UNRATE",src="FRED") #<== Load U.S. monthly unemplyment rate from Federal Reserve Bank of St Louis.
#<== src stands for "source", FRED stands for Federal Reserve Economic Data.
plot(UNRATE)
chartSeries(UNRATE) #<== plot the U.S. monthly unemployment rate
getSymbols("DEXUSEU",src="FRED") #<== Load Dollar verus Euro daily exchange rates from FRED.
chartSeries(DEXUSEU) #<== plot the daily dollar-euro exchange rates.
plot(DEXUSEU)
getSymbols("^VIX") #<== load daily VIX index
head(VIX) 
tail(VIX,10)# <=== show the last 10 rows of data
chartSeries(VIX)
plot(VIX[,6])
getSymbols("^TNX") #<== load interest rates (CBOE 10-year Treasures Notes)
head(TNX)  
chartSeries(TNX)
chartSeries(TNX,theme="white",TA=NULL) # Obtain plot without volume.
plot(TNX[,6])



###### Load Data from csv and xls file
rm(list = ls()) # clear memory

### You should use your working directory where the data file is stored###

###Or get working directory and setwd as above but replace the path in setwd()
getwd()

# import AAPL.csv data from a file
AAPL=read.csv("R/basic_intro/AAPL.csv")
price=AAPL$Adj.Close
plot(price, type='l', col='blue')

# Other ways to load data: click on Import Dataset in Environment

View(AAPL)
head(AAPL)

#Create time series
library(xts)
date=as.Date(AAPL$Date)
price=xts(price,date) # the first argument is a vector or matrix of time series, the second argument is vector of dates
price=xts(AAPL$Adj.Close,date) # the first argument is a vector or matrix of time series, the second argument is vector of dates
plot(price,col='blue')

# import AAPL.xls data from a file
library(readxl)
AAPL=read_excel("AAPL.xls")
dim(AAPL)

price=AAPL$AdjClose
plot(price, type='l', col='blue')

#Create time series
library(xts)
date=as.Date(AAPL$Date)
price=xts(price,date) # the first argument is a vector or matrix of time series, the second argument is vector of dates
price_AAPL=xts(AAPL$AdjClose,date) # the first argument is a vector or matrix of time series, the second argument is vector of dates
plot(price_AAPL,col='blue')

ret_AAPL=diff(log(price_AAPL))
plot(ret_AAPL, col='red')




