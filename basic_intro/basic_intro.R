#R commands basics
#"<==" denotes explanation of the command.

## Install R and RStudio on your computer as demonstrated in class ##
## double click on RStudio to start R ##


###Install packages
install.packages("tseries")
install.packages("ggplot2")
install.packages("quantmod")
install.packages("MTS")
install.packages("readxl")

##############################################################################
rm(list = ls()) # clear memory

#Basic operations
# Scalar Variables
x = 10 #assign 10 to x
x<-10
x    		
#10			
y=2
x/y
#5
x*y
#20

# Vectors
y = c(1,3,5,7,9)
y
# 1 3 5 7 9
y[3] 
#5
dim(y)
#NULL		
length(y)	
#[1] 5

x <- c(1,3,2,5)
x
x = c(1,6,2)
x


# Matrices

y = matrix(nrow=2,ncol=3) 
y	
dim(y)
# 2 3
? dim
y = matrix(c(1,2,3),3,1) 
y  

y = matrix(c(1,2,3, 4,5,6),3,2) 
y  
y = matrix(seq(1:6),3,2) 
y  

# Sequences

seq(1:10)
#[1]  1  2  3  4  5  6  7  8  9 10
seq(from=1, to=10, by=2)
#[1]  1 3 5 7 9
seq(from=1, to=10, length=5)
#[1]  1.00  3.25  5.50  7.75 10.00

##############################################################################
#Get data from online sources using quatmod library
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

# Rt = (pt + dt - p_t-1 ) / p_t-1
# log_return = log(pt + dt) - log(p_t-1)

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
###Click: Session-->Set Working Directory-->To Source File Location
#setwd("~/Desktop/PRMIA/code/basic_intro")

###Or get working directory and setwd as above but replace the path in setwd()
getwd()



# import AAPL.csv data from a file
AAPL=read.csv("AAPL.csv")
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
