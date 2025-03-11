# Moduel 2: CAPM and Multifactor models

### You should use your working directory to load data file from the case study ###
###Click: Session-->Set Working Directory-->To Source File Location

da = read.csv("multifactor.csv",header=T)  #<== Load data with header into R
dim(da)  #<== Check dimension of the data (row = sample size, col = variables)
head(da)  # <== Print out the first 6 rows of the data object "da".
tail(da) #<== Print out the last 6 rows of the data object "da".

market=da$Market
tbills=da$TBills
hml=da$HML
smb=da$SMB
mom=da$MOM
rm=market-tbills #excess return for the market

# CAPM and Multifactor for 4 mutual funds: GLCGX, TRBCX, DTMVX, DVPEX
# GLCGX fund
r_glcgx=da$GLCGX-tbills # excess return for GLCGX

#require(graphics)
m1=lm(r_glcgx~rm)  # CAPM MODEL with one regressor
summary(m1)

m2=lm(r_glcgx~rm+hml+smb+mom) #Multifactor model
summary(m2)

# TRCBX fund
r_trbcx=da$TRBCX-tbills # excess return for TRBCX

m3=lm(r_trbcx~rm)  # CAPM MODEL with one regressor
summary(m3)

m4=lm(r_trbcx~rm+hml+smb+mom) #Multifactor model
summary(m4)
