# Moduel 2: CAPM and Multifactor models

### You should use your working directory to load data file from the case study ###
###Click: Session-->Set Working Directory-->To Source File Location

da = read.csv("R/CaseStudy/multifactor.csv",header=T)  #<== Load data with header into R
dim(da)  #<== Check dimension of the data (row = sample size, col = variables)
head(da)  # <== Print out the first 6 rows of the data object "da".
tail(da) #<== Print out the last 6 rows of the data object "da".

market=da$Market
tbills=da$TBills
hml=da$HML
smb=da$SMB
mom=da$MOM
rm=market-tbills #excess return for the market


####Correlation between factors
cor(cbind(rm,hml,smb,mom))

# CAPM and Multifactor for 4 mutual funds: GLCGX, TRBCX, DTMVX, DVPEX
# GLCGX fund
r_glcgx=da$GLCGX-tbills # excess return for GLCGX

#####Scater Plot
plot(as.data.frame(cbind(rm,r_glcgx))) #scatterplot
title("GLCGX and Market Excess Returns")

#####Scater Plot with fitted regression line
library(ggplot2)
ggplot(aes(x=rm,y=r_glcgx), data=da)+
  geom_point()+
  geom_smooth(method="lm",size=1.1,se=F)+
  ylim(-0.2,0.2)

#Linear regression 
m1=lm(r_glcgx~rm)  # CAPM MODEL with one regressor
summary(m1)

m2=lm(r_glcgx~rm+hml+smb+mom) #Multifactor model
summary(m2)

plot(cbind(rm,r_glcgx)) # scatter plot
abline(m1,col = "blue") # regression line
plot(resid(m1))
hist(resid(m1))

# TRCBX fund
r_trbcx=da$TRBCX-tbills # excess return for TRBCX

m3=lm(r_trbcx~rm)  # CAPM MODEL with one regressor
summary(m3)

m4=lm(r_trbcx~rm+hml+smb+mom) #Multifactor model
summary(m4)


