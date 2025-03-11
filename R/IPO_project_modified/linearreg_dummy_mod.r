# Module 2: Dummy Variables

### You should use your working directory ###

### IPO/SEO example
equity = read.csv("R/IPO_project_modified/equity.csv")   

# Plot Value against SEO dummy variable
plot(value ~ seo, data = equity)

# More fancy plot library ggplot2 needs to be installed
library("ggplot2")
ggplot(aes(x=seo,y=value), data=equity)+geom_point()
  

# t-test for explanatory variables by groups

t.test(value ~ seo, data = equity) # t = -3.4843, df = 244.6, p-value = 0.0005845 <---this means significant difference in means with on average 
#we find on average higher Value  for SEO=1  firms compared to SEO=0 (IPO firms)
#mean in group 0 (SEO) mean in group 1 (IPO)
#191.7952        829.5748 

t.test(income ~ seo, data = equity)
#t = -3.0945, df = 265.31, p-value = 0.002182
#we find on average higher income for SEO=1  firms compared to SEO=0 (IPO firms)
#  mean in group 0 mean in group 1 
#1.598795       15.455752 

with(equity, t.test(assets[seo == 0], assets[seo == 1])) 
# not significant difference in the means of assets of two groups

# Regression
m1 = lm(value~seo+debt+sales+income+assets+seo:debt+seo:sales+seo:income+seo:assets,data=equity)
summary(m1)

# limit value of firms to be below 10000 to remove outlier
equity1=equity[equity$value<10000,] # removing outliers
summary(equity$value)
summary(equity1$value)

# Regresion without an outlier
m2 = lm(value~seo+debt+sales+income+assets+seo:debt+seo:sales+seo:income+seo:assets,data=equity1)
summary(m2)

# limit value of firms to be below 10000 and run a regression of Value on Sales, show regression line. Here we see heteroscedasticity as the variance 
#of observations increases with Sales
ggplot(aes(x=sales,y=value), data=equity)+
  geom_point()+
  geom_smooth(method="lm",size=1.1,se=F)+
  ylim(0,10000)

# Heteroscedasticity correction

library("sandwich")
VCOV=vcovHC(m2, type="HC") #<--type = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5")
library("lmtest")
coeftest(m2, vcov. = VCOV) #<--print out corrected standard errors and tstats

# Explore multicolinearity correlation in X variables
attach(equity1)
X=cbind(debt,sales,income,assets)
cor(X)
equity1$seo_debt=seo*debt
equity1$se_sales=seo*sales
equity1$se_income=seo*income
equity1$se_assets=seo*assets
cor(equity1)
# Close to 1 correlation for debt and seo*debt, sales and seo*sales, income and seo*income,.... 
# This is almost perfect multicollinearity, thus, estimated coefficients might be unstable.
# may produce wrong coefficient sign in the regression for these variables.
detach(equity1)

str(equity1)

equity_ipo=equity1[seo==0,]
attach(equity_ipo)
X_ipo=cbind(debt,sales,income,assets)
cor(X_ipo)
#cor(X_ipo)
#         debt     sales     income    assets
#debt    1.0000000 0.3858773 -0.3341370 0.8319547
#sales   0.3858773 1.0000000  0.3909878 0.7191417
#income -0.3341370 0.3909878  1.0000000 0.1084425
#assets  0.8319547 0.7191417  0.1084425 1.0000000

# High correlation between assets and debt for IPO firms
detach(equity_ipo)

