#####-------------------------------------------------------
##### Business Analytics 
##### Dr. Jiun-Yu Yu
##### Department of Business Administration
##### National Taiwan University
##### Lecture 3, 25 Sep 2018
#####-------------------------------------------------------

HOMES3 <- read.table("d1803_HOMES3.txt", header=TRUE) # Read data for analysis
attach(HOMES3)

pairs(HOMES3)

# Summary of regression on data HOMES3, Y~X1+X2
hm3.lm <- lm(Y~X1+X2)
summary(hm3.lm)

# Beta coefficients 	# Note: such coef does not exist for intercept
hm3.lm$coef["X1"]		# hm3.lm$coef[2]
beta.X1 <- hm3.lm$coef["X1"]*sd(X1)/sd(Y)
beta.X2 <- hm3.lm$coef["X2"]*sd(X2)/sd(Y)

# Summary of regression on data HOMES3, Y~X1
hm1.lm <- lm(Y~X1)
summary(hm1.lm)

detach()

# Summary of regression on data SHIPDEPT, Y~X1+X2+X3+X4
SHIPDEPT <- read.table("d1803_SHIPDEPT.txt", header=TRUE)
ship.full <- lm(data=SHIPDEPT,Y~X1+X2+X3+X4)	# complete model
summary(ship.full)

ship.13 <- lm(data=SHIPDEPT,Y~X1+X3) 		# reduced model
summary(ship.13)


## ------------------------------------------------
## Discussions on Correlation 

# Summary of regression on data SALES2 		(Slides p.3-20 ~ 3-22)
SALES2 <- read.table("d1803_SALES2.txt", header=TRUE)
fitSALES2 <- lm(data=SALES2, Y~X1+X2)
	anova(fitSALES2) 
summary(fitSALES2)

attach(SALES2)
plot(X1, Y, xlab="X1=advertising (in $m)", ylab="Y=sales (in $m)")
plot(X1, Y, type="n",xlab="X1=advertising (in $m)", ylab="Y=sales (in $m)")
for(i in 1:12) points(X1[i], Y[i], pch=as.character(X2[i]))
detach()

# Summary of regression on data SALES3		(Slides p.3-23 ~ 3-24)
SALES3 <- read.table("d1803_SALES3.txt", header=TRUE)
fitSALES3 <- lm(data=SALES3, Y~X1+X2)
	anova(fitSALES3) 
summary(fitSALES3)

fitSALES3p2 <- lm(data=SALES3, Y~X2) 
summary(fitSALES3p2)

pairs(cbind(Y=SALES3[,1], X1=SALES3[,2], X2=SALES3[,3]))


## ------------------------------------------------
## Global Usefulness Test & Nested Model Test

# HOMES3 data
summary(hm3.lm)$fstatistic
(qf(0.95, 2, 3))		# critical value, n=6, k=2
(1-pf(51.434, 2, 3))	# p-value

hm0.lm <- lm(Y~1, data=HOMES3)
anova(hm0.lm, hm3.lm)		# ANOVA table p. 3-28
anova(hm3.lm)

# SHIPDEPT data
summary(ship.full)$fstatistic
(qf(0.95, 4, 15))		# critical value, n=20, k=4
(1-pf(17.035, 4, 15))	# p-value

ship.0 <- lm(Y~1, data=SHIPDEPT)
anova(ship.0, ship.full)	# ANOVA table p. 32, upper (C)
anova(ship.0, ship.13)		# ANOVA table p. 32, lower (R)
anova(ship.13, ship.full)	# Nested F-stat p.32 & 34
	# anova(ship.full), anova(ship.13)

(qf(0.95, 2, 15))
(1-pf(0.472, 2 ,15))


## ------------------------------------------------
## Individual t-test

# SHIPDEPT data
summary(ship.13)



## ------------------------------------------------
## Confidence Intervals for regression coefficients & for E(Y) 
## Prediction Interval for a single Y

confint(ship.13)

xnew <- data.frame(X1=6, X3=20)
predict(ship.13, xnew, interval="confidence", level=0.95)
predict(ship.13, xnew, interval="prediction", level=0.95)



