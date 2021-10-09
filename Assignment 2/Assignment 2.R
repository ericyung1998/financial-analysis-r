########################################################### Problem I ############################################################

setwd("")  # set directory

library("evir")
source("gpd_code.R")

# 1. vector DHSIRET - daily raw returns

HSI <- read.table("DHSI.csv",header = T, sep=",")

DHSI<-(HSI$Close)
n<-length(DHSI)
DHSIRET <- diff(DHSI)/DHSI[1:(n-1)] # compute raw returns (rate of return)

mu_DHSIRET <- mean(DHSIRET) # mean
sd_DHSIRET <- sd(DHSIRET) # standard deviation

round(c(mu_DHSIRET, sd_DHSIRET),4)

# 2. exploratory data analysis

par(mfrow = c(1, 1))
eda.shape(DHSIRET)

# 3. GPD fitted - returns

SHAPE.XI=TRUE
par(mfrow=c(1,2))
shape.plot(DHSIRET,tail="upper")
shape.plot(DHSIRET,tail="lower")

  # fit 

DHSIRET.est <- gpd.tail(DHSIRET, upper=0.023,lower=-0.023)

  # xi

DHSIRET.est[[1]]$par.ests[1] # upper    
DHSIRET.est[[2]]$par.ests[1] # lower

  # goodness of fit

par(mfrow=c(1,2))

tailplot(DHSIRET.est[[1]]) # upper tail
title("Plot of upper tail in log - log scale")
tailplot(DHSIRET.est[[2]]) # lower tail
title("Plot of lower tail in log - log scale")

# 4. Generated S_DHSIRET with QQ Plot

par(mfrow=c(1,1))

S_DHSIRET=gpd.2q(runif(100000), DHSIRET.est)
qqplot(DHSIRET,S_DHSIRET,main = "Q-Q Plot of DHSIRET vs Simulated")
abline(a=0,b=1)

# 5. VaR at 0.005, under conditions:

q<-0.005

  # a. empiracle quantile

VaR_emp <- - quantile(DHSIRET,q)

  # b. normally distributed

VaR_N <- - qnorm(q,mean(DHSIRET),sd(DHSIRET))

  # c. fitted GPD distribution

xi_upper = DHSIRET.est[[1]]$par.ests[1]
threshold_upper = DHSIRET.est[[1]]$threshold
lambda_upper = DHSIRET.est[[1]]$par.ests[2]

xi_lower = DHSIRET.est[[2]]$par.ests[1]
threshold_lower = DHSIRET.est[[2]]$threshold
lambda_lower = DHSIRET.est[[2]]$par.ests[2]

Cond_p=length(DHSIRET[-DHSIRET>threshold_lower])/length(DHSIRET)

VaR_GPD <- qgpd(1-0.005/Cond_p,xi_lower,threshold_lower,lambda_lower)

  # d. Monte Carlo S_DHSIRET

VaR_Monte <- - quantile(S_DHSIRET,q) 

  # e. comparison

VaR_Ratio <-  (VaR_N - VaR_GPD)/VaR_GPD

round(c(VaR_emp,VaR_N,VaR_GPD,VaR_Monte,VaR_Ratio),3)

# 6. Expected Shortfall at 0.005, under conditions:

  # a. empiracle conditional mean

ES_emp <- mean(- DHSIRET[- DHSIRET > VaR_emp])

  # b. normally distributed

N<-100000
X<-rnorm(N,mu_DHSIRET,sd_DHSIRET)
ES_N <- mean( - X[- X > VaR_N])

  # c. fitted GPD distribution

ES_GPD <- mean(- S_DHSIRET[- S_DHSIRET > VaR_GPD])

  # e. comparison

round(c(ES_emp, ES_N, ES_GPD),3)

########################################################### Problem II ###########################################################

rm(list = ls())
dev.off()
cat("\014")

# pg. 187 problem 3.7 - Carmona

UTILITIES <- read.csv("UTILITIES.csv")
X <- UTILITIES[ , 1]
Y <- UTILITIES[ , 2]

# 1. means, standard deviations, correlation coef of X and Y

mu_X <- mean(X)
sd_X <- sd(X)

mu_Y <- mean(Y)
sd_Y <- sd(Y)

cor_XY <- cor(X,Y)

round(c(mu_X,sd_X,mu_Y,sd_Y,cor_XY),4)

# 2. 2-percentiles of variables X + Y and X - Y, empirical estimate of percentiles

# X + Y

qnorm(0.02, mean=(mu_X+mu_Y), sd=sqrt(sd_X^2+sd_Y^2))

# X - Y

qnorm(0.02, mean=(mu_X-mu_Y), sd=sqrt(sd_X^2+sd_Y^2))

########################################################### Problem III ##########################################################

rm(list = ls())
dev.off()
cat("\014")

# 1. correlation between X and X^2 ~N(0,1)

X <- rnorm(1024)
Y <- X^2

par(mfrow=c(1,1))
plot(X,Y) 
cor(X,Y)

# 2. correlation between X and X^2 ~N(3,1)

XX <- rnorm(1024, 3, 1)
YY <- XX^2

plot(XX,YY) 
cor(XX,YY)

# 3. correlation coefficient of 2 random variables (A~N(mu,sd) and B = A^2)

  # n = 1
A <- rnorm(1)
B = A^2
cor(A,B, method = "s")

  # n = 100000
A <- rnorm(10000)
B = A^2
cor(A,B, method = "s")

# 4. mu = 0 - correlation and independence of A and B

plot(A,B)

##################################################################################################################################

# Clear plots
#dev.off()

# Clear variables
#rm(list = ls())

# Clear console
#cat("\014")


