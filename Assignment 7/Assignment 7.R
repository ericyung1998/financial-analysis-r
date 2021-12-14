########################################################## Problem III ###########################################################

setwd("")  # set directory

HSI <- read.table("HSI_2021.csv",header = T, sep=",")
attach(HSI)
head(HSI)

# 1. Time Series Plot (Close and Returns)

par(mfrow=c(1,2))

DHSI <- HSI$Close
DHSIR <- diff(log(DHSI))

ts.plot(DHSI,main="HSI in 2021")
ts.plot(DHSIR,main="HSI Log-Returns in 2021")

# 2. ACF Returns / Squared Returns

acf(DHSIR, main = "ACF Plof of HSI Returns") 
acf((DHSIR)^2, main = "ACF Plof of HSI Returns^2")

# 3. ARCH Order

pacf((DHSIR)^2,main="PACF Plot of DHSIR^2")
DHSIR_sq_ar <- ar((DHSIR)^2)
ts.plot(DHSIR_sq_ar$aic[1:15],main="AIC Plot of DHSIR^2")
DHSIR_sq_ar$order

# 4. GARCH(1,1) Fit

library(fGarch)

DHSIR_training = DHSIR[1:214]
DHSIR_GARCH <- garchFit( ~ garch(1,1), data = DHSIR_training, trace = FALSE)
summary(DHSIR_GARCH)

# 5. GARCH(1,1) without mean Fit

DHSIR_GARCH <- garchFit( ~ garch(1,1), data = DHSIR_training, trace = FALSE, include.mean = FALSE)

  # Model Diagnostics
summary(DHSIR_GARCH)

std.res <- (DHSIR_training)/DHSIR_GARCH@sigma.t
acf(std.res)
qqnorm(std.res)
abline(0,1,col="red")

# 6. Volatility Forecast (T+1)

DHSIR_GARCH@fit$coef

omega <- DHSIR_GARCH@fit$coef[1]
alpha <- DHSIR_GARCH@fit$coef[2]
beta <- DHSIR_GARCH@fit$coef[3]

n <- length(DHSIR_training)
volatility_forecast_1 <- omega + alpha* DHSIR[n]^2+ beta*DHSIR_GARCH@sigma.t[n]^2

prediction <- predict(DHSIR_GARCH, n.ahead = 1)
sd_forecast <- prediction$standardDeviation
volatility_forecast_2 <- sd_forecast^2
c(volatility_forecast_1, volatility_forecast_2)

# 7. 1%-VaR / 1%-Expected Shortfall

mu <- 0

  # 1%-VaR forecast under normal for the next day
VaR_normal <- -qnorm(0.01, mu, sd_forecast)

  # 1%-ES forecast under normal
N<-100000
X<-rnorm(N,mu,sd_forecast)
ES_normal <- mean( - X[- X > VaR_normal])

c(VaR_normal,ES_normal)

##################################################################################################################################

# Clear plots
#dev.off()

# Clear variables
#rm(list = ls())

# Clear console
#cat("\014")