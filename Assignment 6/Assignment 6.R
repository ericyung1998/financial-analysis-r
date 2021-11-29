########################################################### Problem I ############################################################

setwd("")  # set directory

HIBOR <- read.table("hibor_20_21.csv",header = T, sep=",")
attach(HIBOR)
head(HIBOR)

# 1. Time Series Plot and ACF Plot

par(mfrow=c(1,1))
rate=as.numeric(as.character(HIBOR$rate))
ts.plot(rate,type = "l",xlab = "index",ylab="rate",main = "Overnight HIBOR rate 2020 - 2021")

acf(rate,main="Autocorrelation of HIBOR 2020 - 2021")

# 2. AR Order of Training Data

  # training (index: 1 - 252)
rate_training <- rate[1:252]
  # testing (index: 253 - 257)
rate_testing <- rate[253:257]

  # determine order
par(mfrow=c(1,2))
pacf(rate_training,main="PACF of HIBOR", xlim = c(1,16))
fit_rate <- ar(rate_training)
ts.plot(fit_rate$aic[1:10],xlab=" ",main="AIC of HIBOR")
fit_rate$order

# 3. AR(1) fitted to training data, Vasicek Model

fit_rate <- arima(rate_training,order = c(1,0,0))
fit_rate

# 4. Model Diagnostics

par(mfrow=c(1,1))
tsdiag(fit_rate)
qqnorm(fit_rate$residuals)

# 5. Predictions / Prediction Intervals, Comparison to Test Data

  # prediction for HIBOR
n_testing <- length(rate_testing)
predict_rate <- predict(fit_rate, n_testing)

ts.plot(predict_rate$pred,lty = 2, ylim = c(-.19,0.41)) #
lines(seq(length(rate_training) + 1,length(rate_training) + n_testing),rate_testing)

  # prediction interval for HIBOR
PI_l <- predict_rate$pred - 2*predict_rate$se
PI_r <- predict_rate$pred + 2*predict_rate$se
ts.plot(predict_rate$pred,xlab="index",ylab="rate", ylim = c(-.19,1.5), lty = 2,main="Predictions and prediction intervals for the testing period")
lines(PI_r,col = 'red')
lines(PI_l,col = 'red')
lines(seq(length(rate_training) + 1,length(rate_training) + n_testing),rate_testing)
lines(seq(length(rate_training) + 1,length(rate_training) + n_testing),rep(fit_rate$coef[2],n_testing),col="blue",lty=4)
legend('topleft',legend = c('predicted','true','long-term mean'),lty = c(2,1,4), cex = 0.7, col = c('black','black','blue'))

  # long-term mean for HIBOR
plot(seq(length(rate_training) + 1,length(rate_training) + n_testing),rep(fit_rate$coef[2],n_testing),col="blue",lty=4, main="Long-term mean")
lines(seq(length(rate_training) + 1,length(rate_training) + n_testing),rep(fit_rate$coef[2],n_testing),col="blue",lty=4)

########################################################### Problem II ###########################################################

# 1. White noise and AR(3) Simulation
  
  # white noise
set.seed(14) 
W<-rnorm(1024)

  # AR(3)
X<-arima.sim(1024,model=list(ar=0.07,0.02,0.3),n.start=1,start.innov=c(0), innov = W)
ts.plot(X)

# 2. AR fitting and applications

  # AR order 9 w/ AIC
par(mfrow=c(1,1))
fit1<-ar(X)
ts.plot(fit1$aic[1:9],xlab=" ",main="AIC Plot")

  # fit AR(1)
fit2<-arima(X,order=c(1,0,0))
fit2
tsdiag(fit2)

  # forecasting T+16
pred <- predict(fit2, n.ahead = 16)
PI_l <- pred$pred - 2*pred$se
PI_r <- pred$pred + 2*pred$se
ts.plot(pred$pred, ylim = c(-2.5,2.6), lty = 2, main="Forecasting with AR(1) at T+16")
lines(PI_r,col = 'red')
lines(PI_l,col = 'red')
legend('topleft',legend = c('predicted','95% CI'),lty = c(2,1), cex = 0.7, col = c('black', 'red'))

pred$pred

# 3. White noise and ARMA(3,4) Simulation

  # white noise
set.seed(14) 
W<-rnorm(1024)

  # ARMA(3,4)
X2 <- arima.sim(n = 1000, list(ar = c(0.07,0.02,0.3), ma = c(0.4,0.3,0.2,0.05)), innov = W)
ts.plot(X2)

# 4. AR fitting and applications

  # AR order 9 w/ AIC
fit3<-ar(X2)
ts.plot(fit3$aic[1:9],xlab=" ",main="AIC Plot")

  # fit AR(4)
fit4<-arima(X2,order=c(4,0,0))
fit4
tsdiag(fit2)

  # forecasting T+16
pred <- predict(fit4, n.ahead = 16)
PI_l <- pred$pred - 2*pred$se
PI_r <- pred$pred + 2*pred$se
ts.plot(pred$pred, ylim = c(-3,3), lty = 2, main="Forecasting with AR(4) at T+16")
lines(PI_r,col = 'red')
lines(PI_l,col = 'red')
legend('topleft',legend = c('predicted','95% CI'),lty = c(2,1), cex = 0.7, col = c('black', 'red'))

pred$pred

# 5. ARMA Fitting and applications

  # AR(3) forced fitting
fit5 <- arima(X2,order=c(3,0,0))

  # estimated residuals
plot(fit5$resid, main="AR(3) Estimated Residuals")
acf(fit5$resid,main="ACF of AR(3) Residuals")

  # MA Fit
MAfit1<-arima(fit5$resid,order=c(0,0,1))
MAfit2<-arima(fit5$resid,order=c(0,0,2))
MAfit3<-arima(fit5$resid,order=c(0,0,3))
MAfit4<-arima(fit5$resid,order=c(0,0,4))
MAfit5<-arima(fit5$resid,order=c(0,0,5))
round(c(MAfit1$aic,MAfit2$aic,MAfit3$aic,MAfit4$aic,MAfit5$aic),3)

  # forecasting T+16

fit6 <- arima(X2,order=c(3,0,5))
fit6
pred <- predict(fit6, n.ahead = 16)
PI_l <- pred$pred - 2*pred$se
PI_r <- pred$pred + 2*pred$se
ts.plot(pred$pred, ylim = c(-3,3), lty = 2, main="Forecasting with ARMA(3,5) at T+16")
lines(PI_r,col = 'red')
lines(PI_l,col = 'red')
legend('topleft',legend = c('predicted','95% CI'),lty = c(2,1), cex = 0.7, col = c('black', 'red'))

pred$pred

##################################################################################################################################

# Clear plots
#dev.off()

# Clear variables
#rm(list = ls())

# Clear console
#cat("\014")