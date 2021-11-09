########################################################### Problem I ############################################################

setwd("")  # set directory

Gas_Future <- read.table("GAS_Future_2021_Oct15.csv",header = T, sep=",")
attach(Gas_Future)
head(Gas_Future)

# 1.1 Polynomial LS Regression

  # empirical curve
Gas_Future <- SETTLE[1:32]
Delivery_month <- seq(1,32)
plot(Delivery_month, Gas_Future, xlab="Delivery Month", main = "Gas Future Contract Values on Oct 15, 2021")

  # polynomial regression

lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, 3))),lty = 1, col = 1,lwd=1.5)
lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, 6))),lty = 2, col = 2,lwd=1.5)
lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, 9))),lty = 4, col = 4,lwd=1.5)
lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, 12))),lty = 6, col = 6,lwd=1.5)

legend("topright", c("Degree = 3", "Degree = 6", "Degree = 9","Degree = 12"), lty = c(1,2,4,6), col = c(1,2,4,6), lwd=c(1.5,1.5,1.5,1.5), cex = 0.8)

# 1.2 ANOVA and R-sqr (degree p)

  # ANOVA
fit_12 <- lm(Gas_Future ~ poly(Delivery_month, 12, raw = TRUE))
fit_9 <- lm(Gas_Future ~ poly(Delivery_month, 9, raw = TRUE))
anova(fit_9, fit_12)

fit_8 <- lm(Gas_Future ~ poly(Delivery_month, 8, raw = TRUE))
anova(fit_8, fit_9)

fit_7 <- lm(Gas_Future ~ poly(Delivery_month, 7, raw = TRUE))
anova(fit_7, fit_8)

fit_6 <- lm(Gas_Future ~ poly(Delivery_month, 6, raw = TRUE))
anova(fit_6, fit_7)

  # R-sqr
R2 <- rep(0,23)
for (i in 1:23)
{
  fit <- lm(Gas_Future ~ poly(Delivery_month,i))
  R2[i] <- summary.lm(fit)$r.squared
}

plot(R2,xlab="p",main="R^2 vs degree p")

# 1.3 Prediction Future Prices (t+6)

p <- 7
I <- seq(32,38)
T<-length(I)
coef <- lm(Gas_Future ~ poly(Delivery_month,  degree = p, raw = TRUE))$coef
pred <- rep(0,T)
for (i in 1:T)
{
  pred[i] <- sum(coef * I[i]^(0:p))
}

plot(Gas_Future,xlim=c(0,42),ylim=c(2,6),xlab="Delivery Month",  main = "Polynomial Gas Future Price Prediction")
lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, p))), lty = 2, col = 2,lwd=1.5)
lines(I,pred, lty = 4, col = 4,lwd=1.5)
legend("bottomleft", c("Fitted", "Predicted"), lty = c(2,2), col = c(2,4), lwd=c(1.5,1.5))

# 1.4 Natural Splines

library(splines)

  # empirical curve
plot(Delivery_month, Gas_Future, xlab="Delivery Month", main = "Gas Future Contract Values on Oct 15, 2021")

  # natural splines
n <- length(Gas_Future)
x <- seq(1,n)
lines(x,fitted(lm(Gas_Future~ns(x,df=5))),col=1,lwd=1.5)
lines(x,fitted(lm(Gas_Future~ns(x,df=8))),lty=2,col=2,lwd=1.5)
lines(x,fitted(lm(Gas_Future~ns(x,df=15))),lty=4,col=4,lwd=1.5)
legend("topright", c("df = 5", "df = 8", "df = 15"), lty = c(1,2,4), col = c(1,2,4), lwd=c(1.5,1.5,1.5))

# 1.5 Comparison of Polynomial Regression and Natural Splines

  # comparison - fit
plot(Delivery_month, Gas_Future, xlab="Delivery Month", main = "Gas Future Contract Values on Oct 15, 2021")
lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, 9))),lty = 2, col = 2,lwd=1.5)
lines(x,fitted(lm(Gas_Future~ns(x,df=8))),lty=4,col=4, lwd=1.5)

legend("topright", cex = 0.75, c("Polynomial (degree 9)", "Natural Splines (degree 8)"), lty = c(2,4), col = c(2,4), lwd=c(1.5,1.5))

  # comparison - forecast

  # polynomial
p <- 9
I <- seq(32,38)
T<-length(I)
coef <- lm(Gas_Future ~ poly(Delivery_month,  degree = p, raw = TRUE))$coef
pred <- rep(0,T)
for (i in 1:T)
{
  pred[i] <- sum(coef * I[i]^(0:p))
}

plot(Gas_Future,xlim=c(0,42),ylim=c(2,6),xlab="Delivery Month",  main = "Polynomial Gas Future Price Prediction")
lines(Delivery_month,fitted(lm(Gas_Future ~ poly(Delivery_month, p))), lty = 2, col = 2,lwd=1.5)
lines(I,pred, lty = 4, col = 4,lwd=1.5)

  # natural splines
ns_8 <- lm(Gas_Future~ns(x,df=8))
pred_ns <- predict(ns_8, newdata=data.frame(x=I))
lines(I,pred_ns, lty = 6, col = 6,lwd=1.5)

legend("bottomleft", cex = 0.75, c("Fitted", "Predicted (polynomial)", "Predicted (natural splines"), lty = c(2,2, 6), col = c(2,4, 6), lwd=c(1.5,1.5,1.5))

##################################################################################################################################

# Clear plots
#dev.off()

# Clear variables
#rm(list = ls())

# Clear console
#cat("\014")