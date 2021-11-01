########################################################### Problem I ############################################################

setwd("")  # set directory

library(quantreg)

# 1. Problem 4.8 on page 273 on Carmona

# 1.1 LS regressions (diagnostics)

HILLS.data <- read.table("hills.csv",header = T, sep=",")
attach(HILLS.data)
head(HILLS.data)

par(mfrow=c(1,2))

  # LS regression 1 [LM] (time x dist)
HILLS.LSM1 <- lm(time~dist)
plot(dist,time, main="Time VS Dist (LS)")
abline(HILLS.LSM1)

  # Residual QQ Plot
qqnorm(studres(HILLS.LSM1), main="Time VS Dist (LS)\n Studentized Residual Normal QQ Plot")
qqline(studres(HILLS.LSM1))
#plot(density(studres(HILLS.LSM1))

  # Summary
summary(HILLS.LSM1)

  # LS regression 2 [LM] (time x climb)
HILLS.LSM2 <- lm(time~climb)
plot(climb,time, main="Time VS Climb (LS)")
abline(HILLS.LSM2)

  # Residual QQ Plot
qqnorm(studres(HILLS.LSM2), main="Time VS Climb (LS)\n Studentized Residual Normal QQ Plot")
qqline(studres(HILLS.LSM2))
#plot(density(studres(HILLS.LSM2))

  # Summary
summary(HILLS.LSM2)

# 1.2 LAD regressions (diagnostics)

# https://stat.ethz.ch/pipermail/r-help/2006-August/110386.html
# https://stats.stackexchange.com/questions/129200/r-squared-in-quantile-regression


  # LAD regression 1 (time x dist)
HILLS.LAD1 <- rq(time~dist,0.5)
plot(dist,time, main="Time VS Dist (LAD)")
abline(HILLS.LAD1)

  # Residual QQ Plot
qqnorm(HILLS.LAD1$residuals, main="Time VS Dist (LAD)\n Residual Normal QQ Plot")
qqline(HILLS.LAD1$residuals)
#plot(density(HILLS.LAD1$residuals))

  # Analog R-sqr (manual)
HILLS.LAD1.rhat <- HILLS.LAD1$coef[1]+HILLS.LAD1$coef[2]*dist
time.rbar = mean(time)

HILLS.LAD1.ex_var <- sum((HILLS.LAD1.rhat-time.rbar)^2)
HILLS.LAD1.tot_var <- sum((time-time.rbar)^2)

HILLS.LAD1.ex_var / HILLS.LAD1.tot_var

  # Summary
summary(HILLS.LAD1)

# LAD regression 2 (time x climb)
HILLS.LAD2 <- rq(time~climb,0.5)
plot(climb,time, main="Time VS Climb (LAD)")
abline(HILLS.LAD2)

  # Residual QQ Plot
qqnorm(HILLS.LAD2$residuals, main="Time VS Climb (LAD)\n Residual Normal QQ Plot")
qqline(HILLS.LAD2$residuals)
#plot(density(HILLS.LAD2$residuals))

# Analog R-sqr (manual)
HILLS.LAD2.rhat <- HILLS.LAD2$coef[1]+HILLS.LAD2$coef[2]*climb
time.rbar = mean(time)

HILLS.LAD2.ex_var <- sum((HILLS.LAD2.rhat-time.rbar)^2)
HILLS.LAD2.tot_var <- sum((time-time.rbar)^2)

HILLS.LAD2.ex_var / HILLS.LAD2.tot_var

  # Summary
summary(HILLS.LAD2)

# 1.3 Prediction (interpolation and extrapolation)

c(min(dist),max(dist))

  # Interpolation
for (X_dist in c(5,10,15,20,25)) {
  predi2<-HILLS.LSM1$coef[1]+HILLS.LSM1$coef[2]*X_dist
  predi1<-HILLS.LAD1$coef[1]+HILLS.LAD1$coef[2]*X_dist
  predis<-round(c(predi2,predi1,predi2 - predi1),0)
  names(predis)<-c("LS", "LAD", "Diff")
  print(c(X_dist, predis))
}

  # Extrapolation
for (X_dist in c(30,35,40,45,50)) {
  predi2<-HILLS.LSM1$coef[1]+HILLS.LSM1$coef[2]*X_dist
  predi1<-HILLS.LAD1$coef[1]+HILLS.LAD1$coef[2]*X_dist
  predis<-round(c(predi2,predi1,predi2 - predi1),0)
  names(predis)<-c("LS", "LAD", "Diff")
  print(c(X_dist, predis))
}

# 1.4 Perturbed Data

head(HILLS.data)

  # Perturbed data

Thills <- HILLS.data
colnames(Thills) <- c("TX", "Tdist", "Tclimb", "Ttime")
Thills["Ttime"][Thills["TX"] == "Lairig Ghru"] <- 92.667
Thills[Thills["TX"] == "Lairig Ghru"]
attach(Thills)

  # Scatterplot of time x dist (superimposed LS and perturbed LS)

par(mfrow=c(1,2))

plot(dist,time, main="Time VS Dist (LS and Perturbed LS)")
abline(HILLS.LSM1, lty=2) # LS

HILLS.LSM1_new <- lm(Ttime~Tdist)
abline(HILLS.LSM1_new, col="red") # perturbed LS

  # Summary
summary(HILLS.LSM1_new)

legend("bottomright", cex = 0.75, c("LS","Perturbed LS"), lty=c(2,1), col=c("black","red"))

  # Scatterplot of time x dist (superimposed LAD and perturbed LAD)

plot(dist,time, main="Time VS Dist (LAD and Perturbed LAD)")
abline(HILLS.LAD1, lty=2) # LAD

HILLS.LAD1_new <- rq(Ttime~Tdist,0.5) # Perturbed LAD
abline(HILLS.LAD1_new, col="red")

  # Summary
summary(HILLS.LAD1_new)

legend("bottomright", cex = 0.75, c("LS","Perturbed LAD"), lty=c(2,1), col=c("black","red"))

# 1.5 Multiple LS [LM] (time x dist x climb)

  # Multiple Regression
HILLS.multi <- lm(time~dist+climb)
summary(HILLS.multi)

par(mfrow=c(1,1))

  # Residual QQ Plot
qqnorm(studres(HILLS.multi), main="Time VS Distance + Climb (LS)\n Studentized Residuals Normal QQ Plot")
qqline(studres(HILLS.multi))
#plot(density(HILLS.multi$residuals))

########################################################### Problem II ###########################################################

library(MASS)

GOOG <- read.table("Google.csv",header = T, sep=",")
attach(GOOG)
head(GOOG)

# 2.1 Simple LS Regression

  # Fit model
excess_return <- rGoog-rf
GOOG.fit <- lm(excess_return~rM_ex)

  # Summary
summary(GOOG.fit)

# 2.2i Significance

  # r-squared + p-value

# 2.2ii a Significance

GOOG.fit$coefficients[1]

  # a = 0.005715405 

# 2.2iii b Significance

GOOG.fit$coefficients[2]

  # b = 0.996074301 < 1

# 2.2iv Standardized/Studentized Residuals - model diagnostics

par(mfrow=c(2,1))

plot(stdres(GOOG.fit),type="l",main="Standardized Residuals")
plot(studres(GOOG.fit),type="l",main="Studentized Residuals")

n <- length(excess_return) - 1
plot(stdres(GOOG.fit)[-n],stdres(GOOG.fit)[-1],main="Standardized residual against previous one ")
cor(stdres(GOOG.fit)[-n],stdres(GOOG.fit)[-1])

plot(studres(GOOG.fit)[-n],studres(GOOG.fit)[-1],main="Studentized residual against previous one ")
cor(studres(GOOG.fit)[-n],studres(GOOG.fit)[-1])

acf(stdres(GOOG.fit), type = "correlation", plot = TRUE)
acf(studres(GOOG.fit), type = "correlation", plot = TRUE)

par(mfrow=c(1,2))
qqnorm(stdres(GOOG.fit),main="Q-Q Plot of Standardized Residuals")
abline(0,1,col="red")
qqnorm(studres(GOOG.fit),main="Q-Q Plot of Studentized Residuals")
abline(0,1,col="red")

# 3. Fama-French Three-factor Model (LS Regression)

pairs(cbind(excess_return,rM_ex,rSmB,rHmL))

FF3factor <- lm(excess_return ~ rM_ex + rSmB + rHmL)
summary(FF3factor)

# 4. Factor significance (whole and indvidually)

  # F statistics and p-value

# 5. Single Factor vs Multi Factor (variation explanation)

round(c(summary(GOOG.fit)$r.squared, summary(FF3factor)$r.squared),3)

# 6. Single Factor vs Multi Factor (statistically significance)

anova(GOOG.fit,FF3factor)

##################################################################################################################################

# Clear plots
#dev.off()

# Clear variables
#rm(list = ls())

# Clear console
#cat("\014")


