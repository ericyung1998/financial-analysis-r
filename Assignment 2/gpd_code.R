library(robustbase)
gpd.2q<-function(prob,obj)
{
xi_upper= obj[[1]]$par.ests[1]
threshold_upper= obj[[1]]$threshold
lambda_upper= obj[[1]]$par.ests[2]


xi_lower= obj[[2]]$par.ests[1]
threshold_lower= obj[[2]]$threshold
lambda_lower= obj[[2]]$par.ests[2]

ori_data= obj[[3]]

iu=which(ori_data> obj[[1]]$threshold)
il=which(-ori_data> obj[[2]]$threshold)
n=length(ori_data)

qs=quantile(ori_data,prob=prob)
ratios=c(length(il)/n,1-length(il)/n-length(iu)/n,length(iu)/n)

qs[which(prob<ratios[1])]=-qgpd(prob[prob<ratios[1]]/ratios[1],xi_lower,threshold_lower,lambda_lower) 

qs[which(prob>1-ratios[3])]=qgpd((prob[prob>1-ratios[3]]-(1-ratios[3]))/ratios[3], xi_upper,threshold_upper,lambda_upper)    

return(qs)
}

eda.shape<-function (x, xname = deparse(substitute(x))) 
{
  cat(xname)
  par(mfrow = c(2, 2))
  hist(x, xlab = "", main = "", col = "blue")
  boxplot(x)
  iqd <- summary(x)[5] - summary(x)[2]
  plot(density(x, width = 2 * iqd), xlab = "", ylab = "", type = "l", 
       main = "")
  x <- x[!is.na(x)]
  v <- stats::qqnorm(x, main = "", xlab = "")
  abline(lmrob(v$y ~ v$x))
  par(mfrow = c(1, 1))
  title(main = paste("EDA of ", xname, sep = ""))
}







shape.plot<-function (data, tail = "upper", method = "mle", from = 0.5, to = 0.98, 
          nint = 30) 
{
  if (tail == "upper") {
    upper.shape.plot(data)
    title("Percent Data Points above Threshold\n")
  }
  if (tail == "lower") {
    data <- -data
    upper.shape.plot(data)
    title("Percent Data Points below -Threshold\n")
  }
  if (tail == "two") {
    par(mfrow = c(1, 2))
    upper.shape.plot(data)
    title("Percent Data Points above Threshold\n")
    data <- -data
    upper.shape.plot(data)
    title("Percent Data Points below -Threshold\n")
    par(mfrow = c(1, 1))
  }
}







upper.shape.plot<-function (data, method = "ml", from = 0.5, to = 0.98, nint = 30) 
{
  library(evir)
  if (is(data, "series")) {
    data <- seriesData(data)
  }
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }
  data <- sort(unclass(data))
  assign("tempData", data, pos = 1)
  #estFun <- get(paste("gpd", method, sep = "."))
  estFun <- get(paste("gpd", sep = ""))
  assign("tempEstFun", estFun, pos = 1)
  n <- length(data)
  l1 <- data[trunc(from * n)]
  l2 <- data[trunc(to * n)]
  x <- pretty(c(l1, l2), n = nint)
  one.y <- function(u) {
    xx <- tempData[tempData > u]
    excess <- xx - u
    est_xi=NA
    tryCatch({ gpd.est <- tempEstFun(data = excess, threshold = 0, method=method)$par.est
    est_xi=gpd.est[1]},error = function(e){print (e)})
    if(is.na(est_xi))
    {
      gpd.est <- tempEstFun(data = excess, threshold = 0, method="pwm")$par.est
      est_xi=gpd.est[1]
    }
    
        c(est_xi, length(xx)/length(tempData))
  }
  iii <- as.matrix(apply(as.matrix(x), 1, one.y))
  yy <- iii[1,]
  ylim <- range(yy)
  ylim[1] <- ylim[1] - 0.5
  ylim[2] <- ylim[2] + 0.5
  t1 <- "Estimate of k"
  if (SHAPE.XI) 
    t1 <- "Estimate of xi"
  plot(x, yy, type = "l", xlab = "Threshold", ylab = t1, ylim = ylim)
  nl <- length(pretty(x))
  xx <- pretty(x)
  OB <- outer(rep(1, as.integer(length(xx))), data, "*")
  OX <- outer(as.double(xx), rep(1, n), "*")
  PERC <- apply(OB > OX, 1, sum)/n
  axis(3, at = xx, lab = paste(format(round(PERC * 1000)/10)))
}





gpd.tail<-function(data, upper = NA, lower=NA, nextremes = NA, method = c("ml", 
                                                            "pwm"), information = c("observed", "expected"), ..., plotgpd=TRUE) 
{
  library(evir)
  fit_lower=NULL
 fit_upper=NULL
 if(!is.na(upper)&(!is.na(lower))) par(mfrow=c(1,2))
  if(!is.na(upper))
  {
    fit_upper=gpd(data=data, threshold = upper, nextremes = nextremes, method = method, information = information) 
  if(plotgpd==TRUE)
  {
    y <- seq(1, length(data[data>upper]))/(length(data[data>upper])+1)
    qq <- qgpd(y, xi = fit_upper$par.ests[1],mu=fit_upper$threshold, beta=fit_upper$par.ests[2])
 plot(sort(qq), sort(data[data>upper]), xlab = paste("GPD Quantiles, for xi = ", 
                                           format(fit_upper$par.ests[1], digits = 3, nsmall = 2)), 
         ylab = "Excess over threshold",main=("Upper Tail"))

    
  }
  }
  
  if(!is.na(lower))
  {fit_lower=gpd(data=-data, threshold = -lower, nextremes = nextremes, method = method, information = information) 
  if(plotgpd==TRUE)
  {
    y <-  y <- seq(1, length(-data[data<lower]))/(length(-data[data<lower])+1)
    
    qq <- qgpd(y, xi = fit_lower$par.ests[1],mu=fit_lower$threshold, beta=fit_lower$par.ests[2])
    plot(sort(qq), sort(-data[data<lower]), xlab = paste("GPD Quantiles, for xi = ", 
                                    format(fit_lower$par.ests[1], digits = 3, nsmall = 2)), 
         ylab = "Excess over threshold",main=("Lower Tail"))
    
    
  }
  }
 par(mfrow=c(1,1))
 original_data=data
  return(list(fit_upper, fit_lower, original_data))
}



