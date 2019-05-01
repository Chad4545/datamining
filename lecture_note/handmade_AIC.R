rm(list=ls())
library(lars)

data("diabetes");names(diabetes)
diab.df = as.data.frame(cbind(diabetes$y,diabetes$x))
colnames(diab.df)
colnames(diab.df) = c("y",colnames(diabetes$x))
colnames(diab.df)

### sq loss
lm.fit = lm(y~bmi,data=diab.df)
summary(lm.fit)
lm.fit$coefficients

### quantile loss (tau = 0.5 = LAD (least Absolute Deviation loss))
library(quantreg)
rq.fit = rq(y~bmi,data=diab.df,tau=0.5)
summary(rq.fit)
rq.fit$coefficients

### Huber loss (delda = 1.345, Huber's recommendation of delta)
library(robustreg)
hub.fit = robustRegH(y~bmi,data=diab.df,tune = 1.345)
summary(hub.fit)
hub.fit$coefficients


### AIC
aic.fun = function(beta,x.mat,y.vec){
  sse = sum(((y.vec -x.mat%*%beta)^2))
  n = length(y.vec)
  k = length(beta)
  return(log((sse/n)) + 2*k/n)
}

x.mat = cbind(1,diab.df$bmi)
aic.lm = aic.fun(lm.fit$coef,x.mat,diab.df$y);aic.lm
aic.rq = aic.fun(rq.fit$coefficients,x.mat,diab.df$y);aic.rq
aic.hub = aic.fun(hub.fit$coefficients,x.mat,diab.df$y);aic.hub
