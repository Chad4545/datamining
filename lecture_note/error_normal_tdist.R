rm(list=ls())
library(quantreg)
library(robustreg)

aic.fun = function(beta,x.mat,y.vec){
  sse = sum(((y.vec -x.mat%*%beta)^2))
  n = length(y.vec)
  k = length(beta)
  return(log((sse/n)) + 2*k/n)
}


rsq.fun = function(beta,x.mat,y.vec){

  sse = sum((y.vec-x.mat%*%beta)^2)
  sst = sum((y.vec-mean(y.vec))^2)
  return(1-sse/sst)
}

##### training and test sample generation

b = c(0,5) # true
tr.n = 500
te.n = 500
dist= 'n'
dist= 't'
t.df = 1

tr.x.vec = runif(tr.n)
te.x.vec = runif(te.n)

if(dist=='n'){tr.e.vec = rnorm(tr.n);te.e.vec=rnorm(te.n)}
if(dist=='t'){tr.e.vec = rt(tr.n,df=t.df);te.e.vec=rt(te.n,df=t.df)}

tr.y.vec = b[1] + b[2]*tr.x.vec + tr.e.vec
te.y.vec = b[1] + b[2]*te.x.vec + te.e.vec

par(mfrow=c(2,1));
plot(tr.x.vec,tr.y.vec);
plot(te.x.vec,te.y.vec)


tr.df = as.data.frame(cbind(tr.y.vec,tr.x.vec))
te.df = as.data.frame(cbind(te.y.vec,te.x.vec))
colnames(tr.df) = c("y","x")
colnames(te.df) = c("y","x")


tr.x.mat = cbind(1,tr.x.vec)
te.x.mat = cbind(1,te.x.vec)
colnames(tr.x.mat) = c("int",'x')
colnames(te.x.mat) = c("int",'x')


######3 estimation

lm.fit = lm(y~x,data=tr.df)
lm.fit$coef

lad.fit = rq(y~x,data=tr.df,tau=0.5)
lad.fit$coef

huber.fit = robustRegH(y~x,data=tr.df,tune=1.345)
huber.fit$coefficients


### rsq
rsq.fun(lm.fit$coefficients,tr.x.mat,tr.y.vec)
rsq.fun(lad.fit$coefficients,tr.x.mat,tr.y.vec) # no meaning
rsq.fun(huber.fit$coefficients,tr.x.mat,tr.y.vec)# no meaning

### AIC
aic.fun(lm.fit$coefficients,tr.x.mat,tr.y.vec)
aic.fun(lad.fit$coefficients,tr.x.mat,tr.y.vec) # no meaning
aic.fun(huber.fit$coefficients,tr.x.mat,tr.y.vec)# no meaning


