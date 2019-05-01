rm(list=ls())
true.beta.vec = c(0,1)
#### simulations
case.num= 100; sam.num.vec= 10^seq(1,5,length.out = case.num)
beta.mat = matrix(0,nrow=case.num,ncol =2)

for(i in 1:case.num){
  x.mat = cbind(1,runif(sam.num.vec[i]))
  e.vec = rnorm(sam.num.vec[i])
  y.vec = x.mat%*%true.beta.vec+e.vec
  beta.mat[i,]=solve(t(x.mat)%*%x.mat)%*%t(x.mat)%*%y.vec
}

par(mfrow=c(1,2))
plot(1:case.num,beta.mat[,1])
plot(1:case.num,beta.mat[,2])
sam.num.vec
runif(sam.num.vec[i])

