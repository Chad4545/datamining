getwd()
library(glmnet)
#example
y.vec = c(3,-2,1)
x.vec = c(-1,2,2)
x.mat = cbind(1,x.vec)
b.vec = c(-1,1)


### Lasso loss function
lasso.loss.fun <- function(y.vec,x.vec,b.vec,lambda){
  x.mat = cbind(1,x.vec)
  r.vec <- y.vec-x.mat%*%b.vec

  return((sum(r.vec^2))/2 + lambda*abs(b.vec[2]))
}

lasso.loss.fun(y.vec,x.vec,b.vec,lambda = 2)
