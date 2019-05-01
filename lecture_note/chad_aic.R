aic.fun = function(beta,x.mat,y.vec){
  sse = sum(((y.vec -x.mat%*%beta)^2))
  n = length(y.vec)
  k = length(beta)
  return(log((sse/n)) + 2*k/n)
}