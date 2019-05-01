rsq.fun = function(beta,x.mat,y.vec){
  
  sse = sum((y.vec-x.mat%*%beta)^2)
  sst = sum((y.vec-mean(y.vec))^2)
  return(1-sse/sst)
}