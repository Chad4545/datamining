adj.rsq.fun = function(beta,x.mat,y.vec){
  
  sse = sum((y.vec-x.mat%*%beta)^2)
  sst = sum((y.vec-mean(y.vec))^2)
  n = length(y.vec)
  p = length(beta) -1
  return(1-((sse/(n-p-1))/(sst/(n-1))))
}