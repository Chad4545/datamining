##3/13 수요일
setwd('2wk')
getwd()
rm(list=ls())
## Huber loss function
y.vec = rnorm(10) # vector
x.vec = rnorm(10)
b.vec = c(-1,1)
x.mat = cbind(1,x.vec) # matrix
drop(x.mat%*%b.vec) # drop : 한쪽 디멘젼이 1이면 원래는 벡터니깐 벡터로 만들어줌
r.vec = y.vec-drop(x.mat%*%b.vec) # ri값 만든것 
r.vec
del =1 

#위에텀
r.vec^2*(abs(r.vec) <=del)
#아래텀
(2*del*abs(r.vec)-del^2)*(abs(r.vec) >del)

####################실제함수####################
hub.fun = function(y.vec,x.vec,b.vec,del){
  x.mat = cbind(1,x.vec)
  r.vec = y.vec-drop(x.mat%*%b.vec) # ri값 만든것
  
  tmp = r.vec^2*(abs(r.vec) <=del)
  tmp = tmp + (2*del*abs(r.vec)-del^2)*(abs(r.vec) >del)
  
  loss = sum(tmp)
  return(loss)
}
#############################################


