rm(list=ls())
getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/lecture_note")
'''
library(MASS)#huber , rlm
library(robustreg)#robustRegH()
library(quantreg)#L1 , rq
'''

# huberized loss function
huber_loss = function(y,x.vec,b,del){
  x.mat = cbind(1,x.vec)
  r.vec = y - x.mat%*%b
  yes_or_no = abs(r.vec)<=del
  return(mean(r.vec^2 + (2*del*abs(r.vec) - del^2))/2) 
}
## Huber loss function

y.vec = rnorm(10) # vector
x.vec = rnorm(10)
x.mat = cbind(1,x.vec) # matrix
plot(x.vec,y.vec)
del = 1.345
b.vec.01 = c(1,1)
b.vec.02 = c(1,0)

huber_loss(y.vec,x.vec,b.vec.01,del)
huber_loss(y.vec,x.vec,b.vec.02,del)
