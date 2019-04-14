rm(list=ls())

setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/5wk")
getwd()

xy.df =  read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df = read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")

p = dim(xy.df)[2]  ;p
c.set = 2:p ; c.set
opt = c()
error = c()
for(l in 1:p-1){
  e.vec = rep(1000,p-1-length(opt))
  for(i in c.set[!c.set %in% opt]){
    
    
    lm.fit = lm(sensitivity ~. , data = xy.df[,c(1,opt,i)])
    
    ny = predict(lm.fit, newdata = nxy.df[,c(1,opt,i)])
    
    e.vec[i] =  mean(sqrt(sum((nxy.df[,1]-ny)^2))) #AIC나 BIC가 아닌 independent_test_error를 측도로 사용
  }
  opt = c(opt,which.min(e.vec)) ; opt
  error = c(error,e.vec[which.min(e.vec)]) ;error
}

library(data.table)
#install.packages("data.table")
print(opt)
print(error)

name <- c("opt", "error")
rst.table <- data.table(opt,error) ; rst.table
print("Selected Variables")
opt[1:which.min(error)]
plot(1:20,error)
