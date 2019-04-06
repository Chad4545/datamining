rm(list=ls())

setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/5wk")
getwd()

xy.df =  read.csv("old.sam.for.reg.fit.csv")
nxy.df = read.csv("old.sam.for.reg.pred.csv")

p = dim(xy.df)[2]  ;p
c.set = 2:p ; c.set
opt = c()
error = c()
for(l in 1:p-1){
  e.vec = rep(1000,p-1-length(opt))
  for(i in c.set[!c.set %in% opt]){
    
    
    lm.fit = lm(sensitivity ~. , data = xy.df[,c(1,opt,i)])
    
    ny = predict(lm.fit, newdata = nxy.df[,c(1,opt,i)])
    
    e.vec[i] = mean(abs(nxy.df[,1]-ny))
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
