getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/7wk_2")
rm(list=ls())
source("r_functions_advanced.R")

xy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")
xy.df = rbind(xy.df,nxy.df) # 데이터 합치기

xy.df.list=list()
#model1
xy.df.list[[1]] = xy.df
xy.df.list[[1]][,5] = sqrt(abs(xy.df[,5]))
xy.df.list[[1]][,6] = xy.df[,6]^2
#model2
xy.df.list[[2]] = xy.df
xy.df.list[[2]][,5] = xy.df[,5]^2
xy.df.list[[2]][,6] = log(( xy.df[,6]-min( xy.df[,6]))/(max( xy.df[,6])-min( xy.df[,6]))+0.001)
#model3
xy.df.list[[3]] = xy.df
xy.df.list[[3]][,5] = log(abs(xy.df[,5]))
xy.df.list[[3]][,6] = log(abs(xy.df[,6]))
xy.df.list[[3]][,8] = log(abs(xy.df[,8]))
#model4
xy.df.list[[4]] = xy.df
xy.df.list[[4]][,5] = log(abs(xy.df[,5]))
xy.df.list[[4]][,6] = xy.df[,6]^2


ran.num = 100; rst.tab = NULL 
for(i in 1:100){
  print(i)  
  ts.id = sample(1:dim(xy.df)[1])[1:300]
  rst.vec = NULL 
  for(j in 1:4){
    fit = lm(sensitivity~.,data=xy.df.list[[j]][-ts.id,])
    rst.vec = c(rst.vec,mean(abs(xy.df.list[[j]][ts.id,1]-predict(fit,newdata=xy.df.list[[j]][ts.id,]))))
  }
  fit = lm(sensitivity~.,data=xy.df[-ts.id,])
  rst.vec = c(rst.vec,mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,]))))
  rst.tab = rbind(rst.tab,rst.vec)
}
boxplot(rst.tab)


# 랭킹 순서대로 forward해서 멈추기 (랭킹은 주어줄것임)