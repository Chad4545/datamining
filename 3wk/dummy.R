getwd()
rm(list=ls())
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/midterm")
xy.df = read.csv("../old.sam.for.reg.fit.csv")
y.df = read.csv("../old.sam.for.reg.pred.csv")


## make dummy variable

x = sample(x=c('Vision','NLP','ML'),size=10,replace = TRUE)
ux = unique(x)
ux=ux[-1]  #baseline
result = t(t(matrix(rep(x,length(ux)),ncol = length(ux)))==ux)*1
colnames(result)
colnames(result)=ux
result
cbind(result,x)
