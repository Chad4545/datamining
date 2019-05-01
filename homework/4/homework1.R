getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/homework/4")
# data
rm(list=ls())
xy.df = read.csv(file='../../old.sam.for.reg.fit.csv')
nxy.df = read.csv(file='../../old.sam.for.reg.pred.csv')
# result matrix
rst.mat = matrix(NA,4,3)
colnames(rst.mat)=c("lm","lad","huber")
rownames(rst.mat) =c("R^2","Ra^2",'AIC',"pred_error")
rst.mat


'''
1. data의 변수를 모두 다 사용
2. 기본 linear 모델, abs loss 모델, huber loss 모델 모두 사용
3. R^2, adj R^2, AIC, pred_error 의 평가지표 모두 사용 
'''

colnames(xy.df)
#1 linear - square loss
lm.fit = lm(formula=sensitivity~.,data=xy.df)
names(lm.fit)
lm.summ = summary(lm.fit)
names(lm.summ)
rst.mat[,1]=c(summary(lm.fit)$r.squared,
              summary(lm.fit)$adj.r.squared,
              AIC(lm.fit),
              mean((nxy.df[,1]-predict(lm.fit,newdata=nxy.df))^2)) # y_hat

rst.mat
# abs loss
library(quantreg)
rq.fit = rq(formula=sensitivity~.,data=xy.df)
names(rq.fit)
rq.summ = summary(rq.fit)
names(rq.summ)
## R^2, adjR^2가 없다!  SST의 분해는 제곱로스일 때만 가능하다 
rst.mat[,2]=c(NA,NA,AIC(rq.fit),
              mean((nxy.df[,1]-predict(rq.fit,newdata=nxy.df))^2))
rst.mat



# 3. huber loss
library(robustreg)
require(MASS)
rb.fit <- rlm(sensitivity~.,data=xy.df, k2 = 1.345) 
names(rb.fit) # [1] "coefficients" "weights"      "mse" 
rb.summ=summary(rb.fit)
names(rb.summ)
# 후버에 대해서 숙제 
## R^2, adjR^2가 없다!  SST의 분해는 제곱로스일 때만 가능하다 

rst.mat[,3]=c(NA,NA,AIC(rb.fit),
              mean((nxy.df[,1]-predict(rb.fit,newdata=nxy.df))^2))
rst.mat

lm        lad      huber
R^2        1.382343e-01         NA         NA
Ra^2       1.233028e-01         NA         NA
AIC        1.035068e+04 10427.3823 10352.5840
pred_error 1.667319e+02   166.0556   163.3688

