getwd()
rm(list=ls())
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/midterm")
xy.df = read.csv("../old.sam.for.reg.fit.csv")
y.df = read.csv("../old.sam.for.reg.pred.csv")

head(xy.df)
head(y.df)
library(MASS)
source("dummy_function.R")
source("AIC_adj.R")
class(lm.fit)
######################################################################
# test 데이터에대해서도 동일하게 적용하려면 
# 1. 데이터를 합치고 (rbind) / 그 후 dummy.function 적용후 / 다시 나누기 
# 2. 각각 dummy.function 적용
######################################################################
'''
# 1.  - train data
# type 변수 더미화
tr_1 = dummy.function(xy.df,2)
names(tr_1)[1] <- c("sensitivity")
head(tr_1)

# pressure 변수 더미화
tr_2 = dummy.function(tr_1,4)
head(tr_2)
# 2.  - test data
te_1 = dummy.function(y.df,2)
names(te_1)[1] = c("sensitivity")
te_2 = dummy.function(te_1,4)
head(te_2)

'''

# 2  - train data + test data
split_point=dim(xy.df)[1]
new = rbind(xy.df,y.df)
end_point = dim(new)[1]
new = dummy.function(new,2)
names(new)[1]<-c("sensitivity")
new = dummy.function(new,4)

new_tr = new[1:split_point,]
split_point=split_point + 1
new_te = new[split_point:end_point,]


head(new_tr)
######################################## 
#                                      #
#                LOSS                  #
#                                      #
######################################## 
# loss = pred - 실제값 
# Loss 종류
# L2, L1, Huber, lasso...
# 평가지표
# loss , AIC, BIC, GIC, R^2, adjR^2, pred_error,...
# 
#결과테이블
rst.mat = matrix(NA,5,5)
rst.mat
colnames(rst.mat)=c("linear","basic_abs","basic_hubur","best_abs","best_huber")
rownames(rst.mat) =c("R^2","adjR^2","AIC","training_error","pred_error")
rst.mat


#############################
### 1. Linear regression  ###
#############################
lm.fit = lm(formula = sensitivity~V1,data=new_tr)
lm.summ = summary(lm.fit)
#names(lm.summ)
rst.mat[,1]=c(lm.summ$r.squared,
              lm.summ$adj.r.squared,
              AIC(lm.fit,k=2),
              mean((new_tr[,1]-predict(lm.fit,newdata = new_tr))^2),
              mean((new_te[,1]-predict(lm.fit,newdata=new_te))^2)
              ) 
rst.mat



??lad
##### quantle loss (tau=0.5 -> least absolute deviation loss)
library(quantreg)
abs.fit = rq(formula = sensitivity~V1,data=new_tr,tau=0.5)
#install.packages("L1pack")
#library(L1pack)
#abs_2.fit = lad(formula = sensitivity~V1,data=new_tr)
#abs_2.fit
abs.summ = summary(abs.fit)
names(abs.summ)
rst.mat[,2]=c(NA,
              NA,
              AIC(abs.fit),
              mean((new_tr[,1]-predict(abs.fit,newdata=new_tr))^2),
              mean((new_te[,1]-predict(abs.fit,newdata=new_te))^2)
) 
rst.mat


'''robustreg 대신에 rlm 쓰니깐 AIC BIC 나온다 개꿀!!!'''
#library(robustreg)
##### Huber loss (tune=1.345 -> Huber's recommendation of delta)
library(MASS)
huber.fit = rlm(formula = sensitivity~V1,data=new_tr,psi = psi.huber,k2 = 1.345)
huber.summ = summary(huber.fit)
#huber.summ


rst.mat[,3]=c(NA,
              NA,
              AIC(huber.fit),
              mean((new_tr[,1]-predict(huber.fit,newdata = new_tr))^2),
              mean((new_te[,1]-predict(huber.fit,newdata=new_te))^2)
)
rst.mat
