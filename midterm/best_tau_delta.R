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


split_point=dim(xy.df)[1]
new = rbind(xy.df,y.df)
end_point = dim(new)[1]
new = dummy.function(new,2)
names(new)[1]<-c("sensitivity")
new = dummy.function(new,4)

new_tr = new[1:split_point,]
split_point=split_point + 1
new_te = new[split_point:end_point,]



######################################## 
#                                      #
#                LOSS                  #
#                                      #
######################################## 
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

rst.mat

##################################################################
## 여기서 부터 LAD, HUBER의 Tuning parameter인 tau와 ,k2를 조정 ##
##################################################################
# 먼저 LAD 에서 Tau를 조정한다. 측도는 squared error와 AIC 


# grid search for [LAD] 
LAD.mat = matrix(NA,3,100)

rownames(LAD.mat)=c("Tau","AIC","Test_error")
LAD.mat
tau.tune=seq(0,1,length.out = 100)

for(j in 1:100){
  # Tune parameter
  LAD.mat[1,j] = tau.tune[j]
  abs.fit=rq(formula = sensitivity~V1,tau=tau.tune[j] ,data=new_tr)
  #AIC
  LAD.mat[2,j]=AIC(abs.fit)
  #test squared erro
  test_pred=cbind(1,new_te[,5])%*%coef(abs.fit)
  loss=new_te[,1]-test_pred
  test_loss=sqrt(sum((loss)^2)/dim(new_te)[1])
  LAD.mat[3,j]=test_loss
}
#LAD.mat


################
### plot LAD ###
################
matplot(LAD.mat[1,],LAD.mat[2,],type="h",
        ylab = expression("AIC"),
        xlab = expression("Tau"))
title(main = "Tau in LAD & AIC")

matplot(LAD.mat[1,],LAD.mat[3,],type="h",
        ylab = expression("Test error"),
        xlab = expression("Tau"))
title(main = "Tau in LAD & Test error")



# grid search for [Huber]
Huber.mat = matrix(NA,3,100)

rownames(Huber.mat)=c("Delta","AIC","Test_error")
Huber.mat
delta.tune=seq(0.5,1.5,length.out = 100)
for(j in 1:100){
  # Tune parameter
  Huber.mat[1,j] = delta.tune[j]
  huber.fit = rlm(formula = sensitivity~V1,data=new_tr,k2 = delta.tune[j])
  #AIC
  Huber.mat[2,j]=AIC(huber.fit)
  #test squared erro
  test_pred=cbind(1,new_te[,5])%*%coef(huber.fit)
  loss=new_te[,1]-test_pred
  test_loss=sqrt(sum((loss)^2)/dim(new_te)[1])
  Huber.mat[3,j]=test_loss
}
#Huber.mat
Huber.mat
################
### plot Huber ###
################
?matplot
matplot(Huber.mat[1,],Huber.mat[2,],type="h",
        ylab = expression("AIC"),
        xlab = expression("Delta"),
        ylim = c(10300,10600))
title(main = "delta in Huber & AIC")

matplot(Huber.mat[1,],LAD.mat[3,],type="h",
        ylab = expression("Test error"),
        xlab = expression("Delta"))
title(main = "Delta in Huber & Test error")





#######################################################
# result matrix
tuning_1.mat = matrix(NA,3,3)
colnames(tuning_1.mat)=c("lm","LAD","Huber")
rownames(tuning_1.mat)=c("best_Tau","Best_delta","AIC")
tuning_1.mat
#####################################################
# base lm model#
tuning_1.mat[3,1] =AIC(lm.fit)
#####################################################
# LAD
# min AIC
minaic = min(LAD.mat[2,])
tuning_1.mat[3,2] = minaic
# tau when min AIC
inds = which(LAD.mat[2,] == minaic, arr.ind=TRUE)
tuning_1.mat[1,2] = LAD.mat[,inds][1]
tuning_1.mat

Huber.mat
# Huber
minaic = min(Huber.mat[2,])
tuning_1.mat[3,3] = minaic
tuning_1.mat
# delta when min AIC
inds = which(Huber.mat[2,] == minaic, arr.ind=TRUE)
tuning_1.mat[2,3] = Huber.mat[,inds][1]
tuning_1.mat



#######################################################
# result matrix
tuning_2.mat = matrix(NA,3,3)
colnames(tuning_2.mat)=c("lm","LAD","Huber")
rownames(tuning_2.mat)=c("best_Tau","Best_delta","Test error")
tuning_2.mat
#####################################################
# base lm model#
test_pred=cbind(1,new_te[,5])%*%coef(lm.fit)
loss=new_te[,1]-test_pred
test_loss=sqrt(sum((loss)^2)/dim(new_te)[1])

tuning_2.mat[3,1] =test_loss
tuning_2.mat
#####################################################
# LAD
# min AIC
LAD.mat
minerror = min(LAD.mat[3,])
tuning_2.mat[3,2] = minerror
# tau when min error
inds = which(LAD.mat[3,] == minerror, arr.ind=TRUE)
inds
tuning_2.mat[1,2] = LAD.mat[,inds][1]
tuning_2.mat

Huber.mat
# Huber
minerror = min(Huber.mat[3,])
tuning_2.mat[3,3] = minerror
tuning_2.mat
# delta when min error
inds = which(Huber.mat[3,] == minerror, arr.ind=TRUE)
tuning_2.mat[2,3] = Huber.mat[,inds][1]
tuning_2.mat


##############################
# rst.mat에 추가하기 
##############################