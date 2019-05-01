rm(list=ls())
getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/lecture_note")

library(MASS)#huber , rlm
library(robustreg)#robustRegH()
library(quantreg)#L1 , rq



#1.
#install.packages("lars")
library(lars)
data("diabetes") ## 당뇨데이터 

x.mat = diabetes$x[,c(1:3)] ## 설명변수 3개
y.vec = diabetes$y          ## 종속변수 1개 
length(y.vec)               ## 442

data = data.frame(cbind(y.vec,x.mat))   ## 하나의 데이터 프레임
names(data) = c('y','x1','x2','x3')  # x1 = age, x2 = sex, x3 = bmi
train = data.frame(data[1:round(length(y.vec)*0.8),])
test  = data.frame(data[-(1:round(length(y.vec)*0.8)),])

# full
lm = lm(y~.,data=train)
sum((lm$residuals)^2)# train error
y = predict(lm,newdata = test)
sum((y-test[,1])^2) # test error

# 1 var. - 변수를 하나씩만 써보겠다 x1,x2,x3
rst.mat = matrix(NA,3,3)
for(i in 1:3){
  newtrain = train[,c(1,1+i)]
  lm = lm(y~.,data=newtrain)
  train_error = sum((lm$residuals)^2)
  
  newtest = test[,c(1,1+i)]
  #colnames(newtest)[2] = labels(test)[[2]][i+1]
  ny = predict(lm,newdata=newtest)
  test_error = sum((ny-test[,1])^2)
  rst.mat[i,1] = colnames(newtest)[2]
  rst.mat[i,2] = train_error
  rst.mat[i,3] = test_error
}
colnames(rst.mat) = c('설명변수','train_error','test_error')
rst.mat




#2 var.

newtrain = train[,c(1,2,3)] # y, x1,x2
lm = lm(y~., data = newtrain) # 변수 x1,x2 사용 
newtest = test[,c(2,3)]      # 변수 x1,x2 사용
ny = predict(lm, newdata = newtest)
sum((lm$residuals)^2)
sum((ny-test[,1])^2)

newtrain = train[,c(1,2,4)] # y,x1,x3
lm = lm(y~., data = newtrain) # 변수 x1,x3 사용 
newtest = test[,c(2,4)]           # 변수 x1,x3 사용 
ny = predict(lm, newdata = newtest)
sum((lm$residuals)^2)
sum((ny-test[,1])^2)

newtrain = train[,c(1,3,4)] # y, x2,x3
lm = lm(y~., data = newtrain) # 변수  x2,x3 사용 
newtest = test[,c(3,4)]
ny = predict(lm, newdata = newtest)
sum((lm$residuals)^2)
sum((ny-test[,1])^2)

#2.
##logit

library("stats")
convert.fun = function(x){ifelse(x>mean(y.vec),1,0)}
logit_train_y = sapply(train[,1],convert.fun)
train[,1] = logit_train_y

#1)
#AIC
full = glm(y~.,data=train,family = binomial(link = "logit"))
null = glm(y.vec~1,data=train,family = binomial(link = "logit"))
x1   = glm(y.vec~age,data=train,family = binomial(link = "logit"))
x2   = glm(y.vec~sex,data=train,family = binomial(link = "logit"))
x3   = glm(y.vec~bmi,data=train,family = binomial(link = "logit"))
x1x2 = glm(y.vec~age+sex,data=train,family = binomial(link = "logit"))
x1x3 = glm(y.vec~age+bmi,data=train,family = binomial(link = "logit"))
x2x3 = glm(y.vec~sex+bmi,data=train,family = binomial(link = "logit"))
AIC(null)
AIC(x1);AIC(x2);AIC(x3)
AIC(x1x2);AIC(x1x3);AIC(x2x3)
AIC(full)
###


#2)
fit = glm(formula = y.vec ~ bmi, family = binomial(link = "logit"), data = train)
trainy = fit$fitted.values

trainy_logit = sapply(trainy,function(x){ifelse(x>0.5,1,0)})
sum((train[,1] - trainy_logit)^2)

sum((fit$residuals)^2)

#3)
exp(-0.3211+22.8776*mean(test[,4]))/(1+exp(-0.3211+22.8776*mean(test[,4])))



#5)
test_glm = data.frame(1,test[,4])
colnames(test_glm)[2] = "bmi"
pred_y = predict(fit,newdata = test_glm)
test[,1] = sapply(test[,1],function(x){ifelse(x>mean(test[,1]),1,0)})

logit0.3 = sapply(pred_y,function(x){ifelse(x>0.3,1,0)})
table(y=test[,1],pred=logit0.3)

logit0.5 = sapply(pred_y,function(x){ifelse(x>0.5,1,0)})
table(y=test[,1],pred=logit0.5)

logit0.7 = sapply(pred_y,function(x){ifelse(x>0.7,1,0)})
table(y=test[,1],pred=logit0.7)