rm(list=ls())
getwd()
xy.df=read.csv("../old.sam.for.reg.fit.csv")
nxy.df=read.csv("../old.sam.for.reg.pred.csv")
source("chad_forward.R")
source("chad_backward.R")
library(stats)
names(xy.df)=c("y",names(xy.df[-1]))
#모든변수로 Y에 적합
fit.full <- lm(y~., data=xy.df) # 10350.68
# 설명변수를 넣지않은 모델
fit.con <- lm(y ~ 1, data=xy.df) # 10492.26
# forward
fit.forward <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "forward",k=2)
chad_forward_aic=forward.fun(xy.df,mod='aic');chad_forward_aic
'''
step: forward in R
y ~ V7 + V1 + V18 + V11 + V8 + pressure + type + V16
chad_forward by chad
"V7"       "V1"       "V18"      "V11"      "V8"       "pressure" "type"     "V16"  
'''
# backward
fit.backward <- step(fit.full, list(lower=fit.con, upper = fit.full), direction = "backward",k=2)
chad_backward_aic=backward.fun(xy.df,mod='aic');chad_backward_aic
'''
step: backward in R
y ~ type + pressure + V1 + V7 + V8 + V11 + V16 + V18
chad_backward_aic=backward.fun(xy.df,mod='aic');chad_backward_aic
"type"     "pressure" "V1"       "V7"       "V8"       "V11"      "V16"      "V18"     
'''
