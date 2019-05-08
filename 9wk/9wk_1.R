rm(list=ls())
getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/9wk")

'''
Male Prostate cancer(전립선암) : Binary
Non-case : negative , case : positive
dim[xy.df][0] = N = 145
dim[xy.df][1] = P = 1234(including Y)
dataset is N < P so we use only 10 Explanatory variables for this lecture
'''

xy.df = read.csv("../new.sam.for.log.fit.csv")
table(xy.df[,1])
xy.df = xy.df[,1:11]
dim(xy.df)


"""
########################################################################################################
# 1. Linear Regression
########################################################################################################
"""
fit = lm(disease~.,data=xy.df) # error <- Y is not numeric

xxy.df = xy.df
xxy.df[,1] =xy.df[,1]=="case" # Now Y is True or False ( 1 or 0 )
head(xxy.df)

fit = lm(disease~.,data=xxy.df) 
predict(fit,newdata=xxy.df) # XB_hat
'''
predict values are continuous
Remember we want to classify Negative or Positive (Binary classification)
so we try thresh_hold
'''

predict(fit,newdata=xxy.df)>0.5 

# 여기서 0.5만 바꿔서 튜닝 가능 
y = xxy.df[,1]
y_hat=(predict(fit,newdata=xxy.df)>0.5) +0
cbind(y,y_hat)
'''
sensitivity is the ability of a test to correctly identify those with the disease (true positive rate),
specificity is the ability of the test to correctly identify those without the disease (true negative rate).
'''
lm_rst=table(y,y_hat)

# sensitivity(True Positive Rate)
sen = lm_rst[2,2]/sum(lm_rst[2,]);sen
# specificity(True Negative Rate)
lm_rst
spc = lm_rst[1,1]/sum(lm_rst[1,]);spc
# acc (correct_predict/test_total)
acc = sum(lm_rst[1,1],lm_rst[2,2])/sum(lm_rst);acc
# miss-acc
1-acc


rst.mat =matrix(NA,3,2)
rst.mat[,1] =rbind(sen,spc,acc)

########################################################################################################


"""
########################################################################################################
# 2. Logistic Regression
- we use glm function(Generalized Linear Model)
EX) https://www.guru99.com/r-generalized-linear-model.html

glm(formula, family = gaussian, data, weights, subset,
    na.action, start = NULL, etastart, mustart, offset,
control = list(...), model = TRUE, method = 'glm.fit,
x = FALSE, y = TRUE, singular.ok = TRUE, contrasts = NULL, ...)


- when you choose family argument, you should consider Response variable's Distribution

- glm estimates Beta by Newton-Raphson method
########################################################################################################
"""

fit = glm(disease~.,data=xy.df,family = "binomial");fit
summary(fit)
#Coefficients mean Log(odds)'s Variation
predict(fit,newdata = xy.df) 
'''
predict values are continuou
In glm, predict always gives us XB_hat

'''
xb = predict(fit,newdata = xy.df) 
exb = exp(xb)
prob = exb/(1+exb) # logistic transformation
plot(prob)   #여기서 classifier를 만들어 줄 수 있음 (thresh hold = 0.5)
# e^x>1  => x>0 ; xb값이 0보다 크다  =>
ny = (predict(fit,newdata = xy.df) >0)+0 # same as prob>0.5
cbind(xy.df[,1],ny)

'''
result is weird.
Because we didn\'t tell the model which Respose Level is Base Line
'''
xy.df[,1]
# levels = 범주
# case non-case : glm은 y가 factor인 경우에 먼저 level을 조사 
levels(xy.df[,1])
# default로 알파벳 순서로 나열함 
# glm은 맨 앞 케이스를 base-line으로 잡는다 == False == 0로 잡는다.
# 그래서 여기에서는 case를 0으로 잡는다 
# base-line: 기준 (안걸릴 확률)
# 병에 안걸릴 확률 대비(base)/ 병에 걸릴 확률이 중요한 거니깐 

# 따라서 level순서를 바꿔줌 
xy.df[,1] = relevel(xy.df[,1], "non-case")
levels(xy.df[,1])
fit = glm(disease~.,data=xy.df,family = "binomial");fit
ny = (predict(fit,newdata = xy.df) >0)+0
cbind(xy.df[,1],ny) # factor, numeric이라서 이렇게 나옴 
# xy.df[,1] 에서 2 면 1이고 1이면 0인 것 
logistic_rst=table(xy.df[,1],ny)
logistic_rst
sen=logistic_rst[2,2]/sum(logistic_rst[2,]);sen
spc=logistic_rst[1,1]/sum(logistic_rst[1,]);spc
acc = sum(diag(logistic_rst))/sum(logistic_rst);acc


rst.mat[,2] =rbind(sen,spc,acc)
colnames(rst.mat)=c("lm","glm")
rownames(rst.mat)=c("sen","spc","acc")
rst.mat


'''
linear vs logistic 
둘중에 어떤 방법이 어떤 좋은 방법인지
성능을 평가하시오 :
1. train test (갯수가 적으니깐 8:2 or 7:3)
2. train -> fit
3. test -> sen spc acc
4. 1~3을 randomize 100번
5. 4를 average

# randomsample 할때 0따로 1따로 해야지 unbalance를 방지할 수 있다. 

'''