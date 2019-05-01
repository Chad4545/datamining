# 4/8(월)
rm(list=ls())
getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/6wk")
############################################################################
xy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")
############################################################################
xy.df = rbind(xy.df,nxy.df)
n = dim(xy.df)[1];p = dim(xy.df)[2] # 6
cv.id = split(sample(1:n),1:5)

head(xy.df) # sensitivity/ type/ pressure/V1/V2V3

p = dim(xy.df)[2] # 6
p
a.set = NULL  #active set : 현재 모형에 포함되어야 하는 변수의 셋
c.set = 2:p # 추가할 대상이 되는 변수의 셋/ 1은 종속변수니깐
############################################################################
e.vec =  NULL
for(id in c.set){
  d.set =c(1,id) # y도 들어가야되고, 나머지 설명변수들도 들어가야 하니깐
  cv_err = 0
  for (k in 1:5){
    temp_fold=cv.id[[k]]
    lm.fit = lm(sensitivity~.,data=xy.df[-temp_fold,d.set])
    ny = predict(lm.fit,newdata=xy.df[temp_fold,d.set])  
    cv_err = cv_err + mean(sqrt(sum((xy.df[temp_fold,1]-ny)^2))) #AIC나 BIC가 아닌 independent_test_error를 측도로 사용
  }
  e.vec=c(e.vec,cv_err)
}
opt = which.min(e.vec) ; e.opt = e.vec[opt]


e.vec
opt







