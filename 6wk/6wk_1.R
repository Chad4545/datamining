# 4/8(월)
getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/6wk")
############################################################################
xy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")
############################################################################

#xy.df = xy.df[,1:6]
#nxy.df = nxy.df[,1:6]

head(xy.df) # sensitivity/ type/ pressure/V1/V2V3

p = dim(xy.df)[2] # 6

a.set = NULL  #active set : 현재 모형에 포함되어야 하는 변수의 셋
c.set = 2:p # 추가할 대상이 되는 변수의 셋/ 1은 종속변수니깐
############################################################################
e.vec =  NULL
for(id in c.set){
  d.set =c(1,id) # y도 들어가야되고, 나머지 설명변수들도 들어가야 하니깐
  lm.fit = lm(sensitivity~.,data=xy.df[,d.set])
  ny = predict(lm.fit,newdata=nxy.df[,d.set])  
  test_error = mean(sqrt(sum((nxy.df[,1]-ny)^2))) #AIC나 BIC가 아닌 independent_test_error를 측도로 사용
  e.vec=c(e.vec,test_error)
}
opt = which.min(e.vec) ; e.opt = e.vec[opt]

############################################################################
for (iter in 2:p){
  a.set = c(a.set,c.set[opt]) ;c.set = c.set[-opt] # 2,3,5,6
  e.vec = NULL
  for(id in c.set){
    d.set =c(1,a.set,id) # y도 들어가야되고, 나머지 설명변수들도 들어가야 하니깐
    lm.fit = lm(sensitivity~.,data=xy.df[,d.set])
    ny = predict(lm.fit,newdata=nxy.df[,d.set])  
    test_error = mean(sqrt(sum((nxy.df[,1]-ny)^2))) #AIC나 BIC가 아닌 independent_test_error를 측도로 사용
    e.vec=c(e.vec,test_error)  
  }
  opt = which.min(e.vec) # 4
  if(e.opt<min(e.vec)) break
  e.opt = e.vec[opt]
}

