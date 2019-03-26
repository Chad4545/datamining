'''
1. 변수의 갯수 증감에 따른 비교
2. 샘플의 갯수 증감에 따른 비교
3. 샘플이 normal dist를 따를때, 분산에 따른 비교
4. 샘플이 t dist를 따를때, 자유도에 따른 비교

비교 기준은 y와 yhat의 mse로 한다.
'''
'''

1. 변수의 갯수에 따른 비교 
'''
set.seed(1)
library(caret)
'''
p = 사용할 설명변수의 갯수
n = 사용할 데이터의 수 
ratio = train/test 비율 (default 8:2)

실험 1에서는 n = 10000으로 고정 후
p 를 1에서 100 까지 늘려본다.
'''

make_train_test = function(p,n=10000,,ratio=0.8){
  b.vec = 1/(1:p)
  x.mat = matrix(rnorm(n*p),ncol=p)
  y.vec = x.mat%*%b.vec + rnorm(n) # 마지막텀은 오차 
  
  xy.df = data.frame(cbind(y.vec,x.mat))
  a <- createDataPartition(xy.df[,1], p = 0.8, list=FALSE)
  train <- xy.df[a,]
  test <- xy.df[-a,]
  return(list(train=train, test=test,beta=b.vec))
}
?rnorm







pred_err = function(data){
  train = as.data.frame(data$train)
  test = as.data.frame(data$test)
  lm.fit = lm(X1~.,data=train)
  
  beta =data$beta
  b.fit = coef(lm(X1~.,data=train))[-1]
  
  pred_error=mean((beta-b.fit)^2)
  return (pred_error)
}








result = matrix(NA,2,100)
rownames(result)=c("# of samples","pred_error")
for(i in seq(1,100)){
  variables=seq(100,10000,by=100)
  result[1,i] = variables[i]
  data = make_train_test(p=10,n=variables[i])
  pred_error = pred_err(data)
  result[2,i] = pred_error
}

result

min_error = min(result[2,])
min_error
inds = which(result[2,] == min_error, arr.ind=TRUE)
inds
matplot(result[1,],result[2,],type="h",
        ylab = expression("pred_error"),
        xlab = expression("# of samples"))
title(main = "experiment_1")

?matplot
