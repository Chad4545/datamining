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


make_data = function(p=10,n=10000,df_value=1){
  b.vec = 1/(1:p)
  x.mat = matrix(rnorm(n*p),ncol=p)
  y.vec = x.mat%*%b.vec + rt(n,df=df_value) # 마지막텀은 오차 
  
  xy.df = data.frame(cbind(y.vec,x.mat))
  return(list(data=xy.df,beta=b.vec))
}



pred_err = function(data){
  xy.df = as.data.frame(data$data)
  lm.fit = lm(X1~.,data=xy.df)
  
  beta =data$beta
  b.fit = coef(lm.fit)[-1]
  
  pred_error=mean((beta-b.fit)^2)
  return (pred_error)
}


result = matrix(NA,2,30)
rownames(result)=c("Degree of freedom","pred_error")
for(i in seq(1,30)){
  variables=seq(1,30,by=1)
  result[1,i] = variables[i]
  data = make_data(p=10,n=10000,df_value = variables[i])
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
        xlab = expression("Degree of freedom"),
        ylim = c(0,0.008))
title(main = "experiment_1")

?matplot
