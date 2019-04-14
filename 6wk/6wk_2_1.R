getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/6wk")
#1. data load
# read.csv(file='경로')
xy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")


forward.fun = function(xy.df,mod=c("adj","aic","bic","ind","cross"),ratio=0.3,n.fold=3){
  # initialize
  colnames(xy.df)[1]='y'
  p = dim(xy.df)[2]; n = dim(xy.df)[1]
  
  if (mod == "ind"){
    ind.id = sample(1:n)[1:(n*ratio)]#train ,test 셋의 id 값  
  }
  
  if (mod == "cross"){
    crs.id = split(sample(1:n),1:n.fold)
  }
  
  #first step
  a.set = NULL  #active set : 현재 모형에 포함되어야 하는 변수의 셋
  c.set = 2:p # 추가할 대상이 되는 변수의 셋/ 1은 종속변수니깐
  e.vec =  NULL # 에러 벡터
  
  for(id in c.set){
    d.set = c(1,id)
    
    if (mod=="ind"){
      lm.fit = lm(y~.,data=xy.df[-ind.id,d.set])
      ny = predict(lm.fit,newdata=xy.df[ind.id,d.set])  
      e.vec=c(e.vec,mean(abs(ny-xy.df[ind.id,1])))
    }
    if (mod=="crs"){
      cv_err = 0
      for(k in 1:n.fold){
        temp_fold=crs.id[[k]]
        lm.fit = lm(y~.,data=xy.df[-temp_fold,d.set])
        ny = predict(lm.fit,newdata=xy.df[temp_fold,d.set])  
        cv_err = cv_err + mean(abs(ny-xy.df[temp_fold,1]))
      }
      e.vec=c(e.vec,cv_err)
    }
    
    if (mod=="adj"){
      lm.fit = lm(y~.,data=xy.df[,d.set])
      e.vec=c(e.vec,-summary(lm.fit)$adj.r.squared)
    }
    if (mod=="aic"){
      lm.fit = lm(y~.,data=xy.df[,d.set])
      e.vec=c(e.vec,AIC(lm.fit))
    }
    if (mod=="bic"){
      lm.fit = lm(y~.,data=xy.df[,d.set])
      e.vec=c(e.vec,BIC(lm.fit))
    }
  }
  opt = which.min(e.vec); e.opt = min(e.vec)

  # 그 후 iterate
  for (iid in 3:p){
    print(iid)
    a.set = c(a.set,c.set[opt])
    c.set = c.set[-opt] ;e.vec = NULL
    for(id in c.set){
      d.set = c(1,a.set,id)
      if (mod=="ind"){
        lm.fit = lm(y~.,data=xy.df[-ind.id,d.set])
        ny = predict(lm.fit,newdata=xy.df[ind.id,d.set])  
        e.vec=c(e.vec,mean(abs(ny-xy.df[ind.id,1])))
      }
      if (mod=="crs"){
        cv_err = 0
        for(k in 1:n.fold){
          temp_fold=crs.id[[k]]
          
          ny = predict(lm.fit,newdata=xy.df[temp_fold,d.set])  
          lm.fit = lm(y~.,data=xy.df[-temp_fold,d.set])
          ny = predict(lm.fit,newdata=xy.df[temp_fold,d.set])  
          cv_err = cv_err + mean(abs(ny-xy.df[temp_fold,1]))
          
        }
        e.vec=c(e.vec,cv_err)
      }
      
      
      if (mod=="adj"){
        lm.fit = lm(y~.,data=xy.df[,d.set])
        e.vec=c(e.vec,-summary(lm.fit)$adj.r.squared)
      }
      if (mod=="aic"){
        lm.fit = lm(y~.,data=xy.df[,d.set])
        e.vec=c(e.vec,AIC(lm.fit))
      }
      if (mod=="bic"){
        lm.fit = lm(y~.,data=xy.df[,d.set])
        e.vec=c(e.vec,BIC(lm.fit))
      }
    }
    opt = which.min(e.vec) ;
    if(e.opt<min(e.vec)) break
    e.opt = min(e.vec)
  }
  
  return(a.set)
}

adj.fit=forward.fun(xy.df,mod=c("adj"))
aic.fit=forward.fun(xy.df,mod=c("aic"))
bic.fit=forward.fun(xy.df,mod=c("bic"))
ind.fit=forward.fun(xy.df,mod=c("ind"))
crs.fit=forward.fun(xy.df,mod=c("cross"))

