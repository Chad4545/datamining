rm(list=ls())
getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/7wk")
xy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")

crs.id = split(sample(1:1234),1:5)


forward.fun = function(xy.df,
                       mod=c("adj","ind","cross","aic","bic"),
                       ratio=0.3,n.fold=5){
  # initialize 
  colnames(xy.df)[1] = "y"
  p = dim(xy.df)[2] # 변수갯수  # 21 ,  설명변수 20 , 반응변수 1
  n = dim(xy.df)[1] # 데이터 갯수 # 1234
  
  # 데이터가 어떻게 분할되냐에 따라서 최종 모형이 바뀜 
  if(mod=="ind"){
    # 1:n까지 섞고서, ratio 만큼 떼온다
    ind.id = sample(1:n)[1:(n*ratio)]
  }
  
  if(mod=="cross"){
    # 1:n 섞고서, n.fold 만큼 분할
    crs.id = split(sample(1:n),1:n.fold)
  }
  
  
  # first step 
  a.set = NULL;
  c.set = 2:p;  # 선택할 변수들
  e.vec = NULL
  
  for(id in c.set){
    d.set = c(1,id); #실제 변수
    if(mod=="adj"){
      lm.fit = lm(y~.,data=xy.df[,d.set])
      # adj 경우 1에 가까울수록 좋은것이다. 우리의 측도는 가장 낮은게 좋게끔 할것이기 때문에
      # - 부호를 붙혀준다 
      e.vec = c(e.vec,-summary(lm.fit)$adj.r.squared)
    }
    if(mod=="aic"){
      lm.fit = lm(y~.,data=xy.df[,d.set])
      e.vec=c(e.vec,AIC(lm.fit))
    }
    if(mod=="bic"){
      lm.fit = lm(y~.,data=xy.df[,d.set])
      e.vec=c(e.vec,BIC(lm.fit))
    }
    if(mod=="ind"){
      # train으로 fit하고
      lm.fit = lm(y~.,data=xy.df[-ind.id,d.set])
      # test로 에러구함
      ny = predict(lm.fit,newdata=xy.df[ind.id,d.set])
      e.vec = c(e.vec,mean(abs(ny-xy.df[ind.id,1])))
    }
    
    if(mod=="cross"){
      cv.err = 0 
      for(k in 1:n.fold){
        #fold만큼의 에러를 다 더한다
        lm.fit = lm(y~.,data=xy.df[-crs.id[[k]],d.set])
        ny = predict(lm.fit,newdata=xy.df[crs.id[[k]],d.set])
        cv.err = cv.err+mean(abs(ny-xy.df[crs.id[[k]],1]))
      }
      e.vec = c(e.vec,cv.err)
    }
    
  }
  # 현재 e.vec는 c.set만큼 (20개), 그 중에서 에러가 가장 작은 값을 선택 
  opt = which.min(e.vec); e.opt = min(e.vec)
  
  
  
  
  ##################################  ##################################
  # itertation 
  for(iid in 3:p){
    a.set = c(a.set,c.set[opt])
    c.set = c.set[-opt]; e.vec = NULL 
    for(id in c.set){
      d.set = c(1,a.set,id); 
      if(mod=="adj"){
        lm.fit = lm(y~.,data=xy.df[,d.set])
        e.vec = c(e.vec,-summary(lm.fit)$adj.r.squared)
      }
      if(mod=="aic"){
        lm.fit = lm(y~.,data=xy.df[,d.set])
        e.vec = c(e.vec,AIC(lm.fit))
      }
      if(mod=="bic"){
        lm.fit = lm(y~.,data=xy.df[,d.set])
        e.vec=c(e.vec,BIC(lm.fit))
      }
      if(mod=="ind"){
        lm.fit = lm(y~.,data=xy.df[-ind.id,d.set])
        ny = predict(lm.fit,newdata=xy.df[ind.id,d.set])
        e.vec = c(e.vec,mean(abs(ny-xy.df[ind.id,1])))
      }
      if(mod=="cross"){
        cv.err = 0 
        for(k in 1:n.fold){
          lm.fit = lm(y~.,data=xy.df[-crs.id[[k]],d.set])
          ny = predict(lm.fit,newdata=xy.df[crs.id[[k]],d.set])
          cv.err = cv.err+mean(abs(ny-xy.df[crs.id[[k]],1]))
        }
        e.vec = c(e.vec,cv.err)
      }
    } 
    
    opt = which.min(e.vec); 
    print(min(e.vec))
    if(e.opt<min(e.vec)) break 
    e.opt = min(e.vec)
  }
  return(a.set)
}

adj.set = forward.fun(xy.df,mod="adj")
ind.set = forward.fun(xy.df,mod="ind")
crs.set = forward.fun(xy.df,mod="cross")
aic.set = forward.fun(xy.df,mod="aic")
bic.set = forward.fun(xy.df,mod="bic")
