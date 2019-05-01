forward.fun = function(data.df,
                       mod=c("adj","ind","cross","aic","bic"),
                       ratio=0.3,n.fold=5){
  # initialize 
  colnames(data.df)[1] = "y"
  p = dim(data.df)[2] # 변수갯수
  n = dim(data.df)[1] # 데이터갯수
  # 데이터가 어떻게 분할되냐에 따라서 최종 모형이 바뀜
  if(mod=="ind"){
    ind.id = sample(1:n)[1:(n*ratio)]
  }
  if(mod=="cross"){
    crs.id = split(sample(1:n),1:n.fold)
  }
  
  
  # first step 
  a.set = NULL; c.set = 2:p; e.vec = NULL
  
  for(id in c.set){
    d.set = c(1,id); 
    if(mod=="adj"){
      lm.fit = lm(y~.,data=data.df[,d.set])
      e.vec = c(e.vec,-summary(lm.fit)$adj.r.squared)
    }
    if(mod=="aic"){
      lm.fit = lm(y~.,data=data.df[,d.set])
      e.vec=c(e.vec,AIC(lm.fit))
    }
    if(mod=="bic"){
      lm.fit = lm(y~.,data=data.df[,d.set])
      e.vec=c(e.vec,BIC(lm.fit))
    }
    if(mod=="ind"){
      lm.fit = lm(y~.,data=data.df[-ind.id,d.set])
      ny = predict(lm.fit,newdata=data.df[ind.id,d.set])
      e.vec = c(e.vec,mean(abs(ny-data.df[ind.id,1])))
    }
    if(mod=="cross"){
      cv.err = 0 
      for(k in 1:n.fold){
        lm.fit = lm(y~.,data=data.df[-crs.id[[k]],d.set])
        ny = predict(lm.fit,newdata=data.df[crs.id[[k]],d.set])
        cv.err = cv.err+mean(abs(ny-data.df[crs.id[[k]],1]))
      }
      cv.err = cv.err/n.fold
      e.vec = c(e.vec,cv.err)
    }
    
  }
#  a.set = NULL; c.set = 2:p; e.vec = NULL
  opt = which.min(e.vec); e.opt = min(e.vec)
  # itertation 
  for(iid in 3:p){
    a.set = c(a.set,c.set[opt])
    c.set = c.set[-opt]; e.vec = NULL 
    for(id in c.set){
      d.set = c(1,a.set,id); 
      if(mod=="adj"){
        lm.fit = lm(y~.,data=data.df[,d.set])
        e.vec = c(e.vec,-summary(lm.fit)$adj.r.squared)
      }
      if(mod=="aic"){
        lm.fit = lm(y~.,data=data.df[,d.set])
        e.vec = c(e.vec,AIC(lm.fit))
      }
      if(mod=="bic"){
        lm.fit = lm(y~.,data=data.df[,d.set])
        e.vec=c(e.vec,BIC(lm.fit))
      }
      if(mod=="ind"){
        lm.fit = lm(y~.,data=data.df[-ind.id,d.set])
        ny = predict(lm.fit,newdata=data.df[ind.id,d.set])
        e.vec = c(e.vec,mean(abs(ny-data.df[ind.id,1])))
      }
      if(mod=="cross"){
        cv.err = 0 
        for(k in 1:n.fold){
          lm.fit = lm(y~.,data=data.df[-crs.id[[k]],d.set])
          ny = predict(lm.fit,newdata=data.df[crs.id[[k]],d.set])
          cv.err = cv.err+mean(abs(ny-data.df[crs.id[[k]],1]))
        }
        cv.err = cv.err/n.fold
        e.vec = c(e.vec,cv.err)
      }
    } 
    
    opt = which.min(e.vec); 
    if(e.opt<min(e.vec)) break 
    e.opt = min(e.vec)
  }
  return(colnames(data.df[,a.set]))
}

#adj.set = forward.fun(xy.df,mod="adj");adj.set
#ind.set = forward.fun(xy.df,mod="ind");ind.set
#crs.set = forward.fun(xy.df,mod="cross");crs.set
#aic.set = forward.fun(xy.df,mod="aic");aic.set
#bic.set = forward.fun(xy.df,mod="bic");bic.set
