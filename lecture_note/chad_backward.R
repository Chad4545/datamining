backward.fun = function(data.df,
                       mod=c("adj","ind","cross","aic","bic"),
                       ratio=0.3,n.fold=5){
  colnames(data.df)[1] = "y"
  p = dim(data.df)[2] # 변수갯수
  n = dim(data.df)[1] # 데이터갯수
  
  if(mod=="ind"){
    ind.id = sample(1:n)[1:(n*ratio)]
  }
  if(mod=="cross"){
    crs.id = split(sample(1:n),1:n.fold)
  }
  
  # first step
  e.vec = NULL
  if(mod=="adj"){
    lm.fit = lm(y~.,data=data.df)
    e.vec = c(e.vec,-summary(lm.fit)$adj.r.squared)
  }
  if(mod=="aic"){
    lm.fit = lm(y~.,data=data.df)
    e.vec=c(e.vec,AIC(lm.fit))
  }
  if(mod=="bic"){
    lm.fit = lm(y~.,data=data.df)
    e.vec=c(e.vec,BIC(lm.fit))
  }
  if(mod=="ind"){
    lm.fit = lm(y~.,data=data.df[-ind.id,])
    ny = predict(lm.fit,newdata=data.df[ind.id,])
    e.vec = c(e.vec,mean(abs(ny-data.df[ind.id,1])))
  }
  if(mod=="cross"){
    cv.err = 0 
    for(k in 1:n.fold){
      lm.fit = lm(y~.,data=data.df[-crs.id[[k]],])
      ny = predict(lm.fit,newdata=data.df[crs.id[[k]],])
      cv.err = cv.err+mean(abs(ny-data.df[crs.id[[k]],1]))
    }
    cv.err = cv.err/n.fold
    e.vec = c(e.vec,cv.err)
  }
  
  e.opt = min(e.vec)
############################################
  a.set=NULL
  c.vec = 2:p

  for(j in 1:p-1){ # 20번 돌릴거야 
    e.vec = NULL;
    for (i in c.vec){
      d.set =c(a.set,i)
      if(mod=="adj"){
        lm.fit = lm(y~.,data=data.df[,-d.set])
        e.vec = c(e.vec,-summary(lm.fit)$adj.r.squared)
      }
      if(mod=="aic"){
        lm.fit = lm(y~.,data=data.df[,-d.set])
        e.vec=c(e.vec,AIC(lm.fit))
      }
      if(mod=="bic"){
        lm.fit = lm(y~.,data=data.df[,-d.set])
        e.vec=c(e.vec,BIC(lm.fit))
      }
      
      if(mod=="ind"){
        lm.fit = lm(y~.,data=data.df[-ind.id,-d.set])
        ny = predict(lm.fit,newdata=data.df[ind.id,-d.set])
        e.vec = c(e.vec,mean(abs(ny-data.df[ind.id,1])))
      }
      
      if(mod=="cross"){
        cv.err = 0 
        for(k in 1:n.fold){
          lm.fit = lm(y~.,data=data.df[-crs.id[[k]],-d.set])
          ny = predict(lm.fit,newdata=data.df[crs.id[[k]],-d.set])
          cv.err = cv.err+mean(abs(ny-data.df[crs.id[[k]],1]))
        }
        cv.err = cv.err/n.fold
        e.vec = c(e.vec,cv.err)
      }
      
      }
  if(e.opt < min(e.vec)) break  # full모델의 성능이 지금 한거보다 좋으면 멈춤
  e.opt = min(e.vec)
  target=which.min(e.vec);target # 제일 에러가 작다 == 그때 변수 빼는게 맞다 
  a.set=cbind(a.set,c.vec[target]) # 그놈을 a.set에 담고
  c.vec = c.vec[-target]           # c.vec을 업데이트
  }
  return(colnames(data.df[,-c(1,a.set)]))
}

#adj.set = backward.fun(xy.df,mod="adj");adj.set
#ind.set = backward.fun(xy.df,mod="ind");ind.set
#crs.set = backward.fun(xy.df,mod="cross");crs.set
#aic.set = backward.fun(xy.df,mod="aic");aic.set
#bic.set = backward.fun(xy.df,mod="bic");bic.set
