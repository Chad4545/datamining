#cross_validation_error
#### data.df must  add train + test 
cv_err.fun = function(data.df,fold){
  n = dim(data.df)[1]
  
  pos.vec = sample(1:n)
  fold_list = split(pos.vec,1:fold)
  cv_err_vec = rep(0,fold)
  for(i in 1:fold){
    id = fold_list[[i]]
    tr.df = data.df[-id,]
    tr.fit = lm(y~.,data=tr.df)
    
    ts.df=data.df[id,]
    y_hat=predict(tr.fit,newdata=ts.df)
    cv_err_vec[i]=sum((ts.df$y - y-hat)^2)
  } 
  return(mean(cv_err_vec))
}
