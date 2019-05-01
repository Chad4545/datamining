#independent training - test error
ind_err = function(tr.df,te.df){
  tr.fit = lm(y~.,data=tr.df)
  
  y_hat = predict(tr.fit, newdata = te.df)
  ind_err = sum((te.df$y - y_hat)^2)
  return(ind_err/dim(te.df)[1])
}
