#make dummy variables
dummy.function = function(df,number){
  name= colnames(df)
  before_name = as.vector(name[1:number-1])
  after_name = as.vector(name[number+1:(length(name)-number)])
  
  
  target_column = df[,number]
  unique_column = unique(target_column)
  unique_column = unique_column[-1] ## base line
  dummy=t(t(matrix(rep(target_column,length(unique_column)),ncol=length(unique_column)))==unique_column)*1
  colnames(dummy)=unique_column
  dummy = as.data.frame(dummy)
  
  
  result = cbind(dummy,df[,after_name])
  result = cbind(df[,before_name],result)
  
  return(result)
}