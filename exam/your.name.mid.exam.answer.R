
### This is a sample answer sheet for the mid-exam
rm(list=ls())
xy.df = read.csv(file="D:\\mid.exam.csv")
head(xy.df)

##### Question A ################################################# 

# 1)
A.fun = function(xy.df){
  # fill in your answers here 
  return(ret)
}
A.fun(xy.df) # a part of variable indices from 1 to 18 


##### Question B ################################################# 
b.vec = rep(NA,18); aic.vec = rep(NA,18)

# 1) 
fit = lm(target~.,data=xy.df[,c(1,2,3,4)])
b.vec[1] = abs(coef(fit)[4])
b.vec 

# 2) 
# fill in your answers here 
b.vec 

# 3) 
# fill in your answers here 
b.vec 
o.vec = order(b.vec,decreasing=TRUE)
o.vec 

# 4) 
id = o.vec[1]+3
fit = lm(target~.,data=xy.df[,c(1,2,3,id)])
aic.vec[1] = AIC(fit)
aic.vec

# 5) 
# fill in your answers here 
aic.vec

# 6) 
# fill in your answers here 
aic.vec

# 7) 
B.fun = function(xy.df){
  b.vec = aic.vec = rep(NA,18)
  # fill in your answers here
  o.vec = order(b.vec,decreasing=TRUE)
  # fill in your answers here
  opt = which.min(aic.vec)
  ret = o.vec[1:opt]
  return(ret)
}
B.fun(xy.df)

##### Question C ################################################# 

aic.vec = rep(NA,18); b.mat = matrix(NA,50,18)

# 1) 
n = dim(xy.df)[1]; b.id = sample(1:n,n,replace=TRUE); bxy.df = xy.df[b.id,]
fit = lm(target~.,data=bxy.df)
b.mat[1,] = coef(fit)[-c(1,2,3,4)]
b.mat[1,]

# 2) 
# fill in your answers here 
b.mat
o.vec = order(abs(colMeans(b.mat)),decreasing=TRUE) 
o.mat

# 3) 
# fill in your answers here 
aic.vec

# 4) 
# fill in your answers here 
aic.vec

# 5) 
# fill in your answers here 
aic.vec

# 6) 
C.fun = function(xy.df){
  aic.vec = rep(NA,18); b.mat = matrix(NA,50,18)
  # fill in your answers here
  o.vec = order(abs(colMeans(b.mat)),decreasing=TRUE)
  # fill in your answers here
  opt = which.min(aic.vec)
  ret = o.vec[1:opt]
  return(ret)
}
C.fun(xy.df)

##### Question D ################################################# 

# 1) 
set.seed(1); e.vec = rep(NA,3)
# fill in your answers here
e.vec 

# 2) 
set.seed(1); e.mat = matrix(NA,50,3)
# fill in your answers here
e.mat 
boxplot(e.mat)

##### Question E ################################################# 
# fill in your answers here




# good luck to you !!!! 



    
  



