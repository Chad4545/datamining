getwd()
setwd('2wk')
# 2019.03.11 데이터마이닝
# square loss for simple linear regression 

#example
y.vec = c(3,-2,1)
x.vec = c(-1,2,2)
x.mat = cbind(1,x.vec)
b.vec = c(-1,1)
# b.vec = c(bo, b1)


x.mat
x.mat %*% b.vec

y.vec - x.mat %*% b.vec 
(y.vec - x.mat %*% b.vec)^2 # 제곱해주기 loss function
sum((y.vec - x.mat %*% b.vec)^2) # loss의 합


#function 만들기
sq.loss.fun = function(y.vec, x.vec, b.vec){
  x.mat = cbind(1,x.vec)
  ret = sum((y.vec - x.mat %*% b.vec)^2)
  return(ret)
}
sq.loss.fun(y.vec, x.vec, b.vec) #34
sq.loss.fun(y.vec, x.vec, b.vec=c(1,1)) #38

# 어떤 loss가 가장 작을까?

sxx = sum((x.vec - mean(x.vec))^2)
sxy = (x.vec - mean(x.vec))*(y.vec - mean(y.vec))
sx = (x.vec - mean(x.vec))
sy = (y.vec - mean(y.vec))
sxy = sum(sx*sy)

b.vec = c(NA,NA)
b.vec[2] = sxx/sxy
b.vec[1] = mean(y.vec) - b.vec[2]*mean(x.vec)
b.vec
# y = 1.5238095 -0.8571429x
# b.vec : 최소값을 구한것이므로 이것보다 작은거 나올 수 X

b0.vec = seq(-10,10, length.out = 100)
b1.vec = seq(-10,10, length.out = 100)


l.mat = matrix(NA,100,100)
for(i in 1:100){
  for(j in 1:100){
    b.vec = c(b0.vec[i],b1.vec[j])
    l.mat[i,j] = sq.loss.fun(y.vec, x.vec, b.vec)
  }
}

l.mat
head(l.mat)
min(l.mat)

contour(b0.vec, b1.vec, l.mat, nlevels = 100)
?contour


##------------ 
# 3-D normal distribution density plot : persp() of {base} package
# with interactive plotting with Manipulate package in RStudio



install.packages("manipulate")
library(manipulate)



manipulate(persp(b0.vec, b1.vec,l.mat, 
                 theta = theta_x, # theta gives the azimuthal viewing direction
                 phi = phi_x, # phi gives the colatitude viewing direction
                 main = "Square Loss function",
                 col = "green3"),
           theta_x = slider(5, 100, initial = 35), 
           phi_x = slider(5, 100, initial = 10))

?persp
