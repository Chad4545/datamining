getwd()
setwd('4wk')

# 회귀 계수가 수렴하는지 안하는지 알아보자
# 선형식에서 만들어서, 그 자료에다가 회귀모형을 적합한 후
# 자료의 수가 많아지면, 회귀변수가 어떻게 변화 하는지 확인

## simulation sample

x.vec = 1:10
x.vec

y.vec = -3+2*x.vec
y.vec

plot(x.vec,y.vec)
# 하지만 실제 자료는 이렇게 안생김 


n = 10
e.vec = rnorm(n)  # 에러 
y.vec = -3+2*x.vec + e.vec
plot(x.vec,y.vec)


# 이제 x와 y 만 가지고 추정회귀계수를 만들어보고 그것이 진짜 -3, 2가 되는 지 확인

## fit linear regression
xy.df = as.data.frame(cbind(y.vec,x.vec))
#xy.df
b.vec=coef(lm(y.vec~.,data=xy.df))
b.vec


## 자료 늘리기


n = 100
e.vec = rnorm(n)  # 에러 
x.vec = seq(0,1,length.out = n)
y.vec = -3+2*x.vec + e.vec
plot(x.vec,y.vec)


# 이제 x와 y 만 가지고 추정회귀계수를 만들어보고 그것이 진짜 -3, 2가 되는 지 확인

## fit linear regression
xy.df = as.data.frame(cbind(y.vec,x.vec))
#xy.df
b.vec=coef(lm(y.vec~.,data=xy.df))
b.vec
# 비슷하게 나옴 

# 모델이 샘플 사이즈에 따라서 점점 수렴된다는 것을 수치화 하고 싶다 

del = sqrt(sum((b.vec - c(-3,2))^2)) # 두 벡터사이의 거리의 norm


# sample size가 증가함에 따라서 저 del 이라는 값이 줄어들어야 정상 
# 여러개의 sample 크기를 트라이해서 del을 샘플사이즈 축에 그리자 
d.vec = rep(NA,1000)
n.vec = rep(NA,1000)
for (i in 1: 1000){
  n = i*50  
  n.vec[i] = n
  e.vec = rnorm(n)  # 에러 
  x.vec = seq(0,1,length.out = n)
  y.vec = -3+2*x.vec + e.vec
  xy.df = as.data.frame(cbind(y.vec,x.vec))
  b.vec=coef(lm(y.vec~.,data=xy.df))
  d.vec[i] = sqrt(sum((b.vec - c(-3,2))^2)) # 두 벡터사이의 거리의 norm
}
matplot(n.vec,d.vec,type='h')


# 이런 이론들을 활용하여 회귀모형의 진단이 가능하다 