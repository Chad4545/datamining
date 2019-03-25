p = 10 # 계수의 수
n = 10000 # 샘플의 수 
b.vec = 1/(1:p)

x.mat =matrix(rnorm(n*p),ncol=p)
y.vec = x.mat%*%b.vec + rnorm(n) # 마지막텀은 오차 

y.vec

xy.df = data.frame(cbind(y.vec,x.mat))
lm.fit = lm(X1~.,data=xy.df)
b.fit = coef(lm(X1~.,data=xy.df))
summary(lm.fit)



# 좋은 모형을 찾았다는 것이 원래 맞는 모형을 찾는것은 아니다
# 단지 주어진 조건에서의 최선의 모형을 만든 것 
# 샘플 사이즈가 충분히 많아서 데이터가 완전히 그 모형에 집약되기 전 까지는
# 변수를 쳐내는 데에 피밸류를 100%로 믿으면 안된다
# 왜냐면 샘플사이즈에 민감하기 때문에


# 정규분포 vs 티분포
# 샘플사이즈. 오차가 정규분포일때 오차가 티분포일때랑 비교하기

# 같은 정규분포라고 하더라도 그 오류의 분산이 1일때랑 10일떄랑 비교 
# 티분포에서의 자유도에 따라서 