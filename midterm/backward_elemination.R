
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/midterm")
xy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")

#source("dummy_function.R")
#xy.df <- dummy.function(xy.df,2)
#xy.df <- dummy.function(xy.df,4)
head(xy.df)
colnames(xy.df)[1] = "y"
p = dim(xy.df)[2] # 변수갯수  # 21 ,  설명변수 20 , 반응변수 1

# first step 
e.vec = NULL;
lm.fit = lm(y~.,data=xy.df)
e.vec = AIC(lm.fit)
e.opt = min(e.vec)
print(e.vec)

a.set=NULL
c.vec = 2:p
for(j in c(1:p+1)){ # 20번 돌릴거야 
  print(j)
  e.vec = NULL;
  for (i in c.vec){
    d.set =c(a.set,i)
    print(d.set)
    lm.fit = lm(y~.,data=xy.df[,-d.set])
    e.vec = c(e.vec,AIC(lm.fit))
  }
  if(e.opt < min(e.vec)) break  # full모델의 성능이 지금 한거보다 좋으면 멈춤
  e.opt = min(e.vec)
  target=which.min(e.vec);target # 제일 에러가 작다 == 그때 변수 빼는게 맞다 

  a.set=cbind(a.set,c.vec[target]) # 그놈을 a.set에 담고
  c.vec = c.vec[-target]           # c.vec을 업데이트
}

names(xy.df[,-c(a.set)])

library(stats)
lm.fit=lm(y~.,data=xy.df[,-c(2,3)])
?step
step(lm.fit, direction="backward")






### AIC기준으로 변수선택법(AIC가 낮을 수록 좋다)
#모든변수로 Y에 적합
fit.full <- lm(y~., data=xy.df) # 10350.68

# 설명변수를 넣지않은 모델
fit.con <- lm(y ~ 1, data=xy.df) # 10492.26

# 전진선택법
fit.forward <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "forward")
#AIC(fit.forward) 10334.71
# 후진제거법
fit.backward <- step(fit.full, list(lower=fit.con, upper = fit.full), direction = "backward")
#AIC(fit.backward) 10334.71

# 단계적회귀방법(stepwise)
fit.both <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "both")
#AIC(fit.both) # 10334.71


summary(fit.forward)
'''
Residual standard error: 15.86 on 1224 degrees of freedom
Multiple R-squared:  0.1326,	Adjusted R-squared:  0.1262 
F-statistic: 20.79 on 9 and 1224 DF,  p-value: < 2.2e-16
'''

summary(fit.backward)
'''
Residual standard error: 15.86 on 1224 degrees of freedom
Multiple R-squared:  0.1326,	Adjusted R-squared:  0.1262 
F-statistic: 20.79 on 9 and 1224 DF,  p-value: < 2.2e-16
'''
summary(fit.both)
'''
Residual standard error: 15.86 on 1224 degrees of freedom
Multiple R-squared:  0.1326,	Adjusted R-squared:  0.1262 
F-statistic: 20.79 on 9 and 1224 DF,  p-value: < 2.2e-16
'''










new_xy.df = xy.df
new_xy.df[,5] = sqrt(abs(new_xy.df[,5]))
new_xy.df[,6] =new_xy.df[,6]^2
### AIC기준으로 변수선택법(AIC가 낮을 수록 좋다)
#모든변수로 Y에 적합
fit.full <- lm(y~., data=new_xy.df) # 9987.249
AIC(fit.full)
# 설명변수를 넣지않은 모델
fit.con <- lm(y ~ 1, data=new_xy.df) # 10492.26
#AIC(fit.con) 10492.26
# 전진선택법
fit.forward <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "forward")
#AIC(fit.forward)9976.707 y ~ V3 + V2 + V1 + V7 + V18 + V8 + pressure + V11 + type
# 후진제거법
fit.backward <- step(fit.full, list(lower=fit.con, upper = fit.full), direction = "backward")
#AIC(fit.backward)9976.707  y ~ type + pressure + V1 + V2 + V3 + V7 + V8 + V11 + V18
 

# 단계적회귀방법(stepwise) y ~ V3 + V2 + V1 + V7 + V18 + V8 + pressure + V11 + type

fit.both <- step(fit.con, list(lower=fit.con, upper=fit.full), direction = "both")
#