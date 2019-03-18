getwd()
setwd('3wk')

# linear regression test

# 제조공정관리데이터
# sensitivity : 제품의 민감도(노이즈추가후 표준화)
# type: 공장타입(세곳이 있는데 두 군데는 신설공장, C는 오래된 공장)
#       공정타입이 다르다
# pressure : 압력변수(categorical)
# 나머지 수치형 데이터: 근로자 근속년수, 온도, etc...
# target: sensitivity

# 강의노트 11페이지 
# data load

df<-read.csv(file="old.sam.for.reg.fit.csv")
hist(df$sensitivity)
y.vec = df[,1] # target
x.vec = df[,4] # 설명변수 1

# argument값에 따라 분석 결과가 달라진다
# y에 변수설정하는 것 : formula 시험에 출제할 것  
# formula -> y~( ) 

# data: dataframe
# dummy를 자동으로 만들어줌
?lm


##################################################################
is.matrix(df) # FALSE 
is.data.frame((df)) # TRUE
#str(df)
is.data.frame(y.vec) # FALSE
is.vector(y.vec) # TRUE

is.data.frame(df[,c(1,4)]) # TRUE

##################################################################

xy.df = df[,c(1,4)]
head(xy.df)

##################################################################
colnames(xy.df) #  "sensitivity", "V1"
colnames(df)

##################################################################
#결과테이블
rst.mat = matrix(NA,2,3)
rst.mat
##################################################################
# ?read.csv()  # header argument  사용법 시험
##################################################################
sensitivity ~ V1 # formula 안에 들어갈 값
lm.fit = lm(formula = sensitivity~V1,data=xy.df)
lm.fit
##################################################################
summary(lm.fit)
'''square loss'''
#Residuals: 잔차분석 -> 오차의가정
#Min      1Q  Median      3Q     Max 
#-70.281 -10.248   0.529  10.974  55.708 

#Coefficients:
#              Estimate Std.   Error    t value  Pr(>|t|)    
#(Intercept)  -7.4056         0.4745    -15.607  < 2e-16 ***
#  V1         -3.3072          0.4862   -6.802  1.61e-11 ***


# MSE값 은 아래값을 제곱해야함
#Residual standard error: 16.66 on 1232 degrees of freedom 

# 설명을 잘한다 못한다랑 유의하다 아니다는 별개의 문제
#Multiple R-squared:  0.03619,	Adjusted R-squared:  0.03541 

# 이 데이터가 선형회귀 모형에 적합한 데이터인지
# goodness of fit을 말해주는것 , p-value 가 작기 때문에 유의!
#F-statistic: 46.26 on 1 and 1232 DF,  p-value: 1.61e-11

# 데이터는 절대 직선위에 있지 않다는 말!

##################################################################
# 이 강의에서는  아래만 중요
#Coefficients:
#              Estimate Std.   Error    t value  Pr(>|t|)    
#(Intercept)  -7.4056         0.4745    -15.607  < 2e-16 ***
#  V1         -3.3072          0.4862   -6.802  1.61e-11 ***

# 다른방식으로 모델을 평가할 것이기 때문에! -> pred
##################################################################
# AIC는 샘플과 모형만 있으면 정의 된다
# 3x2테이블이 필요 (loss 3개, 평가지표 AIC,pred 2개)
'''quantile(absolute deviation) loss'''
library(quantreg)
?rq
# rq(formula, tau=.5, data, subset, weights, na.action,
# method="br", model = TRUE, contrasts, ...) 
# tau = loss를 기울이는 정도
# tau = 0.5 - median 추정
# tau = 0.9 - quantile 추정
rq.fit=rq(formula = sensitivity~V1,data=xy.df)
rq.fit
#rq(formula = sensitivity ~ V1, data = xy.df)

#Coefficients:
#  (Intercept)          V1 
#    -6.814326   -3.431738 

#Degrees of freedom: 1234 total; 1232 residual


'''Huber loss'''
install.packages("robustreg")
library(robustreg)

?robustRegH()
# robustRegH(formula,data,tune=1.345,m=TRUE,max.it=1000,tol=1e-5,anova.table=FALSE)
# tune = 델타
# m = 
# max.it
# tol
rb.fit=robustRegH(formula = sensitivity~V1,data=xy.df)
names(rb.fit) # "coefficients" "weights"      "mse" 
rb.fit$coefficients
#(Intercept)          V1 
#-7.12942    -3.16871 


#############################################
rst.mat # 결과 저장 매트릭스
pred.df<-read.csv(file="old.sam.for.reg.pred.csv")
head(pred.df)

pred.xy.df =pred.df[,c(1,4)]


#############################################

colnames(rst.mat)=c("lm","rq","hub")
rownames(rst.mat)=c("AIC","Prediction")
rst.mat # 결과 저장 매트릭스
'''
           lm rq hub
AIC        NA NA  NA
Prediction NA NA  NA
'''

#############################################

?coef()
coef(lm.fit)
cbind(1,pred.xy.df[,2])%*%coef(lm.fit)

ny=pred.xy.df[,1]-cbind(1,pred.xy.df[,2])%*%coef(lm.fit)
plot(ny)

# 위의 식을 데이터 갯수로 나누고 dim(pred.xy.df[1]) 
# 거기에 sqrt로 해주면 우리가 보기 좋은 scale로 변한다 


rst.mat[2,1]=sqrt(sum((pred.xy.df[,1]-ny)^2)/dim(pred.xy.df)[1])
rst.mat    



### 숙제: 평가측도를 더 만들어서 매트릭스 만들어보기 
### rq랑 huber에서 타우 (0~1사이) , 델타 최적 범위 만들어보기 
### 모형 200개 추가/ 그림 두개그리기 - tau 축/ pred축 // tune 축/ pred축 
