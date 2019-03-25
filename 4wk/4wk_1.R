#setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/5wk")
#getwd()
# homework.r review
xy.df = read.csv(file='old.sam.for.reg.fit.csv')
nxy.df = read.csv(file='old.sam.for.reg.pred.csv')

rst.mat = matrix(NA,4,3)
colnames(rst.mat) =c("sq","abs",'hub')
rownames(rst.mat) =c("R^2","Ra^2",'AIC',"pred_error")

# sq # 변수몽땅 다
colnames(xy.df)
# lm object = lm 함수로 얻어진 결과값
lm.fit = lm(formula=sensitivity~.,data=xy.df)

names(lm.fit)
'''
[1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values"
 [6] "assign"        "qr"            "df.residual"   "contrasts"     "xlevels"      
[11] "call"          "terms"         "model"       
'''

lm.summ=summary(lm.fit)
names(lm.summ)
'''
 [1] "call"          "terms"         "residuals"     "coefficients"  "aliased"      
 [6] "sigma"         "df"            "r.squared"     "adj.r.squared" "fstatistic"   
[11] "cov.unscaled" 
'''
# $ 기호를 해주고 그 key를 주면 value를 준다 
rst.mat[,1]=c(summary(lm.fit)$r.squared,
summary(lm.fit)$adj.r.squared,
AIC(lm.fit),
mean((nxy.df[,1]-predict(lm.fit,newdata=nxy.df))^2)) # y_hat

rst.mat
'''
Call:
lm(formula = sensitivity ~ ., data = xy.df)

Residuals:
Min      1Q  Median      3Q     Max 
-64.491 -10.177   0.152  10.828  48.526 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -5.3971     0.8628  -6.255 5.50e-10 ***
typeB        -2.3506     1.1178  -2.103 0.035679 *   #더미 A가 기준 # 다른변수가 고정되어 있을떄, type이라는 변수가 A타입일 떄에 비하여, B일때에 y의 조건부 기대값이 ~~만큼 차이난다
typeC         0.8903     1.0977   0.811 0.417503     #더미
pressureLow  -3.4528     0.9176  -3.763 0.000176 *** #더미 High가 기준 
V1           -3.2118     0.4666  -6.883 9.40e-12 *** #더미
V2            0.4206     0.4597   0.915 0.360416    
V3            0.1066     0.4623   0.231 0.817642    
V4            0.2554     0.4664   0.548 0.584016    
V5           -0.1205     0.4584  -0.263 0.792621    
V6            0.4615     0.4447   1.038 0.299580    
V7            3.2684     0.4567   7.156 1.43e-12 ***
V8            1.8431     0.4637   3.975 7.45e-05 ***
V9            0.4476     0.4469   1.001 0.316809    
V10          -0.4020     0.4604  -0.873 0.382807    
V11          -1.8727     0.4534  -4.131 3.87e-05 ***
V12          -0.5021     0.4708  -1.066 0.286424    
V13          -0.4303     0.4539  -0.948 0.343366    
V14          -0.4802     0.4579  -1.049 0.294530    
V15           0.2501     0.4666   0.536 0.592062    
V16           0.7639     0.4561   1.675 0.094219 .  
V17          -0.3179     0.4600  -0.691 0.489606    
V18          -2.1532     0.4531  -4.752 2.26e-06 ***
---
변수선택시 pvalue를 기준으로 세울 수 있음
모형을 변형하거나 변수를 선택을 하는 액션 때 필요함 
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 15.88 on 1212 degrees of freedom
Multiple R-squared:  0.1382,	Adjusted R-squared:  0.1233 
F-statistic: 9.258 on 21 and 1212 DF,  p-value: < 2.2e-16

'''

library(quantreg)
rq.fit = rq(formula=sensitivity~.,data=xy.df)
names(summary.rq.fit)
'''
[1] "coefficients"  "x"             "y"             "residuals"    
[5] "dual"          "fitted.values" "contrasts"     "formula"      
[9] "terms"         "xlevels"       "call"          "tau"          
[13] "rho"           "method"        "model"   
'''
rq.summ = summary(rq.fit)
names(rq.summ)
'''
[1] "call"         "terms"        "coefficients" "residuals"   
[5] "rdf"          "tau" 
'''

## R^2, adjR^2가 없다!  SST의 분해는 제곱로스일 때만 가능하다 

rst.mat[,2]=c(NA,NA,AIC(rq.fit),
mean((nxy.df[,1]-predict(rq.fit,newdata=nxy.df))^2))
rst.mat



library(robustreg)
rb.fit=robustRegH(sensitivity~.,data=xy.df)
names(rb.fit) # [1] "coefficients" "weights"      "mse" 

# 후버에 대해서 숙제 



# 변수를 늘려보자
n = dim(xy.df)[1];n # 샘플갯수
# 1234,10
e.mat=matrix(rnorm(n*20),ncol = 20)

xxy.df = cbind(xy.df,e.mat)
colnames(xxy.df)

lm.fit = lm(formula=sensitivity~.,data=xxy.df)
summary(lm.fit)$r.squared
rst.mat



## fomular 만드는 거 확인해보기!!! 모든 교호 3차 
## Full