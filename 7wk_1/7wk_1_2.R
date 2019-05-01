############################################################
#            Randomization
# 1. 데이터 합치고
# 2. 데이터 랜덤하게 짜르고 
# 3. 하나는 fit하고 하나는 pred 하고
# 4. 1로 돌아가서 다시 
############################################################

getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/7wk")
rm(list=ls())

source("advanced_fucntions.R")
source("r_functions_advanced.R")
xy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")

xy.df = rbind(xy.df,nxy.df) # 데이터 합치기
dim(xy.df) # 1334, 21

par(mar = rep(2, 4))
plot.new()
par(mfrow=c(2,2))
for(k in 4:7){
  plot(xy.df[,k],xy.df[,1])  
}
plot(xy.df[,k])

# 변수 transformation 해서 
# full model 인데
# trainsformation 한거랑 비교!
# 그 모델을 for에 추가해서 그려오기 

for(k in 8:11){
  plot(xy.df[,k],xy.df[,1])  
}


ran.num = 100; rst.tab =NULL
for(i in 1:100){
  print(i)
  ts.id = sample(1:dim(xy.df)[1])[1:300]
  
  rst.vec = NULL
  fit = lm(sensitivity~.,data=xy.df[-ts.id,])
  error=mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,])))
  rst.vec=c(rst.vec,error)
  
  for (mod in c("adj","ind","cross")){
    set = forward.fun(xy.df[-ts.id,],mod=mod)
    fit = lm(sensitivity~.,data=xy.df[-ts.id,c(1,set)])
    rst.vec = c(rst.vec,mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,set]))))
  }
  rst.tab=rbind(rst.tab,rst.vec)
}


# average vs ranking method
# best -> box plot

boxplot(rst.tab) # <- 여기에 트랜스포메이션 한거 붙히기 





fit = lm(sensitivity~.,data=xy.df[-ts.id,])
summary(fit)
