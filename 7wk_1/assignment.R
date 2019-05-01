############################################################
#            Randomization
# 1. 데이터 합치고
# 2. 데이터 랜덤하게 짜르고 
# 3. 하나는 fit하고 하나는 pred 하고
# 4. 1로 돌아가서 다시 
############################################################
getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/7wk_1")
rm(list=ls())
source("advanced_fucntions.R")

xy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")
xy.df = rbind(xy.df,nxy.df) # 데이터 합치기
dim(xy.df) # 1334, 21


###################################
#                                 # 
# Transformation 할 설명변수 탐색 #
#                                 # 
###################################

# 1= sensivitity, 2=type, 3=pressure 카테고리컬 변수이기 때문에 
par(mfrow=c(1,1))
plot(xy.df[,5],xy.df[,1])  
title("Explanatory variable V2")
boxplot(xy.df[,5])
plot(xy.df[,6],xy.df[,1])  
title("Explanatory variable V3")
##########################################################3##########################################################3
#######################################
v2 = xy.df[,5]                       ##
log_v2 = log(v2-min(v2)+1.1)         #1
exp_v2 = exp(v2)                     #2
exp_1_v2 = exp(v2+1)                 #3
square_v2 = v2*v2                    #4
sqrt_abs_v2 = sqrt(abs(v2))          #5
abs_v2 = abs(v2)                     #6
log_abs_v2 = log(abs_v2+1)           #7 
nega_sigmoid_v2 = 1/(1+exp(-v2))     #8
posi_sigmoid_v2 = 1/(1+exp(v2))      #9
#######################################

#plot(sigmoid_v2,xy.df[,1])
#title("sqrt_abs_v2 transformation on V2")

ran.num = 100; test.tab =NULL
for(i in 1:100){
  print(i)
  ts.id = sample(1:dim(xy.df)[1])[1:300]
  rst.vec = NULL
  for (mod in c('log','exp','exp_1','square','sqrt_abs','abs','log_abs','n_sigmoid','p_sigmoid')){
    if(mod=='log'){
      xy.df[,5] = log_v2
    }
    if(mod=='exp'){
      xy.df[,5] = exp_v2
    }
    if(mod=='exp_1'){
      xy.df[,5]=exp_1_v2
    }
    if(mod=='square'){
      xy.df[,5]=square_v2
    }
    if(mod=='sqrt_abs'){
      xy.df[,5] = sqrt_abs_v2
    }
    if(mod=='abs'){
      xy.df[,5] = abs_v2
    }
    if(mod=='log_abs'){
      xy.df[,5] = log_abs_v2
    }
    if(mod=='n_sigmoid'){
      xy.df[,5] = nega_sigmoid_v2
    }
    if(mod=='p_sigmoid'){
      xy.df[,5] = posi_sigmoid_v2
    }
    fit = lm(sensitivity~.,data=xy.df[-ts.id,])
    error=mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,])))
    rst.vec=c(rst.vec,error)
  }
  test.tab=rbind(test.tab,rst.vec)
}
test.tab
mean(test.tab[,5])

#xy.df[,5]=v2
boxplot(test.tab)
title("Transformation Result on V2")

####################################################################################################################3
#######################################
v3 = xy.df[,6]                       ##
log_v3 = log(v3-min(v3)+1.1)         #1
exp_v3 = exp(v3)                     #2
exp_1_v3 = exp(v3+1)                 #3
square_v3 = v3*v3                    #4
sqrt_abs_v3 = sqrt(abs(v3))          #5
abs_v3 = abs(v3)                     #6
log_abs_v3 = log(abs_v3+1)           #7 
nega_sigmoid_v3 = 1/(1+exp(-v3))     #8
posi_sigmoid_v3 = 1/(1+exp(v3))      #9
#######################################
ran.num = 100; test_2.tab =NULL
for(i in 1:100){
  print(i)
  ts.id = sample(1:dim(xy.df)[1])[1:300]
  rst.vec = NULL
  for (mod in c('log','exp','exp_1','square','sqrt_abs','abs','log_abs','n_sigmoid','p_sigmoid')){
    if(mod=='log'){
      xy.df[,5] = log_v3
    }
    if(mod=='exp'){
      xy.df[,5] = exp_v3
    }
    if(mod=='exp_1'){
      xy.df[,5]=exp_1_v3
    }
    if(mod=='square'){
      xy.df[,5]=square_v3
    }
    if(mod=='sqrt_abs'){
      xy.df[,5] = sqrt_abs_v3
    }
    if(mod=='abs'){
      xy.df[,5] = abs_v3
    }
    if(mod=='log_abs'){
      xy.df[,5] = log_abs_v3
    }
    if(mod=='n_sigmoid'){
      xy.df[,5] = nega_sigmoid_v3
    }
    if(mod=='p_sigmoid'){
      xy.df[,5] = posi_sigmoid_v3
    }
    fit = lm(sensitivity~.,data=xy.df[-ts.id,])
    error=mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,])))
    rst.vec=c(rst.vec,error)
  }
  test_2.tab=rbind(test_2.tab,rst.vec)
}
test_2.tab
mean(test_2.tab[,4])

#xy.df[,5]=v3

boxplot(test_2.tab)
title("Transformation Result on V3")


boxplot(xy.df[,6])
title("Transformation Result on V3")


####################################################################################################################3






xy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")
xy.df = rbind(xy.df,nxy.df) # 데이터 합치기
dim(xy.df) # 1334, 21

'''
위에서 구한 최적의 Transformation 적용
'''
new_df=xy.df
new_df[,5]=sqrt(abs(xy.df[,5]))
new_df[,6]=xy.df[,6]*xy.df[,6]



ran.num = 100; rst.tab =NULL
for(i in 1:100){
  print(i)
  ts.id = sample(1:dim(xy.df)[1])[1:300]
  
  rst.vec = NULL
  fit = lm(sensitivity~.,data=xy.df[-ts.id,])
  error=mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,])))
  rst.vec=c(rst.vec,error)
  print(rst.vec)
  
  for (mod in c("adj","ind","cross")){
    
    set = forward.fun(xy.df[-ts.id,],mod=mod)
    fit = lm(sensitivity~.,data=xy.df[-ts.id,c(1,set)])
    rst.vec = c(rst.vec,mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,set]))))
    print(rst.vec)
    
  }
  fit = lm(sensitivity~.,data=new_df[-ts.id,])
  error=mean(abs(new_df[ts.id,1]-predict(fit,newdata=new_df[ts.id,])))
  rst.vec=c(rst.vec,error)
  print(rst.vec)
  
  rst.tab=rbind(rst.tab,rst.vec)
}


# average vs ranking method
# best -> box plot
mean(rst.tab[,4])
boxplot(rst.tab) # <- 여기에 트랜스포메이션 한거 붙히기 
title("Randomization method - Full model, adj, ind, cross,transformation")
##########################################################3
min(rst.tab[,1])
min(rst.tab[,2])
min(rst.tab[,3])
min(rst.tab[,4])
min(rst.tab[,5])

max(rst.tab[,1])
max(rst.tab[,2])
max(rst.tab[,3])
max(rst.tab[,4])
max(rst.tab[,5])


mean(rst.tab[,1])
mean(rst.tab[,2])
mean(rst.tab[,3])
mean(rst.tab[,4])
mean(rst.tab[,5])


####################################################################################################################3
####################################################################################################################3
####################################################################################################################3

xy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")
xy.df[,5]=sqrt(abs(xy.df[,5]))
xy.df[,6]=xy.df[,6]*xy.df[,6]



ran.num = 100; rst.tab =NULL
for(i in 1:100){
  print(i)
  ts.id = sample(1:dim(xy.df)[1])[1:300]
  
  rst.vec = NULL
  fit = lm(sensitivity~.,data=xy.df[-ts.id,])
  error=mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,])))
  rst.vec=c(rst.vec,error)

  
  for (mod in c("adj","ind","cross","aic","bic")){
    
    set = forward.fun(xy.df[-ts.id,],mod=mod)
    fit = lm(sensitivity~.,data=xy.df[-ts.id,c(1,set)])
    rst.vec = c(rst.vec,mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,set]))))
   
    
  }
  rst.tab=rbind(rst.tab,rst.vec)
}


# average vs ranking method
# best -> box plot
mean(rst.tab[,4])
boxplot(rst.tab) # <- 여기에 트랜스포메이션 한거 붙히기 
title("Full model, adj, ind, cross, aic, bic")



new_df=xy.df
fit=lm(sensitivity~.,data=new_df)
summary(fit)
