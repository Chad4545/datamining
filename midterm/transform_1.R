############################################################
#            Randomization
# 1. 데이터 합치고
# 2. 데이터 랜덤하게 짜르고 
# 3. 하나는 fit하고 하나는 pred 하고
# 4. 1로 돌아가서 다시 
############################################################
getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/7wk_2")
rm(list=ls())
source("r_functions_advanced.R")

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
v3 = xy.df[,6]
v5 = xy.df[,8]

type_1_v2 = sqrt(abs(v2))         
type_1_v3 = v3^2


type_2_v2 = v2^2
type_2_v3 = log((v3-min(v3))/(max(v3)-min(v3))+0.001)


type_3_v2 = log(abs(v2))
type_3_v3 = log(abs(v3))
type_3_v5 = log(abs(v5))

type_4_v2 = log(abs(v2))
type_4_v3 = v3^2



new_df_1=xy.df
new_df_1[,5]=type_1_v2
new_df_1[,6]=type_1_v3

new_df_2=xy.df
new_df_2[,5]=type_2_v2
new_df_2[,6]=type_2_v3


new_df_3=xy.df
new_df_3[,5]=type_3_v2
new_df_3[,6]=type_3_v3
new_df_3[,8]=type_3_v5

new_df_4=xy.df
new_df_4[,5]=type_4_v2
new_df_4[,6]=type_4_v3


ran.num = 100; rst.tab =NULL
for(i in 1:100){
  print(i)
  ts.id = sample(1:dim(xy.df)[1])[1:300]
  rst.vec = NULL
  # full
  fit = lm(sensitivity~.,data=xy.df[-ts.id,])
  error=mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,])))
  rst.vec=c(rst.vec,error)
  print(rst.vec)
  
  # type 1
  fit = lm(sensitivity~.,data=new_df_1[-ts.id,])
  error=mean(abs(new_df_1[ts.id,1]-predict(fit,newdata=new_df_1[ts.id,])))
  rst.vec=c(rst.vec,error)
  print(rst.vec)
  # type 2
  fit = lm(sensitivity~.,data=new_df_2[-ts.id,])
  error=mean(abs(new_df_2[ts.id,1]-predict(fit,newdata=new_df_2[ts.id,])))
  rst.vec=c(rst.vec,error)
  print(rst.vec)
  #type 3
  fit = lm(sensitivity~.,data=new_df_3[-ts.id,])
  error=mean(abs(new_df_3[ts.id,1]-predict(fit,newdata=new_df_3[ts.id,])))
  rst.vec=c(rst.vec,error)
  print(rst.vec)
  #type 4
  fit = lm(sensitivity~.,data=new_df_4[-ts.id,])
  error=mean(abs(new_df_4[ts.id,1]-predict(fit,newdata=new_df_4[ts.id,])))
  rst.vec=c(rst.vec,error)
  print(rst.vec)
  rst.tab=rbind(rst.tab,rst.vec)
}


# average vs ranking method
# best -> box plot

boxplot(rst.tab) # <- 여기에 트랜스포메이션 한거 붙히기 
title("Original - model_1 - model_2 - model_3 - model_4")
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


ran.num = 100; rst.tab =NULL
for(i in 1:100){
  print(i)
  ts.id = sample(1:dim(xy.df)[1])[1:300]
  
  # Full model 
  rst.vec = NULL
  fit = lm(sensitivity~.,data=xy.df[-ts.id,])
  error=mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,])))
  rst.vec=c(rst.vec,error)
  
  # type 1
  fit = lm(sensitivity~.,data=new_df_1[-ts.id,])
  error=mean(abs(new_df_1[ts.id,1]-predict(fit,newdata=new_df_1[ts.id,])))
  rst.vec=c(rst.vec,error)
  print(rst.vec)
  
  for (mod in c("adj","ind","cross","aic","bic")){
    
    set = forward.fun(xy.df[-ts.id,],mod=mod)
    fit = lm(sensitivity~.,data=xy.df[-ts.id,c(1,set)])
    rst.vec = c(rst.vec,mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,set]))))
    
    
  }
  # type 2
  fit = lm(sensitivity~.,data=new_df_2[-ts.id,])
  error=mean(abs(new_df_2[ts.id,1]-predict(fit,newdata=new_df_2[ts.id,])))
  rst.vec=c(rst.vec,error)
  print(rst.vec)
  
  for (mod in c("adj","ind","cross","aic","bic")){
    
    set = forward.fun(xy.df[-ts.id,],mod=mod)
    fit = lm(sensitivity~.,data=xy.df[-ts.id,c(1,set)])
    rst.vec = c(rst.vec,mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,set]))))
    
    
  }
  #type 3
  fit = lm(sensitivity~.,data=new_df_3[-ts.id,])
  error=mean(abs(new_df_3[ts.id,1]-predict(fit,newdata=new_df_3[ts.id,])))
  rst.vec=c(rst.vec,error)
  print(rst.vec)
  
  for (mod in c("adj","ind","cross","aic","bic")){
    
    set = forward.fun(xy.df[-ts.id,],mod=mod)
    fit = lm(sensitivity~.,data=xy.df[-ts.id,c(1,set)])
    rst.vec = c(rst.vec,mean(abs(xy.df[ts.id,1]-predict(fit,newdata=xy.df[ts.id,set]))))
    
    
  }
  #type 4
  fit = lm(sensitivity~.,data=new_df_4[-ts.id,])
  error=mean(abs(new_df_4[ts.id,1]-predict(fit,newdata=new_df_4[ts.id,])))
  rst.vec=c(rst.vec,error)
  print(rst.vec)
  rst.tab=rbind(rst.tab,rst.vec)
  
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

