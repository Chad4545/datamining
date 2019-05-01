setwd("../homework/3")
getwd()
rm(list=ls())
# data load 
# train_data
df<-read.csv(file="../../old.sam.for.reg.fit.csv")
tr.df = df[,c(1,4)]
#test_data
test.df<-read.csv(file="../../old.sam.for.reg.pred.csv")
te.df =test.df[,c(1,4)]
# result matrix
rst.mat = matrix(NA,2,3)
colnames(rst.mat)=c("lm","rq","hub")
rownames(rst.mat)=c("AIC","scaled_error")
rst.mat

#             lm rq hub
#AIC          NA NA  NA
#scaled_error NA NA  NA



#models = simple linear regression + L2, L1, huber
library(robustreg)
library(quantreg)
###############################################################3
#L2
lm.fit = lm(formula = sensitivity~V1,data=tr.df)
#L1
rq.fit=rq(formula = sensitivity~V1,data=tr.df)
#Huber
rb.fit=robustRegH(formula = sensitivity~V1,data=tr.df)
rb.fit
#######################################################################




# implement AIC and Scaled Error
aic.fun <- function(loss){
  sse = sum((loss)^2)
  return(log(sse/length(loss)) + 2*2/length(loss))
}

scaled_error.fun <- function(loss){
  sqrt(sum((loss)^2)/length(loss))
}

#L2

test_pred=cbind(1,te.df[,2])%*%coef(lm.fit) #pred 값
test_pred

loss=te.df[,1]-test_pred                    # 실제값 - pred값 : loss
aic.lm = aic.fun(loss)
scaled_error=scaled_error.fun(loss)

rst.mat[1,1]=aic.lm
rst.mat[2,1]=scaled_error
rst.mat    

#L1
test_pred=cbind(1,te.df[,2])%*%coef(rq.fit)
loss=te.df[,1]-test_pred
aic.rq = aic.fun(loss)
scaled_error=scaled_error.fun(loss)

rst.mat[1,2]=aic.rq
rst.mat[2,2]=scaled_error
rst.mat    
#Huber
test_pred=cbind(1,te.df[,2])%*%coef(rb.fit)
test_pred
loss=te.df[,1]-test_pred
aic.hub = aic.fun(loss)
scaled_error=scaled_error.fun(loss)

rst.mat[1,3]=aic.hub
rst.mat[2,3]=scaled_error
rst.mat    

# grid search for [L1] & Huber
L1.mat = matrix(NA,3,100)

rownames(L1.mat)=c("Tau","AIC","error")
L1.mat
# tau 
tau.tune=seq(0,1,length.out = 100)

for(j in 1:100){
    L1.mat[1,j] = tau.tune[j]
    rq.fit=rq(formula = sensitivity~V1,tau=tau.tune[j] ,data=tr.df)
    
    test_pred=cbind(1,te.df[,2])%*%coef(rq.fit)
    loss=te.df[,1]-test_pred
    aic.rq = aic.fun(loss)
    L1.mat[2,j]=aic.rq

    square_loss=sqrt(sum((loss)^2)/dim(te.df)[1])
    L1.mat[3,j]=square_loss
}


L1.mat

# grid search for L1 & [Huber]
hub.mat = matrix(NA,3,100)
rownames(hub.mat)=c("delta","AIC","Prediction")
#delta
hub.tune=seq(0.5,1.5,length.out = 100)

for(j in 1:100){
  hub.mat[1,j] = hub.tune[j]
  rb.fit=robustRegH(formula = sensitivity~V1,tune=hub.tune[j] ,data=tr.df)

  test_pred=cbind(1,te.df[,2])%*%coef(rb.fit)
  loss=te.df[,1]-test_pred
  aic.hub = aic.fun(loss)
  hub.mat[2,j]=aic.hub
  
  square_loss=sqrt(sum((loss)^2)/dim(te.df)[1])
  hub.mat[3,j]=square_loss
  
}



##############################################################
# result matrix
tuning.mat = matrix(NA,4,3)
colnames(tuning.mat)=c("lm","rq","hub")
rownames(tuning.mat)=c("best_Tau","Best_delta","AIC","scaled_error")
tuning.mat

##############################################################
matplot(hub.mat[1,],hub.mat[2,],type="h",
        ylab = expression("AIC"),
        xlab = expression("Delta"))
title(main = "Delta in Huber & AIC")
matplot(hub.mat[1,],hub.mat[3,],type="h",
        ylab = expression("AIC"),
        xlab = expression("Loss"))
title(main = "Delta in Huber & Loss")

minaic = min(hub.mat[2,])
tuning.mat[3,3] = minaic
minaic # 5.035846
minloss = min(hub.mat[3,]) 
tuning.mat[4,3]=minloss
minloss # 12.15722
inds = which(hub.mat[2,] == min(hub.mat[2,]), arr.ind=TRUE)
inds
inds = which(hub.mat[3,] == min(hub.mat[3,]), arr.ind=TRUE)
inds
tuning.mat[2,3]=hub.mat[,inds][1]
#hub.mat[,inds]
#delta        AIC Prediction 
#0.9444444  5.0358462 12.1572186 
##############################################################
matplot(L1.mat[1,],L1.mat[2,],type="h",
        ylab = expression("AIC"),
        xlab = expression("Tau"))
title(main = "Tau in L1 & AIC")

matplot(L1.mat[1,],L1.mat[3,],type="h",
        ylab = expression("Loss"),
        xlab = expression("Tau"))
title(main = "Tau in L1 & Loss")

minaic = min(L1.mat[2,])
tuning.mat[3,2] = minaic
minaic # 5.033593
minloss = min(L1.mat[3,]) 
tuning.mat[4,2] = minloss
minloss # 12.15773
inds = which(L1.mat[2,] == min(L1.mat[2,]), arr.ind=TRUE)
inds
inds = which(L1.mat[3,] == min(L1.mat[3,]), arr.ind=TRUE)
inds

tuning.mat[1,2] = L1.mat[,inds][1]
#Tau        AIC Prediction 
#0.4949495  5.0359300 12.1577278 
##############################################################
tuning.mat[3,1]=aic.lm
tuning.mat[4,1]=scaled_error
result = as.data.frame(tuning.mat)
result
