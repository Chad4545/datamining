
### This is a sample answer sheet for the mid-exam
rm(list=ls())
getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/exam")
xy.df = read.csv(file="mid.exam.csv")
names(xy.df)[1]='target'
head(xy.df)

##### Question A ################################################# 
#(Usual forward selection)#
# 1)
A.fun = function(xy.df){
  # fill in your answers here 
  p = dim(xy.df)[2] # number of Explanatory variable
  n = dim(xy.df)[1] # number of samples
  rst = NULL
  
  # first step 
  a.set = NULL;
  c.set = 4:p; # skip [target, typem pressure] 
  e.vec = NULL
  for(id in c.set){
    d.set = c(1,id); 
    lm.fit = lm(target~.,data=xy.df[,d.set])
    e.vec=c(e.vec,AIC(lm.fit))
  }
  
  opt = which.min(e.vec) # first opt
  e.opt = min(e.vec)
  
  # iteration
  for(iid in 5:p){
    a.set = c(a.set,c.set[opt])
    c.set = c.set[-opt]; e.vec = NULL 
    for(id in c.set){
      d.set = c(1,a.set,id); 
      lm.fit = lm(target~.,data=xy.df[,d.set])
      e.vec = c(e.vec,AIC(lm.fit))
    }
    opt = which.min(e.vec); 
    if(e.opt<min(e.vec)) break 
    e.opt = min(e.vec)
  }
  ret=a.set-3
  return(ret)
}
A.fun(xy.df) # a part of variable indices from 1 to 18 


##### Question B ################################################# 
b.vec = rep(NA,18); aic.vec = rep(NA,18)

# 1) 
fit = lm(target~.,data=xy.df[,c(1,2,3,4)])
b.vec[1] = abs(coef(fit)[5])
b.vec 

# 2) 
# fill in your answers here 
fit = lm(target~.,data=xy.df[,c(1,2,3,5)])
b.vec[2] = abs(coef(fit)[5])
b.vec 

# 3) 
# fill in your answers here 
for(i in 6:21){
  fit = lm(target~.,data=xy.df[,c(1,2,3,i)])
  b.vec[i-3] = abs(coef(fit)[5])
}
b.vec 
o.vec = order(b.vec,decreasing=TRUE)
o.vec 

# 4) 
id = o.vec[1]+3
fit = lm(target~.,data=xy.df[,c(1,2,3,id)])
aic.vec[1] = AIC(fit)
aic.vec

# 5) 
# fill in your answers here 
id = c(id,o.vec[2]+3)
fit = lm(target~.,data=xy.df[,c(1,2,3,id)])
aic.vec[2] = AIC(fit)
aic.vec

# 6) 
# fill in your answers here 
for(i in 3:18){
  id = c(id,o.vec[i]+3)
  fit = lm(target~.,data=xy.df[,c(1,2,3,id)])
  aic.vec[i] = AIC(fit)
}
aic.vec

# 7) 
B.fun = function(xy.df){
  b.vec = aic.vec = rep(NA,18)
  # fill in your answers here
  for(i in 4:21){
    fit = lm(target~.,data=xy.df[,c(1,2,3,i)])
    b.vec[i-3] = abs(coef(fit)[5])
  }
  o.vec = order(b.vec,decreasing=TRUE)
  # fill in your answers here
  id = NULL
  for(i in 1:18 ){
    id = c(id,o.vec[i]+3)
    fit = lm(target~.,data=xy.df[,c(1,2,3,id)])
    aic.vec[i] = AIC(fit)
    aic.vec
  }
  opt = which.min(aic.vec)
  ret = o.vec[1:opt]
  return(ret)
}
B.fun(xy.df)

##### Question C ################################################# 

aic.vec = rep(NA,18); b.mat = matrix(NA,50,18)

# 1) 
# bootstrap samples.
n = dim(xy.df)[1];
b.id = sample(1:n,n,replace=TRUE);
bxy.df = xy.df[b.id,]

fit = lm(target~.,data=bxy.df)
b.mat[1,] = coef(fit)[-c(1,2,3,4)]
b.mat[1,]

# 2) 
# fill in your answers here 
b.mat = matrix(NA,50,18)
for(i in 1:50){
  b.id = sample(1:n,n,replace=TRUE); #boostrap
  bxy.df = xy.df[b.id,]
  fit = lm(target~.,data=bxy.df) # using bxy.df with dupulicated sampling
  b.mat[i,] = coef(fit)[-c(1,2,3,4)]
}
b.mat
o.vec = order(abs(colMeans(b.mat)),decreasing=TRUE) 
o.vec

# 3) 
# fill in your answers here 
id = o.vec[1]+3
fit = lm(target~.,data=xy.df[,c(1,2,3,id)])
aic.vec[1] = AIC(fit)
aic.vec

# 4) 
# fill in your answers here 
id = c(id,o.vec[2]+3)
fit = lm(target~.,data=xy.df[,c(1,2,3,id)])
aic.vec[2] = AIC(fit)
aic.vec


# 5) 
# fill in your answers here 
for(i in 3:18){
  id = c(id,o.vec[i]+3)
  fit = lm(target~.,data=xy.df[,c(1,2,3,id)])
  aic.vec[i] = AIC(fit)
  
}

aic.vec

# 6) 
C.fun = function(xy.df){
  aic.vec = rep(NA,18); b.mat = matrix(NA,50,18)
  # fill in your answers here
  for(i in 1:50){
    b.id = sample(1:n,n,replace=TRUE); #boostrap
    bxy.df = xy.df[b.id,]
    fit = lm(target~.,data=bxy.df) # using bxy.df with dupulicated sampling
    b.mat[i,] = coef(fit)[-c(1,2,3,4)]
  }
  b.mat
  
  o.vec = order(abs(colMeans(b.mat)),decreasing=TRUE)
  # fill in your answers here
  id = NULL
  for(i in 1:18){
    id = c(id,o.vec[i]+3)
    fit = lm(target~.,data=xy.df[,c(1,2,3,id)])
    aic.vec[i] = AIC(fit)
  }
  
  opt = which.min(aic.vec)
  ret = o.vec[1:opt]
  return(ret)
}
C.fun(xy.df)

##### Question D ################################################# 

# 1) 
set.seed(1); e.vec = rep(NA,3)
# fill in your answers here
tr.id = sample(1:dim(xy.df)[1])[1:800]

a.train=A.fun(xy.df[tr.id,]);a.train
b.train=B.fun(xy.df[tr.id,]);b.train
c.train=C.fun(xy.df[tr.id,]);c.train

a.fit = lm(target~.,data=xy.df[tr.id,c(1,2,3,c(a.train)+3)])
b.fit = lm(target~.,data=xy.df[tr.id,c(1,2,3,c(b.train)+3)])
c.fit = lm(target~.,data=xy.df[tr.id,c(1,2,3,c(c.train)+3)])

a.pred = predict(a.fit,newdata=xy.df[-tr.id,c(1,2,3,c(a.train)+3)])
b.pred = predict(b.fit,newdata=xy.df[-tr.id,c(1,2,3,c(b.train)+3)])
c.pred = predict(c.fit,newdata=xy.df[-tr.id,c(1,2,3,c(c.train)+3)])

a_err = mean((xy.df[-tr.id,1]-a.pred)^2)
b_err = mean((xy.df[-tr.id,1]-b.pred)^2)
c_err = mean((xy.df[-tr.id,1]-c.pred)^2)
e.vec = c(a_err,b_err,c_err)
e.vec 

# 2) 
set.seed(1); e.mat = matrix(NA,50,3)
# fill in your answers here
for(i in 1:50){
  tr.id = sample(1:dim(xy.df)[1])[1:800]
  e.vec = rep(NA,3)
  for(j in 1:3){
    if(j==1){
      train=A.fun(xy.df[tr.id,])
      fit = lm(target~.,data=xy.df[tr.id,c(1,2,3,c(train)+3)])
      pred = predict(fit,newdata=xy.df[-tr.id,c(1,2,3,c(train)+3)])
      err = mean((xy.df[-tr.id,1]-pred)^2)
      e.vec[1]=err
    }
    else if(j==2){
      train=B.fun(xy.df[tr.id,])
      fit = lm(target~.,data=xy.df[tr.id,c(1,2,3,c(train)+3)])
      pred = predict(fit,newdata=xy.df[-tr.id,c(1,2,3,c(train)+3)])
      err = mean((xy.df[-tr.id,1]-pred)^2)
      e.vec[2]=err
    }
    else if(j==3){
      train=C.fun(xy.df[tr.id,])
      fit = lm(target~.,data=xy.df[tr.id,c(1,2,3,c(train)+3)])
      pred = predict(fit,newdata=xy.df[-tr.id,c(1,2,3,c(train)+3)])
      err = mean((xy.df[-tr.id,1]-pred)^2)
      e.vec[3]=err
    }
    e.mat[i,]=e.vec
  }
  
}
e.mat 
colnames(e.mat)=c("A","B","C")
boxplot(e.mat)
title("Before Transformation")

##### Question E ################################################# 
# fill in your answers here
trans_list=list()
count=0
for(i in 1:4){
  for(j in 1:4){
    for(k in 1:4){
      for(l in 1:4){
        count = count + 1
        trans_list[[count]]=NA
        trans_list[[count]]=c(i,j,k,l)
      }
    }
  }
}
trans_list


trans_col=function(df,candidate){
  new_df=df[,8:11]
  count=0
  for(i in candidate){
    count = count + 1
    if(count==1){
      if(i==1){
        new_df[,1]=df[,8]
      }
      else if(i==2){
        new_df[,1]=(df[,8]^2)
      }
      else if(i==3){
        new_df[,1]=log(abs(df[,8]))
      }
      else if(i==4){
        new_df[,1]=sqrt(abs(df[,8]))
      }
    }
    else if(count==2){
      
      if(i==1){
        new_df[,2]=df[,9]
      }
      else if(i==2){
        new_df[,2]=(df[,9]^2)
      }
      else if(i==3){
        new_df[,2]=log(abs(df[,9]))
      }
      else if(i==4){
        new_df[,2]=sqrt(abs(df[,9]))
      }
    }
    else if(count==3){
      
      if(i==1){
        new_df[,3]=df[,10]
      }
      else if(i==2){
        new_df[,3]=(df[,10]^2)
      }
      else if(i==3){
        new_df[,3]=log(abs(df[,10]))
      }
      else if(i==4){
        new_df[,3]=sqrt(abs(df[,10]))
      }
    }
    else if(count==4){
    
      if(i==1){
        new_df[,4]=df[,11]
      }
      else if(i==2){
        new_df[,4]=(df[,11]^2)
      }
      else if(i==3){
        new_df[,4]=log(abs(df[,11]))
      }
      else if(i==4){
        new_df[,4]=sqrt(abs(df[,11]))
      }
    }
  }
  df[,8:11]=new_df
  return(df)
}    
 

e.mat_A = matrix(NA,50,256)
e.mat_B = matrix(NA,50,256)
e.mat_C = matrix(NA,50,256)
# fill in your answers here
for(i in 1:50){
  print(i)
  tr.id = sample(1:dim(xy.df)[1])[1:800]
  e.vec_A = rep(NA,256)
  e.vec_B = rep(NA,256)
  e.vec_C = rep(NA,256)
  for(j in 1:3){
    if(j==1){
      print("---------------")
      print("A")
      for(k in 1:256){
        print(k)
        trans_candidate=trans_list[[k]] # 1 1 1 1
        new_df = trans_col(xy.df,trans_candidate)
        train=A.fun(new_df[tr.id,])
        fit = lm(target~.,data=new_df[tr.id,c(1,2,3,c(train)+3)])
        pred = predict(fit,newdata=new_df[-tr.id,c(1,2,3,c(train)+3)])
        err = mean((new_df[-tr.id,1]-pred)^2)
        e.vec_A[k]=err
      }
      e.mat_A[i,]=e.vec_A
    }
    else if(j==2){
      print("---------------")
      print("B")
      for(k in 1:256){
        print(k)
        trans_candidate=trans_list[[k]] # 1 1 1 1
        new_df = trans_col(xy.df,trans_candidate)
        train=B.fun(new_df[tr.id,])
        fit = lm(target~.,data=new_df[tr.id,c(1,2,3,c(train)+3)])
        pred = predict(fit,newdata=new_df[-tr.id,c(1,2,3,c(train)+3)])
        err = mean((new_df[-tr.id,1]-pred)^2)
        e.vec_B[k]=err
      }
      e.mat_B[i,]=e.vec_B
    }
    else if(j==3){
      print("---------------")
      print("C")
      for(k in 1:256){
        print(k)
        trans_candidate=trans_list[[k]] # 1 1 1 1
        new_df = trans_col(xy.df,trans_candidate)
        train=C.fun(new_df[tr.id,])
        fit = lm(target~.,data=new_df[tr.id,c(1,2,3,c(train)+3)])
        pred = predict(fit,newdata=new_df[-tr.id,c(1,2,3,c(train)+3)])
        err = mean((new_df[-tr.id,1]-pred)^2)
        e.vec_C[k]=err
      }
      e.mat_C[i,]=e.vec_C
    }
  }
}

#write.csv(e.mat_A,"error_matrix_A.csv")
#write.csv(e.mat_B,"error_matrix_B.csv")
#write.csv(e.mat_C,"error_matrix_C.csv")
?read.csv()
e.mat_A=read.csv("error_matrix_A.csv",row.names=1)
e.mat_B=read.csv("error_matrix_B.csv",row.names=1)
e.mat_C=read.csv("error_matrix_C.csv",row.names=1)

best_trans_A=which.min(colMeans(e.mat_A))#81
best_trans_B=which.min(colMeans(e.mat_B))#81
best_trans_C=which.min(colMeans(e.mat_C))#81

#trans_list[81] == 2 2 1 1
'''
V5 => square
V6 => square
V7 => No transformation
V8 => No transformation
'''

set.seed(1); 
transform_error.mat=matrix(NA,50,3)
trans_df = xy.df
trans_df[,8]=xy.df[,8]^2
trans_df[,9]=xy.df[,9]^2
for(i in 1:50){
  tr.id = sample(1:dim(trans_df)[1])[1:800]
  e.vec = rep(NA,3)
  for(j in 1:3){
    if(j==1){
      train=A.fun(trans_df[tr.id,])
      fit = lm(target~.,data=trans_df[tr.id,c(1,2,3,c(train)+3)])
      pred = predict(fit,newdata=trans_df[-tr.id,c(1,2,3,c(train)+3)])
      err = mean((trans_df[-tr.id,1]-pred)^2)
      e.vec[1]=err
    }
    else if(j==2){
      train=B.fun(trans_df[tr.id,])
      fit = lm(target~.,data=trans_df[tr.id,c(1,2,3,c(train)+3)])
      pred = predict(fit,newdata=trans_df[-tr.id,c(1,2,3,c(train)+3)])
      err = mean((trans_df[-tr.id,1]-pred)^2)
      e.vec[2]=err
    }
    else if(j==3){
      train=C.fun(trans_df[tr.id,])
      fit = lm(target~.,data=trans_df[tr.id,c(1,2,3,c(train)+3)])
      pred = predict(fit,newdata=trans_df[-tr.id,c(1,2,3,c(train)+3)])
      err = mean((trans_df[-tr.id,1]-pred)^2)
      e.vec[3]=err
    }
    transform_error.mat[i,]=e.vec
  }
  
}
colnames(transform_error.mat)=c("A","B","C")
boxplot(transform_error.mat)
title("After Transformation")
