getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/6wk")
xy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.fit.csv")
nxy.df= read.csv("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/old.sam.for.reg.pred.csv")



colnames(xy.df)[1]='y'
colnames(nxy.df)[1]='y'
p = dim(xy.df)[2]; n = dim(xy.df)[1]
#first step
a.set = NULL  #active set : 현재 모형에 포함되어야 하는 변수의 셋
c.set = 2:p # 추가할 대상이 되는 변수의 셋/ 1은 종속변수니깐
e.vec =  NULL # 에러 벡터
n.fold=5
crs.id = split(sample(1:n),1:n.fold)


for(id in c.set){
  d.set = c(1,id)
  cv_err = 0
  
  for(k in 1:n.fold){
    temp_fold=crs.id[[k]]
    lm.fit = lm(y~.,data=xy.df[-temp_fold,d.set])
    ny = predict(lm.fit,newdata=xy.df[temp_fold,d.set])  
    cv_err = cv_err + mean(abs(ny-xy.df[temp_fold,1]))
  }
  e.vec=c(e.vec,cv_err)
}

opt = which.min(e.vec); e.opt = min(e.vec)
opt
e.opt

for (iid in 3:p){
  print(iid)
  a.set = c(a.set,c.set[opt])
  c.set = c.set[-opt] ;e.vec = NULL
  for(id in c.set){
    d.set = c(1,a.set,id)
    cv_err = 0
    
    for(k in 1:n.fold){
      temp_fold=crs.id[[k]]
      lm.fit = lm(y~.,data=xy.df[-temp_fold,d.set])
      ny = predict(lm.fit,newdata=xy.df[temp_fold,d.set])
      cv_err = cv_err + mean(abs(ny-xy.df[temp_fold,1]))
    }
    e.vec=c(e.vec,cv_err)
  }
  opt = which.min(e.vec) ;
  if(e.opt<min(e.vec)) break
  e.opt = min(e.vec)
}
a.set
