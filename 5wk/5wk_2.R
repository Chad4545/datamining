# 2019.04.03 수 
getwd()
setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/5wk")
xy.df = read.csv('../old.sam.for.reg.fit.csv')
nxy.df = read.csv('../old.sam.for.reg.pred.csv')


rm(list=ls())
num_variables = dim(xy.df)[2] - 1 # 설명변수의 개수 20개
result_error = matrix(NA,3,num_variables) # 결과 매트릭스
rownames(result_error)=c("Num of Variables","selected Variables","pred_error")
picked=c()# 선택한 변수 모아둘 벡터


v = seq(1,num_variables) # 선택할 변수들 
count = 0 # iter



for (j in 1:num_variables){ # 1:20
  result_error[1,j] = j # Num of variables  
  error_vector=rep(NA,num_variables) # 무조건 길이 20
  prev_error=0
  for (i in v){
    if(i==0){  # 선택한 변수자리의 값 0으로 바꿔줄거임 
      error_vector[i] = 5000
    }
  
    else{
      d.set = c(i,picked) + 1 # 실제 변수위치는 + 1
      lm.fit = lm(sensitivity ~. , data = xy.df[,c(1,d.set)])
      ny = predict(lm.fit, newdata = nxy.df[,c(1,d.set)])
      err = mean(abs(nxy.df[,1]-ny))
      error_vector[i] = err
      d.set =c() # d.set 다시 초기화
    }
    }
    
  print("-----selected_variable-----")
  selected_variable = which.min(error_vector) # 가장 작은 에러 == select
  print(selected_variable + 1) # 실제 변수의 위치 == + 1

  print("-----picked variables-----")
  picked = c(picked,selected_variable) # 차례대로 뽑힌 변수들
  print(picked + 1) 
  
  result_error[2,j] = selected_variable + 1
  pred_error = min(error_vector,na.rm = TRUE)
  result_error[3,j] = pred_error
  
  v[selected_variable]=0
  print("-----left variables-----")
  print(v) # 남은 변수들 
  
}
result_error  
