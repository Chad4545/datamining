##### diabetes samples
install.packages("lars")
library(lars);
data(diabetes);# 분명히 y, x,x2 만 있는데
names(diabetes)
diab.df <- as.data.frame(cbind(diabetes$y,diabetes$x)) 
# 데이터 프레임에 넣는순간 설명변수가 10개로 증가... ?
diab.df
colnames(diab.df) <- c("diab",colnames(diabetes$x))
##### least square estimation
diab.mod <- lm(diab~.,data=diab.df)
##### analysis of variance and other statistical results
summary(diab.mod)
##### final fitted model
diab.mod$coef
