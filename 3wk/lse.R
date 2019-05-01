library(lars)
data(diabetes)
diab.df <- as.data.frame(cbind(diabetes$y,diabetes$x2))

colnames(diab.df) <- c("diab",colnames(diabetes$x2))
colnames(diab.df)
diab.df[1:5,]
lm(diab~.,data=diab.df)$coef


##### least square estimator
x.mat <- cbind(1,as.matrix(diabetes$x)); y.vec <- as.vector(diabetes$y)
x.mat
#손계산
# solve == 역행렬, drop 은 약간 flatten 느낌
drop(solve(t(x.mat)%*%x.mat)%*%t(x.mat)%*%y.vec)


#라이브러리
diab.df <- as.data.frame(cbind(diabetes$y,diabetes$x)); colnames(diab.df)[1] <- "diab"
lm(diab~.,data=diab.df)$coef
