getwd()
#setwd("/Users/sungjinpark/Desktop/OneDrive - konkuk.ac.kr/datamining/homework/4")
# data
xy.df = read.csv(file='../4/old.sam.for.reg.fit.csv')
nxy.df = read.csv(file='../4/old.sam.for.reg.pred.csv')

###
colnames(xy.df)
# example 1 variable vs 2 variables vs 3variables
r.sq.1 = summary(lm(formula = sensitivity~.,data=xy.df[,1:2]))$r.sq
r.sq.2 = summary(lm(formula = sensitivity~.,data=xy.df[,1:3]))$r.sq
r.sq.3 = summary(lm(formula = sensitivity~.,data=xy.df[,1:4]))$r.sq

# 
rs.mat = matrix(NA,3,20)
rownames(rs.mat)=c("numbers","R^2","adjR^2")


for(i in 2:21){
  rs.mat[1,i-1] = i-1
  rs.mat[2,i-1] = summary(lm(formula = sensitivity~.,data=xy.df[,1:i]))$r.sq
  rs.mat[3,i-1] = summary(lm(formula = sensitivity~.,data=xy.df[,1:i]))$adj.r.sq
}
df=as.data.frame(t(rs.mat))




plot(df$numbers,df$`R^2`,ylab="",xlab="the number of variables",ylim=c(0,0.2),pch=4,type="o",col="#228B22")
par(new=T)
plot(df$numbers,df$`adjR^2`,ylab="",xlab="",ylim=c(0,0.2),pch=0,type="o",col="#FF3030")
par(new=T)
legend(x=16,y=0.20, c("Rsquare","Adj_Rsquare"), cex=0.7, pch=c(2,0,4),col=c("#228B22","#FF3030"))
title(main = "R square VS Adj R square")


