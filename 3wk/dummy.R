## make dummy variable

x = sample(x=c('Vision','NLP','ML'),size=10,replace = TRUE)
x
ux = unique(x)
ux=ux[-1]  #baseline

result = t(t(matrix(rep(x,length(ux)),ncol = length(ux)))==ux)*1
colnames(result)=ux

cbind(result,x)
