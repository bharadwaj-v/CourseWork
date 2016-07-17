library(MASS)
data(Boston)
names(Boston)

lm.fit<-lm(medv~rm,Boston)
summary(lm.fit)
coef(lm.fit)


lm.fitlogrm<-lm(medv~log(rm),data=Boston)
summary(lm.fitlogrm)
plot(log(rm),medv,col="blue")
abline(lm.fitlogrm,col="red")

lm.fitlstat<-lm(medv~lstat,data=Boston)
summary(lm.fitlstat)