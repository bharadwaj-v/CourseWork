install.packages("ISLR")
library(ISLR)
oj<-OJ
set.seed(1)
train<-sample(nrow(oj),800)
train.oj<-oj[train,]
test.oj<-oj[-train,]
install.packages("e1071")
library(e1071)
svmfit<-svm(Purchase~.,data=train.oj,kernel="linear",cost=0.01)
summary(svmfit)
train.pred <- predict(svmfit, train.oj)
table(train.oj$Purchase, train.pred)
mean(train.oj$Purchase!=train.pred) #misclassification rate

test.pred <- predict(svmfit, test.oj)
table(test.oj$Purchase, test.pred)
mean(test.oj$Purchase!=test.pred) #misclassification rate

tune.train.out<-tune(svm,Purchase~.,data=train.oj,kernel="linear", ranges=list(cost=c(0.01,0.05,0.1,0.5,1,5,10)))
summary(tune.train.out)

svmfit.linear<-svm(Purchase~.,data=train.oj,kernel="linear",cost=0.1)
summary(svmfit.linear)
train.pred.linear <- predict(svmfit.linear, train.oj)
table(train.oj$Purchase, train.pred.linear)
mean(train.oj$Purchase!=train.pred.linear) #misclassification rate

test.pred.linear <- predict(svmfit.linear, test.oj)
table(test.oj$Purchase, test.pred.linear)
mean(test.oj$Purchase!=test.pred.linear) #misclassification rate


svmfit.poly<-svm(Purchase~.,data=train.oj,kernel="polynomial",cost=0.01,degree=2)
summary(svmfit.poly)
train.pred.poly <- predict(svmfit.poly, train.oj)
table(train.oj$Purchase, train.pred.poly)
mean(train.oj$Purchase!=train.pred.poly) #misclassification rate

test.pred.poly <- predict(svmfit.poly, test.oj)
table(test.oj$Purchase, test.pred.poly)
mean(test.oj$Purchase!=test.pred.poly) #misclassification rate


tune.train.poly<-tune(svm,Purchase~.,data=train.oj,kernel="polynomial",degree=2, ranges=list(cost=c(0.01,0.05,0.1,0.5,1,5,10)))
summary(tune.train.poly)

svmfit.polybest<-svm(Purchase~.,data=train.oj,kernel="polynomial",degree=2,cost=tune.train.poly$best.parameters$cost)
summary(svmfit.polybest)
train.pred.poly <- predict(svmfit.polybest, train.oj)
table(train.oj$Purchase, train.pred.poly)
mean(train.oj$Purchase!=train.pred.poly) #misclassification rate

test.pred.poly <- predict(svmfit.polybest, test.oj)
table(test.oj$Purchase, test.pred.poly)
mean(test.oj$Purchase!=test.pred.poly) #misclassification rate

set.seed(1234)
result<-NULL
for(k in 1:17){
  rf.purchase <- randomForest(Purchase~.,data=train.oj,mtry=k,importance=TRUE)
  rf.pred <- predict(rf.purchase, test.oj)
  table(test.oj$Purchase,rf.pred)
  error<-mean(test.oj$Purchase!=rf.pred)
  result<-rbind(result,c(k,error))
}
plot(result,xlab="number of variables",ylab="Test Misclassification Error", type="a")

rf.purchase <- randomForest(Purchase~.,data=train.oj,mtry=3,importance=TRUE)
rf.pred <- predict(rf.purchase, test.oj)
table(test.oj$Purchase,rf.pred)
mean(test.oj$Purchase!=rf.pred)
