library(ISLR)
set.seed(1)
attach(Carseats)
train<-sample(nrow(Carseats),nrow(Carseats)/2)
carseats.train<-Carseats[train,]
carseats.test<-Carseats[-train,]

install.packages("tree")
library(tree)
tree.carseats<-tree(Sales~.,Carseats,subset=train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0,xpd=TRUE)
Sales.train<-carseats.train[,c("Sales")]
Sales.test<-carseats.test[,c("Sales")]
Sales.Hat<-predict(tree.carseats,carseats.test)
mean((Sales.Hat - carseats.test$Sales)^2)

set.seed(1)
cv.carseats<-cv.tree(tree.carseats,FUN=prune.tree)
cv.carseats


#visualize the results
par(mfrow=c(1,1))
plot(cv.carseats$size,cv.carseats$dev,type="b")
tree.min <- which.min(cv.carseats$dev)
points(tree.min-2, cv.carseats$dev[tree.min], col = "red", cex = 2, pch = 20)
plot(cv.carseats$k, cv.carseats$dev, type = "b")

#prune.misclass() based on cv results
prune.carseats= prune.tree(tree.carseats,best=8)
plot(prune.carseats)
text(prune.carseats,pretty=0,xpd=TRUE)
summary(prune.carseats)


#test pruned tree
prune.sales.Hat<-predict(prune.carseats,carseats.test)
mean((prune.sales.Hat - carseats.test$Sales)^2)

#bagging the tree
bagging.sales<-randomForest(Sales~.,data=carseats.train,mtry=10,importance=T)
bagging.sales
bagging.pred = predict(bagging.sales, carseats.test)
plot(bagging.pred,carseats.test)
mean((bagging.pred-carseats.test$Sales)^2)
importance(bagging.sales)

#random forests
set.seed(2)
rf.carseats= randomForest(Sales~.,data=carseats.train,mtry=4,importance=TRUE)
saleshat.rf <- predict(rf.carseats, carseats.test)
mean((saleshat.rf - carseats.test$Sales)^2)

rf.carseats= randomForest(Sales~.,data=carseats.train,mtry=3,importance=TRUE)
saleshat.rf <- predict(rf.carseats, carseats.test)
mean((saleshat.rf - carseats.test$Sales)^2)

plot(rf.carseats)
text(rf.carseats,pretty=0,xpd=TRUE)