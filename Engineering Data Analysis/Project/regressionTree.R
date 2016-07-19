
library(randomForest)
set.seed(5)
par(mfrow = c(1,1))
result<-NULL
#loop the k to find the k with least error
for(k in 1:22){
  rf.model<-randomForest(violentPerPop~.,crimeViolenceFinalData1[train,],mtry=k,importance=TRUE,ntree=1501)
  pred.value<-predict(rf.model,crimeViolenceFinalData1[-train,])
  error<-mean((pred.value-crimeViolenceFinalData1[-train,]$violentPerPop)^2)
  result<-rbind(result,c(k,error))
}
plot(result,xlab="number of variables",ylab="Root mean squared error",type="b")

#Random forest
set.seed(1111)
rf.model<-randomForest(violentPerPop~.,crimeViolenceFinalData1[train,],mtry=14,importance=TRUE,ntree=1501)
pred.value<-predict(rf.model,crimeViolenceFinalData1[-train,])
mean((pred.value-crimeViolenceFinalData1[-train,]$violentPerPop)^2)
plot(rf.model,log="y") 
importance(rf.model)
varImpPlot(rf.model)
getTree(rf.model,1,labelVar = TRUE)
treesize(rf.model, terminal=TRUE)

#Tree
set.seed(333)
tree.data<-tree(violentPerPop~.,crimeViolenceFinalData1[train,])
par(mfrow=c(1,2))
plot(tree.data)
text(tree.data,pretty=0,xpd=TRUE)
summary(tree.data)
tree.pred<-predict(tree.data ,crimeViolenceFinalData1[-train,])
mean((tree.pred-crimeViolenceFinalData1[train,]$violentPerPop)^2)


tree.data<-tree(violentPerPop~.,crimeViolenceFinalData1[train,])
cv.data =cv.tree(tree.data,FUN=prune.tree)
par(mfrow =c(1,2))
plot(cv.data$size ,cv.data$dev ,type="b")
plot(cv.data$k ,cv.data$dev ,type="b")
cv.data

prune.data =prune.tree (tree.data,best = 7)
par(mfrow =c(1,1))
plot(prune.data)
text(prune.data,pretty=0,xpd=TRUE)
tree.pred1=predict(prune.data,crimeViolenceFinalData1[-train,])
mean((tree.pred1-crimeViolenceFinalData1[-train,]$violentPerPop)^2)
