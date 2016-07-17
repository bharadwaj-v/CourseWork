glm.fit<-glm(Direction~.-Year ,data=training_data ,family =binomial )
glm.probs<-predict(glm.fit,testing_data,type="response")
glm.preds<-rep("Down",length(glm.probs))
glm.preds[glm.probs>0.55]="Up"
table(glm.preds, Direction.test)
mean(glm.preds==Direction.test)

lda.fit<-lda(Direction~.-Year,data=training_data)
lda.preds<-predict(lda.fit,testing_data)
lda.class<-lda.preds$class
Direction.test<-Direction[!train]
table(lda.class, Direction.test)
mean(lda.class==Direction.test)



qda.fit<-qda(Direction~.-Year,data=training_data)
qda.preds<-predict(qda.fit,testing_data)
qda.class<-qda.preds$class
table(qda.class, Direction.test)
mean(qda.class==Direction.test)

Direction.train<-Direction[train]
train.X=as.matrix(training_data$Lag1,training_data$Lag2,training_data$Lag3,training_data$Lag4,training_data$Lag5,training_data$Volume)
test.X=as.matrix(testing_data$Lag1,testing_data$Lag2,testing_data$Lag3,testing_data$Lag4,testing_data$Lag5,testing_data$Volume)
knn.pred=knn(train.X,test.X,Direction.train,k=3)
table(knn.pred,Direction.test)
mean(knn.pred==Direction.test)