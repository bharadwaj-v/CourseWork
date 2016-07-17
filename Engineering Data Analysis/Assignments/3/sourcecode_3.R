cor(weekly[,-9])?
summary(weekly)
plot(Volume~Lag1,data=weekly)

#Logistic Regression
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume ,data=weekly ,family =binomial )
attach(weekly)
glm.probs<-predict(glm.fit,type="response")
glm.preds<-rep("Down",length(glm.probs))
glm.preds[glm.probs>0.5]="Up"
table(glm.preds ,Direction)
weekly.pre09<-weekly[Year<2009,] #dataset before 2009
weekly.post09<-weekly[Year>=2009,] #dataset after 2009
glm.fitLag2<-glm(Direction~Lag2,data=weekly.pre09,family=binomial)
glm.probsLag2<-predict(glm.fitLag2,weekly.post09,type="response")
glm.predsLag2<-rep("Down",length(glm.probsLag2))
glm.predsLag2[glm.probsLag2>0.5]="Up"
Direction.0910<-subset(weekly.post09,select=Direction,drop=TRUE)
table(glm.predsLag2, Direction.0910)
#LDA
library(MASS)
lda.fitLag2<-lda(Direction~Lag2,data=weekly.pre09)
lda.predsLag2<-predict(lda.fitLag2,weekly.post09)
lda.class<-lda.predsLag2$class
table(lda.class, Direction.0910)
#QDA
qda.fitLag2<-qda(Direction~Lag2,data=weekly.pre09)
qda.predsLag2<-predict(qda.fitLag2,weekly.post09)
qda.class<-qda.predsLag2$class
table(qda.class, Direction.0910)

#knn
train.X=as.matrix(weekly.pre09$Lag2)
test.X=as.matrix(weekly.post09$Lag2)
set.seed(1)
knn.pred=knn(train.X,test.X,subset(weekly.pre09,select=Direction,drop=TRUE),k=1)
table(knn.pred,Direction.0910)

#(i)
glm.fit2<-glm(Direction~Lag2:Lag1 ,data=weekly.pre09 ,family =binomial )
glm.probs2<-predict(glm.fit2,weekly.post09,type="response")
glm.preds2<-rep("Down",length(glm.probs2))
glm.preds2[glm.probs2>0.5]="Up"
table(glm.preds2, Direction.0910)
mean(glm.preds2==Direction.0910)

#LDA
lda.fit2<-lda(Direction~Lag2+I(Lag2^2),data=weekly.pre09)
lda.preds2<-predict(lda.fit2,weekly.post09)
lda.class2<-lda.preds2$class
table(lda.class2, Direction.0910)
mean(lda.class2==Direction.0910)

#QDA
qda.fit2<-qda(Direction~Lag2+I(Lag2^2),data=weekly.pre09)
qda.preds2<-predict(qda.fit2,weekly.post09)
qda.class2<-qda.preds2$class
table(qda.class2, Direction.0910)
mean(qda.class2==Direction.0910)

#KNN
set.seed(10)
train.X=as.matrix(weekly.pre09$Lag2,weekly.pre09$Lag2^2)
test.X=as.matrix(weekly.post09$Lag2,weekly.pre09$Lag2^2)
knn.pred=knn(train.X,test.X,subset(weekly.pre09,select=Direction,drop=TRUE),k=20)
table(knn.pred,Direction.0910)
mean(knn.pred==Direction.0910)