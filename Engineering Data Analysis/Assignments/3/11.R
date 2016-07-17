attach(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto <- data.frame(Auto, mpg01)
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01")

train_sample_size <- floor(0.75 * nrow(Auto))
set.seed(123)
train_index <- sample(seq_len(nrow(Auto)), size = train_sample_size)
train_data<-Auto[train_index,]
test_data<-Auto[-train_index,]
mpg01.test<-mpg01[-train_index]
#lda
lda.Auto<-lda(mpg01~cylinders+displacement+horsepower+weight+acceleration+year,data=train_data)
lda.Auto
lda.predAuto<-predict(lda.Auto,test_data)
lda.classAuto<-lda.predAuto$class
table(lda.classAuto, mpg01.test)
mean(lda.classAuto != mpg01.test)

#qda
qda.Auto<-qda(mpg01~cylinders+displacement+horsepower+weight+acceleration+year,data=train_data)
qda.Auto
qda.predAuto<-predict(qda.Auto,test_data)
qda.classAuto<-qda.predAuto$class
table(qda.classAuto, mpg01.test)
mean(qda.classAuto != mpg01.test)

#LR
glm.Auto<-glm(mpg01~cylinders+displacement+horsepower+weight+acceleration+year,data=train_data ,family =binomial )
summary(glm.Auto)
glm.probsAuto<-predict(glm.Auto,test_data,type="response")
glm.predsAuto<-rep(0,length(glm.probsAuto))
glm.predsAuto[glm.probsAuto>0.5]=1
table(glm.predsAuto, mpg01.test)
mean(glm.predsAuto!=mpg01.test)

#KNN

set.seed(1)
mpg01.train<-mpg01[train_index]
train.Auto=as.matrix(train_data$cylinders,train_data$displacement,train_data$horsepower,train_data$weight,train_data$acceleration,train_data$year)
test.Auto=as.matrix(test_data$cylinders,test_data$displacement,test_data$horsepower,test_data$weight,test_data$acceleration,test_data$year)
knn.predAuto=knn(train.Auto,test.Auto,mpg01.train,k=10)
table(knn.predAuto,mpg01.test)
mean(knn.predAuto!=mpg01.test)
