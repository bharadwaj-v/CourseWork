#remove other predictor variables
crimeViolence<-crimeNonNA[,c(-120,-118:-103)]
crimeViolence1<-crimeNonNA1[,c(-120,-118:-103)]
#create a model matrix X and Y containing the predictors and outputs
x<-model.matrix (violentPerPop~.,crimeViolence)[,-1]
y<-crimeViolence$violentPerPop

#create a grid of values to iterate the lambda
grid <- 10^ seq (10,-2, length =100)


set.seed(1)
#create the training data and test data. Training:Test ratio is 70:30
train<-sample(nrow(crimeViolence),(0.7)*nrow(crimeViolence))
crime.train<-crimeViolence[train,]
crime.test<-crimeViolence[-train,]

#run lasso regression using alpha = 1 and lambda = grid
lasso.mod =glmnet (x[train ,],y[train],alpha =1, lambda =grid)

#running cross validation using cv.glmnet
cv.crime.out<-cv.glmnet (x[train ,],y[train],alpha =1)
#plot the cross validation output
plot(cv.crime.out)
#get the best lambda
bestlam<-cv.crime.out$lambda.min

#using the bestlam predict the test MSE
lasso.pred<-predict (lasso.mod ,s=bestlam ,newx=x[-train,])
mean((lasso.pred - y[-train])^2)

#predicting the coefficients 
out=glmnet (x,y,alpha =1, lambda =bestlam)
lasso.coef=predict (out ,type ="coefficients",s=bestlam)[1:103,]
lasso.coef[lasso.coef!=0]



#creating a vector with the new variables whose coefficients are not 0
newVariables<-names(lasso.coef[lasso.coef!=0][-1])
newVariables<-c("pctBlack","pctWhite","pct12-29","pctUrban","asianPerCap","otherPerCap","pctEmployMfg",
                "pctMaleDivorc","pctKids2Par","pctWorkMom-18","pctKidsBornNevrMarr","pctPopDenseHous",
                "pctSmallHousUnits","houseVacant","pctHousOccup","pctVacantBoarded","rentQrange","medRentpctHousInc",
                "medOwnCostPctWO","pctForeignBorn","pctBornStateResid","pctOfficDrugUnit","violentPerPop")
crimeViolenceFinalData<-crimeViolence[newVariables]
names(crimeViolenceFinalData1)<-c("pctBlack","pctWhite","pct12to29","pctUrban","asianPerCap","otherPerCap","pctEmployMfg",
                                 "pctMaleDivorc","pctKids2Par","pctWorkMom18","pctKidsBornNevrMarr","pctPopDenseHous",
                                 "pctSmallHousUnits","houseVacant","pctHousOccup","pctVacantBoarded","rentQrange","medRentpctHousInc",
                                 "medOwnCostPctWO","pctForeignBorn","pctBornStateResid","pctOfficDrugUnit","violentPerPop")
crimeViolenceFinalData1<-crimeViolence[newVariables]
crimeViolenceFinalData2<-crimeViolence1[newVariables]
#linear regression model on the complete set of variables, performed on the training data
lm.fit<-lm(violentPerPop~.,data=crimeViolenceFinalData[train,])
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)

lm.pred<-predict(lm.fit,crimeViolenceFinalData[-train,])
mean((lm.pred-crimeViolenceFinalData[-train,]$violentPerPop)^2)
#correlation matrix


#removing outliers and high leverage points


lm.fit1<-lm(violentPerPop~.,crimeViolenceFinalData2[train,])
summary(lm.fit1)
par(mfrow=c(2,2))
plot(lm.fit1)
lm.pred1<-predict(lm.fit1,crimeViolenceFinalData2[-train,])
mean((lm.pred1-crimeViolenceFinalData2[-train,]$violentPerPop)^2)

lm.fit2<-lm(violentPerPop~.-pctWhite-asianPerCap-medRentpctHousInc-pctSmallHousUnits-pctHousOccup-`pct12-29`-pctVacantBoarded-pctOfficDrugUnit-pctForeignBorn-pctBornStateResid-pctKids2Par,crimeViolenceFinalData2[train,])
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
lm.pred2<-predict(lm.fit2,crimeViolenceFinalData2[-train,])
mean((lm.pred2-crimeViolenceFinalData2[-train,]$violentPerPop)^2)
vif(lm.fit2)

plot(predict(lm.fit1), residuals(lm.fit1), cex=0.8, main="Residuals vs. Predicted")
abline(0,0,col="red")


