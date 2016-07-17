LDA.fit=lda(Direction~Lag2+I(Lag2^2),data=weekly)
## predict probabilities of training data
LDA.pred1=predict(LDA.fit,type="response")
LDA.pred=LDA.pred1$posterior[,2]
## Calculate FPR and TPR under a given threshold
roc.curve1=function(s,print=FALSE){
  Ps=(LDA.pred>s)*1
  FP=sum((Ps==1)*(Direction=="Down"))/sum(Direction=="Down")
  TP=sum((Ps==1)*(Direction=="Up"))/sum(Direction=="Up")
  if(print==TRUE){
    print(table(Observed=Direction,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold=0.5
roc.curve1(threshold,print=TRUE)
ROC.curve1=Vectorize(roc.curve1)
M.ROC1=ROC.curve1(seq(0,1,by=0.01))
plot(M.ROC1[1,],M.ROC1[2,],col="grey",lwd=2,type="l"
     ,xlab="False
     positive rate"
     ,ylab="True positive rate")
