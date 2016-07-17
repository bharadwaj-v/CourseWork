LR.fit=glm(Direction~Lag2+I(Lag2^2),family=binomial,data=weekly)
LR.pred=predict(LR.fit,type="response")
contrasts(Direction)

roc.curve=function(s,print=FALSE){
  Ps=(LR.pred>s)*1
  FP=sum((Ps==1)*(Direction=="Down"))/sum(Direction=="Down")
  TP=sum((Ps==1)*(Direction=="Up"))/sum(Direction=="Up")
  if(print==TRUE){
    print(table(Observed=Direction,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}

s=0.5
roc.curve(s,print=TRUE)
## Plot ROC Curve
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=0.01))
plot(M.ROC[1,],M.ROC[2,],col="green",lwd=2,type="l"
     ,xlab="False
     positive rate"
     ,ylab="True positive rate")
