
DDgetAUC<-function(est.alpha.0,est.alpha.1,Z.i,Z.a,D,M,N,Y.i, Y.ia){
  est.P.i=matrix(NA,N,N)
  est.P.ia=matrix(NA,N,M)

  
  for ( i in 1:(N-1)){
    for (j in (i+1):N){
      est.P.i[i,j]=inv.logit(est.alpha.0-sum((Z.i[i,] - Z.i[j,]) ^ 2))

    }
  }
  
  
  
  for(i in 1:N){
    for(a in 1:M){
      est.P.ia[i,a]=inv.logit(est.alpha.1-sum((Z.i[i,] - Z.a[a,]) ^ 2))
    }
  }
  
  
  a=roc(c(Y.i),c(est.P.i),auc.polygon=FALSE, grid=FALSE, plot=TRUE,auc=TRUE,main=NULL, font.axis = 2,bty="n", cex.axis =1.2,ann=FALSE,  col="black",
        ylab="true positive rate",xlab="false positive rate",xlim=c(1,0),legacy.axes = TRUE, lwd=5)
  text(x=0.45, y=.1,cex=1.6, labels=paste("AUC = ", as.character(round(as.numeric(a$auc),digits = 4)),sep = ""), 
       font= 2, col="black")
  
  
  c=roc(c(Y.ia),c(est.P.ia),auc.polygon=FALSE, grid=FALSE, 
        ylab="true positive rate",xlab="false positive rate",xlim=c(1,0),plot=TRUE,auc=TRUE,main=NULL, font.axis = 2,bty="n", cex.axis =1.2,ann=FALSE,  col="black",legacy.axes = TRUE, lwd=5)
  text(x=0.45, y=.1,cex=1.6, labels=paste("AUC = ", as.character(round(as.numeric(c$auc),digits = 4)),sep = ""), font= 2, col="black")
  
  return(c(a$auc,c$auc))
  
}

