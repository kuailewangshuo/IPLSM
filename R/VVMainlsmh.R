VVMainlsmh<-function(Niter,Y.i,Y.ia,M,N,D){
  
  auc=matrix(NA,nrow=3,ncol=2)
  lsmhMat=list()
  ZsMat=list()
  for(I in 1:3){
    
    d <- dist(t(Y.ia)) 
    fit <- cmdscale(d,eig=TRUE, k=2) 
    Z.a= fit$points 
    
    ZsMat[[I]]=list("Z.i" = lsm(Y.i,D=D)$lsmEZ,"Z.a" = Z.a)
    lsmhMat[[I]]<-VVlsmh(Niter,Y.i,Y.ia,M,N,D,ZsMat[[I]])
    auc[I,]<-as.numeric(VVgetAUC(est.alpha.0 =lsmhMat[[I]]$lsmhAlpha.0,est.alpha.1 = lsmhMat[[I]]$lsmhAlpha.1
                                 ,Z.i=lsmhMat[[I]]$lsmhEZ.i,Z.a=lsmhMat[[I]]$lsmhEZ.a,D,M,N,Y.i, Y.ia))
    
  }
  lsmhMat[[which.max(apply(auc, 1, sum))]]
  
  
}
