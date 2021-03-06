
DDgetEstP<-function(est.alpha.0,est.alpha.1,Z.i,Z.a,D,M,N){

  est.P.i=matrix(NA,N,N)

  est.P.ia=matrix(NA,N,M)

  
  for ( i in 1:(N-1)){
    for (j in (i+1):N){
      est.P.i[i,j]=inv.logit(est.alpha.0-sum((Z.i[i,] - Z.i[j,]) ^ 2))
      est.P.i[j,i]=est.P.i[i,j]

    }
  }
  
  
  for(i in 1:N){
    for(a in 1:M){
      est.P.ia[i,a]=inv.logit(est.alpha.1-sum((Z.i[i,] - Z.a[a,]) ^ 2))
    }
  }
  return(list(est.P.i,est.P.ia))
  
  
  
}

