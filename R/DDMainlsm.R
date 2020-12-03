

#' run the distance by distance IPLSM
#'
#' Function to joint modeling of item and person network model using the IPLSM Wang, Paul and De Boeck
#' The IPLSM merges information given by the social network and the item responses
#'
#'  @param Niter number of iterations
#'  @param Y.i N by N matrix containing the binary social network
#'  @param Y.ia N by M matrix containing the binary item response matrix
#'  @param M number of items in the data
#'  @param N number of persons in the data
#'  @param D number of dimensions in the data
#'  @return list containing:
#'  \itemize{
#'  \item \code{lsmhEZ.i} (\code{N} x \code{D}) matrix containing the posterior means of the latent person positions
#'  \item \code{lsmhEZ.a} (\code{M} x \code{D}) matrix containing the posterior means of the latent item positions
#'  \item \code{lsmhVZ.0} (\code{D} x \code{D}) matrix containing the posterior variance of the latent person positions
#'  \item \code{lsmhVZ.1} (\code{D} x \code{D}) matrix containing the posterior variance of the latent item positions
#'  \item \code{lsmEZ} list contatining a (\code{N} x \code{D}) matrix for each network view containing the posterior means of the latent positions under each model in the latent space.
#'  \item \code{lsmVZ} list contatining a (\code{D} x \code{D}) matrix for each network view containing the posterior variance of the latent positions under each model in the latent space.
#'  \item \code{lsmhAlpha.0} scaler of mean of the posterior distributions of \eqn{\alpha.0}
#'  \item \code{lsmhAlpha.1} scaler of mean of the posterior distributions of \eqn{\alpha.1}
#'  \item \code{lsmhKL} expected log-likelihood
#'  }
#'  @export


DDMainlsmh<-function(Niter,Y.i, Y.ia,M,N,D){
  auc=matrix(NA,nrow=3,ncol=2)
  lsmhMat=list()
  ZsMat=list()
  for(I in 1:3){

    d <- dist(t(Y.ia))
    fit <- cmdscale(d,eig=TRUE, k=D)
    Z.a= fit$points

    ZsMat[[I]]=list("Z.i" = lsm(Y.i,D=D)$lsmEZ,"Z.a" = Z.a)
    lsmhMat[[I]]<-DDlsmh(Niter,Y.i,Y.ia,M,N,D,ZsMat[[I]])
    auc[I,]<-as.numeric(DDgetAUC(est.alpha.0 =lsmhMat[[I]]$lsmhAlpha.0,est.alpha.1 = lsmhMat[[I]]$lsmhAlpha.1
                                 ,Z.i=lsmhMat[[I]]$lsmhEZ.i,Z.a=lsmhMat[[I]]$lsmhEZ.a,D,M,N,Y.i, Y.ia))

  }
  lsmhMat[[which.max(apply(auc, 1, sum))]]


}
