plotjoint <-function(Y.i, Y.ia, N, M, K.V,  colPl = 1, 
                     colEll.i = rgb(.6, .6 ,.6 , alpha=.1), colEll.ia = rgb(1, .6 ,.6 , alpha=.1),
                     LEVEL = .80, 
                     pchplot = 20, pchEll = 19, pchPl = 19, cexPl = 1.1, 
                     arrowhead = FALSE, curve = 0, xlim = NULL, ylim = NULL, lwdLine = .001, ...){
  
  Z.i=K.V$lsmhEZ.i
  Z.a=K.V$lsmhEZ.a
  
  
  
  VZ1=K.V$lsmhVZ.1
  VZ0=K.V$lsmhVZ.0
  

  par(mar=c(2,2,2,2))
  ## first plot, latent person space 
  plot(NA, xlim = c(-1.5,1.5), ylim = c(-1.5,1.5),
       ylab="", xlab = "",font.axis=2, main = NA,bty="n")
  
  for(n in 1:N)
  {
    coEl<-ellipse::ellipse(VZ0,centre = Z.i[n,],level=LEVEL,pch=pchEll)
    polygon(coEl[,1], coEl[,2], col = colEll.i, border=colEll.i)
  }
  
  
  for(i in 1:(N-1)){ 
    for(j in 2:N){ 
      if(Y.i[i,j] == 1)
        network.arrow(Z.i[i,1], Z.i[i,2], Z.i[j,1], Z.i[j,2], col = "black", 
                      border = rgb(0, 0, 0, alpha =.02), lwd = .005, 
                      arrowhead = arrowhead, curve = curve)		
    }	
  }
  
  
  for(n in 1:M)
  {
    coEl<-ellipse::ellipse(VZ1,centre = Z.a[n,],level=LEVEL,pch=pchEll)
    polygon(coEl[,1], coEl[,2], col = colEll.ia, border=colEll.ia)
  }
  
  if(!is.null(colnames(attri_1))){ 
    text(Z.a, labels = colnames(attri_1), col = "black", font = 2, cex=1)
  } 
  
  
  
}