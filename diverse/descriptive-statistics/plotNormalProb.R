plotNormalProb<-function(media=0,desvstd=1,li=-4.0,ls=0.88){
  mean=media; sd=desvstd
  lb=li; ub=ls
  
  x <- seq(-4,4,length=200)*sd + mean
  hx <- dnorm(x,mean,sd)
  
  plot(x, hx, type="n", xlab="Valores inter cuartiles", ylab="",
       main="Distribución Normal", axes=FALSE)
  
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
  
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  result <- paste("P(",lb,"< IC <",ub,") =",
                  signif(area, digits=3))
  mtext(result,3)
  axis(1, at=seq(-3, 3, 1), pos=0)
}