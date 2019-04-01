fcosto_CV<-function(coeficientes,dtCV,mu,ds){
  XN<-normalizar(dtCV[,1:(length(dtCV)-1)],muN = mu,desvN = ds)
  
  XNORM<-XN[[1]]
  
  yFC<-dtCV[,length(dtCV)]
  
  x0P<-c(rep(1,length(XNORM[,1])))
  XFC<-cbind(x0P,XNORM)
  
  mFC<-length(yFC)
  JFC<-0
  GFC<-as.matrix(c(rep(0,length(coeficientes))))
  
  zFC<-as.matrix(XFC)%*%coeficientes
  
  gzFC<-sigmoide(zFC)
  
  J1<-t(as.matrix(yFC))%*%log(gzFC)
  J2<-t(as.matrix(1-yFC))%*%log(1-gzFC)
  
  JFC<-((J1+J2)/(-mFC))
  
}