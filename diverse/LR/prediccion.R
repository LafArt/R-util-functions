prediccion=function(XP,thetaP,muPR,dsPR,polydegre=1){
  rutafunc<-"G:/My Drive/r-scripts/diverse/LR/"
  
  source(file=paste(rutafunc,"sigmoide.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutafunc,"normalizar.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutafunc,"polyexp.R",sep = ""),encoding = "UTF8")
  
  
  
  if(polydegre>1){
    XP<-polyexp(XP,polydegre)
  }
  
  tempNorm<-normalizar(XP,muPR,dsPR)
  datosNorm<-tempNorm[[1]]
  
  x0P<-c(rep(1,length(datosNorm[,1])))
  datosNorm<-cbind(x0P,datosNorm)
  zP<-as.matrix(datosNorm)%*%thetaP
  pP<-sigmoide(zP)
  return(round(pP,digits = 0))
}