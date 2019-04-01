fcosto_JR=function(XFC,yFC,thetaFC,alfaFC,lambdaFC){
  #pX<-Xnorm
  #theta<-as.matrix(c(rep(0,NCOL(Xnorm)))) 
  
  # XFC<-as.matrix(XPR)
  # yFC<-as.matrix(yPR)
  # thetaFC<-as.matrix(fc[[2]])
  # alfaFC<-0.5
  # lambdaFC<-1
  
  
  mFC<-length(yFC)
  JFC<-0
  GFC<-as.matrix(c(rep(0,length(thetaFC))))
  
  zFC<-as.matrix(XFC)%*%thetaFC
  
  gzFC<-sigmoide(zFC)
  
  J1<-t(as.matrix(yFC))%*%log(gzFC)
  J2<-t(as.matrix(1-yFC))%*%log(1-gzFC)
  
  termino_de_reg<-(lambdaFC/(2*mFC))*(sum(thetaFC[-1]^2))
  #print(paste("Reg_TERM:",termino_de_reg,"Lamdda:",lambdaFC))
  
  
  JFC<-((J1+J2)/(-mFC))+termino_de_reg
  
  GFC<-t(as.matrix(XFC))%*%(gzFC-as.matrix(yFC))
  
  thetaFC[1]<-thetaFC[1]-((alfaFC/mFC)*GFC[1])
  
  thetaFC[-1]<-thetaFC[-1]-((alfaFC/mFC)*(GFC[-1]-(lambdaFC*thetaFC[-1]))) 
  
  return(list(JFC,thetaFC))
  
}