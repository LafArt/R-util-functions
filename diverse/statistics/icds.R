icds<-function(S=1,n=30,conf=95){
  conf<-ifelse(conf>1,conf/100,conf)
  p<-(1-conf)/2
  livar<- ((n-1)*S^2)/(qchisq(1-p,n-1))
  lsvar<- ((n-1)*S^2)/(qchisq(p,n-1))
  return(c(sqrt(livar),sqrt(lsvar)))
}




