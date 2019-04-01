icmedia<-function(xmedia=91.03,S=2.99,n=10,conf=95){
  conf<- ifelse(conf>1,conf/100,conf)
  me<-qt((1-((1-conf)/2)),n-1)*(S/sqrt(n))
  li<- xmedia - me
  ls<- xmedia + me
  return(c(li,ls))
}
