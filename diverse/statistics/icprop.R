icprop<-function(p=0.5,n=30,conf=95){
  conf<- ifelse(conf>1,conf/100,conf)
  am<-(1-conf)/2
  me<-qnorm(1-am)*sqrt(p*(1-p)/n)
  li<- p - me
  ls<- p + me
  return(c(li,ls))
}