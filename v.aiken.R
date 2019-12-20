v.aiken<-function(data,lsLikert,v1=NULL,v2=NULL){
  data<-validez
  lsLikert=5
  divisor<-lsLikert-1
  datamat<-matrix(data)
  datatr<-data-1
  datatr<-datatr/divisor
  
  vaikenItems<-round(apply(datatr,2,mean),digits = 3)
  if (!is.null(v1))
    vaikenV1<-round(apply(datatr[,v1],2,mean),digits = 3)
  if (!is.null(v2))
    vaikenV2<-round(apply(datatr[,v2],2,mean),digits = 3)
  vaikenTotal<-mean(vaikenItems)
  return(list(vaikenItems,mean(vaikenV1),mean(vaikenV2),vaikenTotal))
}