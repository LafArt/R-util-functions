normalizar=function(XNR,muN=NaN,desvN=NaN){
  if(is.nan(muN) && is.nan(desvN)){
    muN<-apply(XNR,2,mean)
    desvN<-apply(XNR, 2,sd)
    calculo=function(valor){(valor-mean(valor))/sd(valor)}
    XNRnormalizada<-as.data.frame(apply(XNR, 2, calculo))   
  }
  else{
    XNRnormalizada<-t((t(XNR)-muN)/desvN)
  }
  return(list(XNRnormalizada,muN,desvN))
  #esto no funciona
  #mu<-colMeans(tabla)
  #desv<-apply(tabla, 2,sd)
  #calculo=function(valor){(valor-mu)/desv}
}