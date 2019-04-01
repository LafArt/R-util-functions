sigmoide=function(zS){
  sig=function(valor){return (1/(1+exp(1)^(-valor)))}
  S<-apply(zS, 2, sig)
  return(S)
  
}