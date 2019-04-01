evaluacion=function(XE,thetaE,yE,muEval,sdEval){
  source(file="C:/Master/Logistic Regression/LR_R/prediccion.R")
  pE<-prediccion(XE,thetaE,muEval,sdEval)
  mediaE<-mean(pE==yE)
  return(round(mediaE*100,digits = 2))
}