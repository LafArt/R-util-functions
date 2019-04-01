#Determina los mejores parametro del modelo basado en una funcion de costos por gradiente descendente
LRGD<-function(XPR,yPR,alfaPR,lambdaPR){
  JC<-0
  JCV<-0
  thetaPR<-as.matrix(c(rep(0,NCOL(XPR))))
  fc<-fcosto_JR(as.matrix(XPR),as.matrix(yPR),thetaPR,alfaPR,lambdaPR)
  
  i<-1
  b<-TRUE
  while(b){
    fc<-fcosto_JR(as.matrix(XPR),as.matrix(yPR),fc[[2]],alfaPR,lambdaPR)
    JC[i]<-fc[[1]]
    #calcula el costo del error en el conjunto cross validation con los coeficientes proporcionados
    #por los cálculos para minimizar la función de costos en el conjunto de entrenamiento.
    #dentro se genera la columna de unos y se agrega al conjunto de datos
    #JCV[i]<-fcosto_CV(fc[[2]],dtCV = dtCV,mu=dtMedia,ds=dtSd)
    if(i>2){
      if(abs(JC[i-2]-JC[i-1])<0.003){
        b<-FALSE
      }
    }
    i<-i+1
  }
  
  return(list(JC,fc[[2]]))
  
}