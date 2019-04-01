#recibe un dataset y regresa una lista de vectores, cada vector contiene los valores de la
#variable objetivo, filtrados por un nivel de cada variable predictiva
#regresa una lista en lugar de un dataframe debido a que los vectores son de distinto tamaño
#Esta función es útil cuando cada variable predictiva en el dataframe tiene los mismos niveles 
#utiliza partialVector
totalTargetVectors<-function(nivelVarPredictiva,dataset){
  user<-Sys.info()[7]
  user<-user[[1]]
  rutacode<-paste("C:/Users/",user,"/Google Drive/R-util-functions/diverse/ShannonEntropy/",sep = "")
  source(file=paste(rutacode,"parcialVector.R",sep = ""),encoding = "UTF8")
  
  
  listaVectoresObjetivo<-list()
  for(i in 1:(ncol(dataset)-1)){
    listaVectoresObjetivo[[i]]<-parcialVector(nivelVarPredictiva,dataset[,i],dataset[,ncol(dataset)])
  }
  return(listaVectoresObjetivo)
}