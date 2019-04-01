#Recibe una lista de vectores, cada vector tiene los datos de la variable objetivo
#fltrados por el nivel deseado (que se definio en la funcion totalVectors)
#regresa un dataframe donde la primera columna son los nombres de las variables y la 
#segunda columna es la entropia correspondiente a cada variable
#utiliza oneVectorEntropy
multiVectorEntropy<-function(nombresVariables,listaVectores){
  user<-Sys.info()[7]
  user<-user[[1]]
  rutacode<-paste("C:/Users/",user,"/Google Drive/R-util-functions/diverse/ShannonEntropy/",sep = "")
  source(file=paste(rutacode,"oneVectorEntropy.R",sep = ""),encoding = "UTF8")
  
  entropiasColumna<-c()
  for(i in 1:length(listaVectores)){
    entropiasColumna<-c(entropiasColumna,round(oneVectorEntropy(c(listaVectores[[i]])),digits = 2))
  }
  cbind.data.frame(nombresVariables,entropiasColumna)
}