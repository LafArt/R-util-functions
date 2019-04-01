# Recibe un dataset con todas las variables predictivas categoricas
# Se asume que la variable objetivo esta en la ultima columna
# regresa una lista con todos los dataframes que tienen 
# la entropia y el nivel por peso como en el siguiente ejemplo:
  # nivel         entropia    peso
  # nunca            0.34     0.21
  # algunas veces    0.56     0.12
  # siempre          0.14     0.30
  
entropyWeightMultiVariable<-function(dt){
  #testcode
   #dt<-datos
  #testcode
  rutacode<-paste("C:/Users/",user,"/Google Drive/R-util-functions/diverse/ShannonEntropy/",sep = "")
  source(file=paste(rutacode,"entropyWeightOneVariable.R",sep = ""),encoding = "UTF8")
  listDataFrames<-list()
  for(i in 1:(ncol(dt)-1)){
    listDataFrames[[i]]<-entropyWeightOneVariable(dt[,i],dt[,ncol(dt)])
  }
  return(listDataFrames)
}
