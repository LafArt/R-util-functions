# calcula el nivel de entropia y el peso por nivel para una variable
# utiliza parcialVector
# recibe un vector que es una variable categorica y la variable objetivo
# regresa una dataframe como el siguiente

# nivel         entropia    peso
# nunca            0.34     0.21
# algunas veces    0.56     0.12
# siempre          0.14     0.30

entropyWeightOneVariable<-function(predictive,target){
  
  user<-Sys.info()[7]
  user<-user[[1]]
  rutacode<-paste("C:/Users/",user,"/Google Drive/R-util-functions/diverse/ShannonEntropy/",sep = "")
  source(file=paste(rutacode,"oneVectorEntropy.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutacode,"parcialVector.R",sep = ""),encoding = "UTF8")
  
  #test code
  # datos<-read.csv(file="C:/Users/artur/Google Drive/DAIH/Paper komputer sapiens/ejemplo-enf.csv",header = T,sep = ",")
   # predictive<-datos$Estornudos
   # target<-datos$Enfermedad
  #test code
  
  predictiveLeves<-table(predictive)
  matrixRes<-matrix(NA,nrow = length(predictiveLeves),ncol = 3)
  
  for(i in 1:length(predictiveLeves)){
    #names(predictiveLeves)[i]
    entropyLevel<- round(oneVectorEntropy(c(parcialVector(names(predictiveLeves)[i],predictive,target))),digits = 3)
    weightLevel<-round(predictiveLeves[[i]]/length(target),digits = 3)
    matrixRes[i,]<-c(names(predictiveLeves)[i],entropyLevel,weightLevel)
  }
  #fue necesario hacer columna por columna para poner el tipo de dato correcto
  dfRes<-cbind.data.frame(Nivel=as.factor(matrixRes[,1]),Entropia=as.numeric(matrixRes[,2]),Peso=as.numeric(matrixRes[,3]))
  
}
