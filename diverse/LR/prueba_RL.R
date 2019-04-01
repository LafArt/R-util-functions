prueba_RL<-function(){
  #código para prueba fuera de shiny
  
  rutafunc<-"G:/My Drive/r-scripts/diverse/LR/"
  source(file=paste(rutafunc,"regresa_df_prueba.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutafunc,"LR_LAHA.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutafunc,"prediccion.R",sep = ""),encoding = "UTF8")
  
  
  dfp<-regresa_df_prueba()
  dataset<-dfp[[1]]
  alfa<-0.05
  lambda<-1
  
  dataset<-read.csv("C:/Users/Arturo Laflor/OneDrive/DAIH/breast_cancer_nn/breast_cancer.csv")
  dataset[which(dataset$v6=='?', arr.ind=TRUE),7]<-NA
  dataset<-dataset[complete.cases(dataset),]
  dataset<-dataset[-1]
  dataset$v6<-as.integer(dataset$v6)
  dataset[which(dataset$R==4, arr.ind=TRUE),10]<-1
  dataset[which(dataset$R==2, arr.ind=TRUE),10]<-0
  
  library(ggplot2)
  
  gg<-ggplot(data = dataset)+geom_point(aes(x=v2,y=v4),colour=as.factor((dataset$R)+1))
  gg
  
  # La funcion LR_LAHA regresa una lista que contiene los siguientes vectores:
  #   1.- Los coeficientes del modelo provistos mediante la regresión logística.
  #   2.- El valor de la media
  #   3.- El valor de la desviación estándar
  #   4.- El vector de los errores generados en cada iteracion en que se utilizó la funcion de costos (trainingSet)
  #   5.- El vector correspondiente a los errores con el conjunto CVSet en cada iteración
  
  RL_M<-list()
  
  for(i in seq(from=0.01, to=1.23, by=0.02)){
    RL_M<-c(RL_M,list(LR_LAHA(dtTraining = dataset[1:500,],dtCV=dataset[501:600,], alfa = alfa,lambda = i)))
  }
  
  
  pred_TR<-list()
  pred_CV<-list()
  precision_TR<-c()
  precision_CV<-c()
  for(i in 1:length(RL_M)){
    pred_TR<-c(pred_TR,list(prediccion(dataset[1:500,1:(length(dataset)-1)],RL_M[[i]][[1]],RL_M[[i]][[2]],RL_M[[i]][[3]])))
    pred_CV<-c(pred_CV,list(prediccion(dataset[501:600,1:(length(dataset)-1)],RL_M[[i]][[1]],RL_M[[i]][[2]],RL_M[[i]][[3]])))
    
    precision_TR<-c(precision_TR,mean(pred_TR[[i]]==dataset[1:500,1:(length(dataset)-1)]))
    precision_CV<-c(precision_CV,mean(pred_CV[[i]]==dataset[501:600,1:(length(dataset)-1)]))
    
  }
  
  
  plot(seq(1:length(RL_M)),precision_TR)
  
  
  #PR<-RL_M[[1]][[1]]
  
  RL<-LR_LAHA(dtTraining = dataset[1:500,],dtCV=dataset[501:600,], alfa = alfa,lambda = 200)
  pred<-prediccion(dataset[150:360,1:(length(dataset)-1)],RL[[1]],RL[[2]],RL[[3]])
  mean(dataset[150:360,10]==pred)
  
  
  
  # Para la prediccion se debe pasar como parámetro el conjunto de datos de variables predictivas sin la variable objetivo
  pred<-prediccion(dataset[150:360,1:(length(dataset)-1)],RL_M[[30]][[1]],RL[[30]][[2]],RL[[30]][[3]])
  
  mean(dataset[150:360,10]==pred)
  
  RL[[1]]
  RL[[2]]
  RL[[3]]
  
  plot(RL[[4]],RL[[5]])
 
  

  #prueba de regresión logística utilizando unicamente dos variables del dataset v1 y v2 y tratando de ajustar 
  #el modelo mediante el incremento del grado del polinomio
  
  source(file = "C:/Users/Arturo Laflor/OneDrive/R util-codes/machine-learning/polyexp.R",encoding = "UTF8")
  
  ndt<-data.frame(v1=dataset$v1,v2=dataset$v2,R=dataset$R)
  
  gg<-ggplot(data = ndt)+geom_point(aes(x=v1,y=v2),colour=as.factor((ndt$R)+1))
  gg
  
  ndt[501:600,(length(ndt)-1)]
  source(file="./code/LR/LR_LAHA.R",encoding = "UTF8")
  modelo<-LR_LAHA(ndt[1:500,],ndt[501:600,],alfa = 0.3,lambdaMin = 0.01,lambdaMax = 0.3,polydegre =  1)
  
  pred<-prediccion(ndt[501:600,1:(length(ndt)-1)],modelo[[1]],modelo[[2]],modelo[[3]],polydegre = 1)
  mean(ndt[501:600,3]==pred)
  
  
  pred<-prediccion(ndt[601:650,1:(length(ndt)-1)],modelo[[1]],modelo[[2]],modelo[[3]],1,polydegre = 1)
  
  mean(ndt[601:650,3]==pred)
  
  
  #si se genera un polinomio de grado cinco
  
  ndtaum<-data.frame(polyexp(matrixVarInd = as.matrix(ndt[,1:(length(ndt)-1)]),7))
  ndtaum<-cbind(ndtaum,R=ndt$R)
  
  muN<-apply(ndtaum[,1:2],2,mean)
  
  modelo<-LR_LAHA(ndtaum[1:500,],ndtaum[501:600,],alfa = 0.3,lambda = 0.5)
  
  pred<-prediccion(ndtaum[501:600,1:(length(ndtaum)-1)],modelo[[1]],modelo[[2]],modelo[[3]])
  
  mean(ndt[501:600,3]==pred)
  
  
  #
   
  
  
  
  
  #codigo para prueba fuera de shiny
}