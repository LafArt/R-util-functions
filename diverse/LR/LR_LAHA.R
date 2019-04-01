# La funcion calcula los coeficientes del modelo mendiante una regresión logistica programada a través de un gradiente descendente.
# Para hacer un cálculo efectivo, hace la normalización de los datos como método de escalamiento de variables a fin de ponerlas todas 
# en el mismo rango [-3 3].
# La funcion regresa una lista que contiene los siguientes vectores:
#   1.- Los coeficientes del modelo provistos mediante la regresión logística.
#   2.- El valor de la media
#   3.- El valor de la desviación estándar
#   4.- El vector de los errores generados en cada iteracion en que se utilizó la funcion de costos (trainingSet)
#   5.- El vector correspondiente a los errores con el conjunto CVSet
# 
# Los valores de la media y la desviación estándar serán utilizados por la fucion predicción cuando se requiera 
# para calcular la eficiencia de la predicción
# Recibe como parámetros el dataset con el que se va trabajar asumiendo que la varaible objetivo está en la 
# ultima columna y que está represetada en valor numérico binario. [1 y 0]

#Descripcion de los parametros de entrada
# 1.- dtTraining: conjunto de datos de entrenamiento
# 2.- dtCV: conjunto de datos CrossValidation
# 3.- alfa: valor que define el paso en el gradiente descendente
# 4.- lambdaMin: Parámetro de regularización con el valor mínimo.
# 5.- lambdaMax: Parámetro de regularizacion con el valor máximo.
# El algoritmo utilizará todos los valores intermedios entre estos dos parámetros y regresará el valor de lambda que 
# ajustó mejor el modelo al conjunto de datos Cross Validation. 

LR_LAHA<-function(dtTraining,dtCV,alfa=0.5,lambdaMin=1, lambdaMax=1,polydegre=1,updateProgress=NULL){
  
  rutafunc<-"G:/My Drive/r-scripts/diverse/LR/"
  
  source(file=paste(rutafunc,"sigmoide.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutafunc,"fcosto_JR.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutafunc,"normalizar.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutafunc,"fcosto_CV.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutafunc,"LRGD.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutafunc,"polyexp.R",sep = ""),encoding = "UTF8")
  
  
  # #código para prueba fuera de shiny
  # source(file="./code/regresa_df_prueba.R",encoding = "UTF8")
  # dfp<-regresa_df_prueba()
  # dataset<-dfp[[1]]
  # alfa<-0.05
  # lambda<-1
  # 
  # dataset<-read.csv("C:/Users/Arturo Laflor/OneDrive/DAIH/breast_cancer_nn/breast_cancer.csv")
  # dataset[which(dataset$v6=='?', arr.ind=TRUE),7]<-NA
  # dataset<-dataset[complete.cases(dataset),]
  # dataset<-dataset[-1]
  # dataset$v6<-as.integer(dataset$v6)
  # dataset[which(dataset$R==4, arr.ind=TRUE),10]<-1
  # dataset[which(dataset$R==2, arr.ind=TRUE),10]<-0
  # 
  # #codigo para prueba fuera de shiny
  
  #Preparación del conjunto de datos########################################################
  
  
  if(polydegre>1){
    varObjetivo<-names(dtTraining[length(dtTraining)])
    dtTrainingTemp<-polyexp(dtTraining[,1:(length(dtTraining)-1)],polydegre)
    dtTraining<-data.frame(dtTrainingTemp,varObjetivo=dtTraining[length(dtTraining)])
    dtCVTemp<-polyexp(dtCV[,1:(length(dtCV)-1)],polydegre)
    dtCV<-data.frame(dtCVTemp,varObjetivo=dtCV[length(dtCV)])

  }
  
  nombres<-names(dtTraining)
  dfPredictivo<-dtTraining[nombres[1:(length(nombres)-1)]]
  vec_Objetivo<-dtTraining[nombres[length(nombres)]]
  dtNorm<-normalizar(dfPredictivo) #el conjunto de datos se transforma a valores entre [-4 4]
  dtMedia<-dtNorm[[2]]
  dtSd<-dtNorm[[3]]
  XPR<-dtNorm[[1]]
  x0<-c(rep(1,length(XPR[[1]])))
  XPR<-cbind.data.frame(x0,XPR)
  yPR<-vec_Objetivo
  
  ############################################################################################
  
  
  #Preparación de lambda#################################################################
  paso<-0.03
  if(lambdaMin==lambdaMax){
    lambdaPR<-lambdaMin  
  }else if(lambdaMin>lambdaMax){
    paso<-paso*(-1)
  }
  
  ######################################################################################
  
  #Entrtena el modelo de regresión logística con diversa lambdas, al fina regresa el modelo que mejor se 
  #ajustó a los datos tomando en cuanta el parámetro lambda. También se regresa el parámetro lambda como
  #parte de la lista 
  lambdas<-matrix(nrow = 0,ncol = 2)
  iteraciones<-(lambdaMax-lambdaMin)/paso
  k<-1
  for(lambda in seq(from=lambdaMin, to=lambdaMax, by=paso)){
    modeloRL<-LRGD(XPR,yPR,alfa,lambda)
    tempPR<-prediccion(dtCV[,1:(length(dtCV)-1)],modeloRL[[2]],muPR = dtMedia,dsPR = dtSd)
    lambdas<-rbind(lambdas,c(lambda,round(mean(tempPR==dtCV[,length(dtCV)]),digits = 2)))
    if (is.function(updateProgress)) {
      text <- paste(floor(k),"de",floor(iteraciones),"iteraciones")
      updateProgress(value= k/iteraciones, detail = text)
    }
    k<-k+1
  }
  lambdaOptimo<-lambdas[which.max(lambdas[,2]),1]
  #lambdaOptimo<-lambdas[which.min(lambdas[,2]),1]
  modeloRL<-LRGD(XPR,yPR,alfa,lambdaOptimo)
  JC<-modeloRL[[1]]
  #######################################################################################################
  
  
  
  # La funcion regresa una lista que contiene los siguientes vectores:
  #   1.- Los coeficientes del modelo provistos mediante la regresión logística.
  #   2.- El valor de la media
  #   3.- El valor de la desviación estándar
  #   4.- El vector de los errores generados en cada iteracion en que se utilizó la funcion de costos (trainingSet)
  #   5.- Lambda óptimo
  
  res<-list(modeloRL[[2]],dtMedia,dtSd,JC,lambdaOptimo)
    #plot(c(2:i)/1000,JC[1:i-1])
  return(res)
}