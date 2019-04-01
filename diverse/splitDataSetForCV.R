#Particiona un dataset preparandolo para el proceso de cross validation
#recibe como parámetros el dataset, el porcentaje de registros que se desea tener para el TraininSet
#y el porcentaje de registros que se desea para el CVSet
#Regresa una lista con tres datagframes conteniendo los datasets dttrain, dtcv y dttest en ese orden
splitDataSetForCV<-function(dataSet,nTraining,nCV){
  #con este código nos aseguramos de que los regisros que se tomen para el entrenamiento siempre sean distintos
  # dtime<-Sys.time()
  # tm<-regexpr('[0-9]{2}[:][0-9]{2}[:][0-9]{2}',dtime)
  # tm<-substr(dtime,tm,tm+attr(tm,'match.length')-1)
  # semilla<-as.numeric(gsub('[:]','',tm)) 
  # set.seed(semilla)
  #
  #set.seed(4532)
  nregistros<-ceiling(nrow(dataSet)*(nTraining/100))
  indexRows<-sample(nrow(dataSet),nregistros,replace=F)
  dttrain<-dataSet[indexRows,]
  dttemp<-dataSet[-indexRows,]
  tope<-ceiling(nrow(dataSet)*(nCV/100)) 
  dtcv<-dttemp[1:tope,]
  dttest<-dttemp[(tope+1):nrow(dttemp),]
  listDF<-list(dttrain,dtcv,dttest)  
}
