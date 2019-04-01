#regresa un vector con los registros de la variable objetivo utilizando como filtro 
#el nivel de la variable predictiva a la que se desea calcular su ganancia
#Recibe el nivel para el que se está calculando el remainder que posteriormente se 
#utilizará para la ganancia.
parcialVector<-function(nivel,predictiva,objetivo){
  tablaPV<-cbind.data.frame(pred=predictiva,obj=objetivo)
  names(tablaPV)<-c("pred","obj")
  parcialTabla<-subset(tablaPV,pred==nivel)
  parcialTabla$obj
}