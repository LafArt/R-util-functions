
#recibe una lista de dataframes con la siguiente estructura
# nivel         entropia    peso
# nunca            0.34     0.21
# algunas veces    0.56     0.12
# siempre          0.14     0.30
#utiliza remainderOneVariable para calcular el remainder de cada dataframe (cada variable)
#regresa un vector con los remainders de cada dataframe (variable) de la lista
#Los dataframes son generados por la funcion entropyWeightMultiVariable

remainderMultiVariable<-function(listdfWE){
  rutacode<-paste("C:/Users/",user,"/Google Drive/R-util-functions/diverse/ShannonEntropy/",sep = "")
  source(file=paste(rutacode,"remainderOneVariable.R",sep = ""),encoding = "UTF8")
  
  #testcode
   #listdfWE<-listDataFrames
  #testcode
   
   remainderVector<-c()
   for(lista in 1:length(listdfWE)){
     remainderVector<-c(remainderVector,remainderOneVariable(data.frame(listdfWE[[lista]])))
   }
 return(remainderVector)  
}


