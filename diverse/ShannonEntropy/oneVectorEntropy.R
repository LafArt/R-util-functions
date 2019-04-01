oneVectorEntropy<-function(variable){
  #Author: Charles Determan Jr.
  #Grade: Data Scientist at Healthgrades
  #linkedin: https://www.linkedin.com/in/cdeterman
  #edited by: Arturo Laflor
  #Descripcion: Calcula la entropía de un vector que en este caso para un 
  #dataframe cada columna será un vector
  freqs <- table(variable)/length(variable)
  -sum(freqs * log2(freqs))  
}
