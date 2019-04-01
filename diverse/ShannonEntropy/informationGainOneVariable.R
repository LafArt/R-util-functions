#Recibe una variable predictiva y la variable objetivo
#Regresa la ganancia de informacion de una variable predictiva
informationGainOneVariable<-function(ColumnIG,targetIG){
  #testcode
  # ColumnIG<-datos$Pamarilla
  # targetIG<-datos$Enfermedad
  #testcode
  rutacode<-paste("C:/Users/",user,"/Google Drive/R-util-functions/diverse/ShannonEntropy/",sep = "")
  source(file=paste(rutacode,"entropyWeightOneVariable.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutacode,"remainderOneVariable.R",sep = ""),encoding = "UTF8")
  source(file=paste(rutacode,"oneVectorEntropy.R",sep = ""),encoding = "UTF8")
  dfWEIG<-entropyWeightOneVariable(ColumnIG,targetIG)
  remainderIG<-remainderOneVariable(dfWEIG)
  targetEntropy<-oneVectorEntropy(targetIG)
  IG<-round(targetEntropy-remainderIG,digits = 3)
}