informationGainMultiVariable<-function(dfIGMV){
  #se asume que la variable objetivo está en la última columna del dataset
  #testercode
    #dfIGMV<-datos
  #testercode
  rutacode<-paste("C:/Users/",user,"/Google Drive/R-util-functions/diverse/ShannonEntropy/",sep = "")
  source(file=paste(rutacode,"informationGainOneVariable.R",sep = ""),encoding = "UTF8")
  dfInformationGain<-apply(dfIGMV[,1:(ncol(dfIGMV)-1)],2,FUN=informationGainOneVariable,dfIGMV[,ncol(dfIGMV)])
  prdf<-data.frame(dfInformationGain)  
  prdf<-cbind.data.frame(Variable=row.names(prdf),Ganancia=prdf[,1])
}
