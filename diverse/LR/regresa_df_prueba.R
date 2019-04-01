regresa_df_prueba<-function(){
  ruta<-"G:/My Drive/thesis-project/Data/"
  dc<-read.csv(paste(ruta,"sq_numeric_todos_.csv",sep = ""),header = T,sep = ",")
  dc$SQ<-as.character(dc$SQ)
  dc[which(dc$SQ=="Buena",arr.ind = FALSE),35]<-0
  dc[which(dc$SQ=="Pobre",arr.ind = FALSE),35]<-1
  dc$SQ<-as.integer(dc$SQ)
  dc<-data.frame(dc[,14:34],CLASE=dc[,35])
  nregistros<-ceiling(nrow(dc)*(70/100))
  indexRows<-sample(nrow(dc),nregistros,replace=F)
  trainingSet<-dc[indexRows,]
  dttemp<-dc[-indexRows,]
  tope<-ceiling(nrow(dc)*(15/100))
  cvSet<-dttemp[1:tope,]
  testSet<-dttemp[(tope+1):nrow(dttemp),]
  listaDF<-list(trainingSet,cvSet,testSet)  
}
