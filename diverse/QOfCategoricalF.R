QOfCategoricalF=function(datasetCF){
  calc_mode=function(x,primsec=1){
    #temp<-table(datasetC$SH3)
    temp<-table(x)
    tableorder<-order(temp,decreasing = T)
    #vecnames<-names(table)[temp==max(temp)]
    vecvalues<-names(temp[tableorder[primsec]])
  }
  calc_frecmode=function(x,primsec=1){
    #temp<-table(datasetC$SH3)
    temp<-table(x)
    tableorder<-order(temp,decreasing = T)
    vecvalues<-temp[tableorder[primsec]]
  }
  
  
  Feature<-colnames(datasetCF)
  Count<- unlist(apply(datasetCF,2,FUN = function(x) length(x)))
  Miss<- unlist(apply(datasetCF,2,FUN = function(x)  sum(is.na(x))))
  Card<- unlist(apply(datasetCF,2,FUN = function(x) length(levels(factor(x)))))
  Mode<- unlist(apply(datasetCF,2, FUN=calc_mode,primsec=1))
  ModeFrec<- unlist(apply(datasetCF,2, FUN=calc_frecmode,primsec=1))
  ModePerc<-unlist(paste(round(ModeFrec/Count*100,digits = 2),"%",sep = ""))
  Mode2<-unlist(apply(datasetCF,2, FUN=calc_mode,primsec=2))
  Mode2Frec<-unlist(apply(datasetCF,2, FUN=calc_frecmode,primsec=2))
  Mode2Perc<-unlist(paste(round(Mode2Frec/Count*100,digits = 2),"%",sep = ""))
  
  
  TQCategoricalFeatures<-data.frame(Count,Miss,Card,Mode,ModeFrec,ModePerc,Mode2,Mode2Frec,Mode2Perc)
  rownames(TQCategoricalFeatures)<-Feature
  TQCategoricalFeatures
}
