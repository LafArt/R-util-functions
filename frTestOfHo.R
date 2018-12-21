frTestOfHo<-function(stObject=NULL, sTableText="text",
                     levelSig=0.05,typeOfTest="chi",
                     language="sp",estat="chi-square"){
  estadistico<-round(stObject$statistic,digits = 3) 
  pvalue<-ifelse(stObject$p.value < 0.0001,format(stObject$p.value,scientific = TRUE),round(stObject$p.value,digits = 4))   
  grados<-ifelse(is.na(stObject$df) ||  is.nan(stObject$df),"NA",stObject$df) 
  reject<-""
  noReject<-""
  if(identical(language,"en")){
    H0<-switch(typeOfTest,
               "independency"="The variables are independent.",
               "goodnessOfFit"="The observed frequencies and the expected ones are estatistically equals."
    )
    reject<-paste("Reject Ho:'",H0,"' The p-value(",pvalue,") is less than the significance level(",levelSig,").",sep = "") 
    noReject<-paste("Do not reject Ho:'",H0,"' The p-value(",pvalue,") is greather than the significance level(", levelSig,").",sep = "") 
  }else{
    H0<-switch(typeOfTest,
               "independency"="Las variables son independientes.",
               "goodnessOfFit"="Las frecuencias esperadas y las observadas son estadísticamente iguales."
    )
    reject<-paste("Rechazar Ho:",H0,"el valor p(",pvalue,") es menor que el nivel de significancia (", levelSig,").") 
    noReject<-paste("No rechazar Ho:",H0,"el valor p(",pvalue,") es mayor que el nivel de significancia (", levelSig,").")
  }
  
  if(identical(sTableText,"text")){
    msg<-""
    msg2<-paste(estat,"=",estadistico,"; df=",grados,".")
    if(pvalue < levelSig){
      msg<-paste(reject,msg2)
    }
    else{
      msg<-paste(noReject,msg2)
    }
    return(msg)
  }else{
    dfRes<-data.frame(param=estadistico,pvalue=pvalue,df=grados)
    return(dfRes)
  }
  
}

