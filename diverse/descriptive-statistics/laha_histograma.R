#Funci�n para graficar el histograma de variables categóricas y continuas
#Parámetros:
#1.- variable: Vector con los datos de la variable que se desea graficar
#2.- tipoVariable: categorica|continua, el default es categorica
#3.- tituloy: Título para el eje y
#4.- titulox: Título para el eje x
#5.- tituloGeneral: Título del gráfico, si se desea omitir pasar como parámetro tituloGeneral=""
#6.- colorRelleno: El color de las barras en el gráfico
#Resultado
  #Regresa una lista con dos elementos
  # [[1]]: Para variables categóricas, tabla de frecuencias
  #        Para variables continuas, Resumen de descriptivo
  # [[2]]: Para variables categóricas, histograma por clases
  # [[2]]: Para variables continuas, histograma con densidad de probabilidad
laha_histograma<-function(variable,
                          tipoVariable="categorica",
                          tituloy="Frecuencia",
                          titulox="Clases",
                          tituloGeneral="Histograma",
                          colorRelleno="blue"){
  #variable<-talla
  
  variable<-as.numeric(as.character(variable[!is.na(variable)]))
  
  if(identical(tipoVariable,"continua")){
    df<-data.frame(variable)
    histograma<-ggplot(df,aes(x=variable)) + 
      geom_histogram(aes(y=..density..), colour="black", fill=colorRelleno,bins = 30)+
      geom_density(alpha=.2, fill="#FCF3CF")+
      labs(y=tituloy,x=titulox,title=tituloGeneral)
    df<-summary(talla)
    df<-data.frame(Min=df[[1]],Q1=df[[2]],Mediana=df[[3]],Media=df[[4]],Q3=df[[5]],Max=df[[6]],Faltantes=df[[7]])
    return(list(df,histograma))
  }  
  
  generaIntervalos<-function(variable){
    ifelse(length(variable)<=400,nclases<-sqrt(length(variable)),nclases<-length(variable)^(2/5)) 
    rango<-max(variable)-min(variable)
    lclase<-round(rango/nclases,digits = 0)
    j=1
    linf<-c()
    lsup<-c()
    for (i in seq(from=min(variable),to=max(variable),by=lclase)) {
      linf[j]<-i
      lsup[j]<-i+lclase-1
      j<-j+1
    }
    intervalos<-data.frame(Linf=linf,Lsup=lsup)
  }
  
  tablaFrecuencias<-function(variable,intervalos){
    frecuencia<-c()
    FrecAcum<-c()
    for (i in 1:dim(intervalos)[1]) {
      frecuencia[i]<-length(variable[(variable>=intervalos$Linf[i]) & (variable<=intervalos$Lsup[i])])
      if(i==1){
        FrecAcum[i]<-frecuencia[i]
      }else{
        FrecAcum[i]<-FrecAcum[i-1]+frecuencia[i]
      }
      
    }
    Porc<-round(frecuencia/sum(frecuencia),digits = 4) 
    PorcAcum<-round(FrecAcum/sum(frecuencia),digits = 4)
    tablaFrecc<-data.frame(intervalos,Clase=paste(intervalos$Linf,intervalos$Lsup,sep = "-"),
                           MClase=(intervalos$Linf+intervalos$Lsup)/2,
                           Frec=frecuencia,FrecAcum,Porc,PorcAcum)
  }
  
  #crea los intervalos
  I<-generaIntervalos(variable = variable)
  
  #genera la tabla de frecuencias
  tf<-tablaFrecuencias(variable = variable,I)
  
  #ordena las clases
  tf$Clase<- factor(tf$Clase,levels = tf$Clase[order(tf$Linf)])
  
  
  library(ggplot2)
  
  histograma <- ggplot(data = tf, aes(x = Clase, y = Frec)) + 
    geom_bar(stat = "identity", fill = colorRelleno) + 
    theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    geom_text(aes(label=Frec), vjust=-0.3, size=3)+
    labs(y=tituloy,x=titulox,title=tituloGeneral)
  
  
  return(list(tf,histograma))
  
}
#Paletas de colores para hacer gráficos
# autumnPalette<-c("#6D7696", "#59484F", "#455C4F", "#CC5543", "#EDB579", "#DBE6AF")
# tomatoTones<-c("#D6CFC9", "#C2C290", "#4A572C", "#803018", "#E34819", "#E87F60")
# canyonColor<-c("#6E352C", "#CF5230", "#F59A44", "#E3C598", "#8A6E64", "#6E612F")
# autumnTones<-c("#D1CEC5", "#997C67", "#755330", "#B0703C", "#DBA72E", "#E3CCA1")
# colorBasket<-c("#E6E2DF", "#B2E3E8", "#CCB8D1", "#966C5D", "#452B29", "#8F8172")
# colorFresh<-c("#D9D9D9", "#F5B3B4", "#D15656", "#94353C", "#47322D", "#996B42")
# mineralTones<-c("#694364", "#B58BAB", "#E3D1E2", "#E8E4E1", "#C4C4C0", "#CCA772")  
# autumnSpice<-c("#EBE3D9", "#E0CDAF", "#C2BC74", "#6E615A", "#807E82", "#B8B8B8")
# chiliColor<-c("#283811", "#66492F", "#B8997F", "#A68887", "#D94330", "#5C0811")
# spicedPalette<-c("#F7EFD4", "#FADDAF", "#EB712F", "#91371B", "#472C25", "#D4C2B2") 