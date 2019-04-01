#Función para graficar el histograma de variables categóricas y continuas
#Parámetros:
#1.- variable: Vector con los datos de la variable que se desea graficar
#2.- tipoVariable: categorica|continua, el default es categorica
#3.- tituloy: Título para el eje y
#4.- titulox: Título para el eje x
#5.- tituloGeneral: Título del gráfico, si se desea omitir pasar como parámetro tituloGeneral=""
#6.- colorRelleno: El color de las barras en el gráfico
#7.- probva: probabilidad para considerar los valores atipicos, como default se establece 0.997
      #que considera valores normales aquellos dentro de las 3 desviaciones estandares
      #0.95 tomaría en cuenta hasta dos desviaciones estándar
      #0.68 una desviaciónestándar
      #1 equivale a la muestra completa, no elimina valores atípicos
      #(puede usarse todos los demás valores entre 0 y 1, por ejemplo podría utilizarse 0.97, 
      #que es un valor intermedio entre tres y dos desv. est.)
#8.- metodoNclases: Metodo para calcular el numero de clases
#     laha: Metodo propio donde para n<=400 la formula es sqrt(n) y para n>400 es raiz quinta de n cuadrada
#     FD: usa el método de Freedman-Diaconis (1981)
#     SC: usa el método de Scott (1979)
#     SG: usa el metodo de Sturges (1926)

#Resultado
  #Regresa una lista con dos elementos
  # [[1]]: Para variables categóricas, tabla de frecuencias
  #        Para variables continuas, Resumen de descriptivo
  # [[2]]: Para variables categóricas, histograma por clases
  # [[2]]: Para variables continuas, histograma con densidad de probabilidad
  # [[3]]: Vector con los valores atípicos

#algunas ideas fueron tomadas de https://rstudio-pubs-static.s3.amazonaws.com/255429_0653e8cb1e244ef997a69f266ac79630.html

genera_histograma<-function(variable,
                          tipoVariable="categorica",
                          tituloy="Frecuencia",
                          titulox="Clases",
                          tituloGeneral="Histograma",
                          colorRelleno="blue",probva=0.997,metodoNclases="laha"){
  #for test only
  # variable<-data$presionDiastolica #,talla #for test only
  # tipoVariable="categorica"
  # tituloy="Frecuencia"
  # titulox="Clases"
  # tituloGeneral="Histograma"
  # colorRelleno="blue"
  # probva=0.997
  # metodoNclases="laha"
  #for test only
  #Carga paquetes
  source(file="C:/wmd/R-util-functions/diverse/descriptive-statistics/load_missing_packages.R",encoding = "UTF8")
  
  load_missing_packages(c("ggplot2","magrittr","outliers","RColorBrewer","grDevices"))
  
  #elimina NA'S
  variable<-as.numeric(as.character(variable[!is.na(variable)]))
  
  #elimina outliers mayores a tres desviaiones estandar
  #ref:http://mazamascience.com/WorkingWithData/?p=912
  var = tryCatch({
    iatipicos<-(scores(variable,type = "z",prob = probva)%>%which(.))
    vatipicos<-variable[iatipicos]
    variable<-variable[-iatipicos]
    list(variable,vatipicos)
  }, warning = function(w) {
    print("Exist a warning")
    return(list(variable,NULL))
  }, error = function(e) {
    print("Exist an error")
    return(list(variable,NULL))
  }, finally = {
    
  })
  variable<-var[[1]]
  vatipicos<-var[[2]]
  
  
  if(identical(tipoVariable,"continua")){
    df<-data.frame(variable)
    histograma<-ggplot(df,aes(x=variable)) + 
      geom_histogram(aes(y=..density..), colour="black", fill=colorRelleno,bins = 30)+
      geom_density(alpha=.2, fill="#FCF3CF")+
      labs(y=tituloy,x=titulox,title=tituloGeneral)
    df<-summary(variable)
    dff<-NULL
    for (i in 1:length(df)) {
      dff<-cbind(dff,df[[i]])
    }
    dff<-data.frame(dff)
    names(dff)<-c("Min","Q1","Mediana","Media","Q3","Max","Faltantes")[1:length(dff)]
    #df<-data.frame(Min=df[[1]],Q1=df[[2]],Mediana=df[[3]],Media=df[[4]],Q3=df[[5]],Max=df[[6]],Faltantes=df[[7]])
    return(list(dff,histograma,vatipicos))
  }  
  
  generaIntervalos<-function(variable){
    ifelse(length(variable)<=400,nclaseslaha<-sqrt(length(variable)),nclaseslaha<-length(variable)^(2/5))
    nclases<-switch(tolower(metodoNclases),
      "laha"=nclaseslaha,
      "fd"=nclass.FD(variable),
      "sc"=nclass.scott(variable),
      "sg"=nclass.Sturges(variable)
    )
    
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
  
  
  histograma <- ggplot(data = tf, aes(x = Clase, y = Frec)) + 
    geom_bar(stat = "identity", fill = colorRelleno) + 
    theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    geom_text(aes(label=Frec), vjust=-0.3, size=3)+
    labs(y=tituloy,x=titulox,title=tituloGeneral)
  
  return(list(tf,histograma,vatipicos))
  
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