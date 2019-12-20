Simula_Likert<-function(escala,escala_interpretada,
                        probabilidad,numero_de_items,
                        cantidad_respuestas,
                        dsBase=NULL,escala2,escala_interpretada2,
                        numero_de_items2,
                        porcentaje_aleatorio=0,corr_pos_neg="+"){
  library(dplyr)
  
  # escala: escala de balores numérica, un vector, or ejemplo: 1:5 
  # en una escala de 5 niveles
  # escala_interpretada: es el baremo para la escala, por ejemplo, 
  # "Totalmente en desacuerdo","En desacuerdo", "De Acuerdo", ...
  # probabilidad: Vector de probabilidades para dar sesgo a la encuesta si así se desea
  # numero_de_items: la cantidad de items del instrumento
  # cantidad_de_respuestas: La cantidad de personas que se simulará que contestaron la respuesta
  
  #Para probar la función antes de utilizarla
  # escala=1:5
  # numero_de_items=20
  # cantidad_respuestas=100
  # probabilidad=c(0.25,0.2,0.3,0.15,0.10)
  # escala_interpretada=c("Totalmente en desacuerdo","En desacuerdo",
  # "Indiferente","De acuerdo","Totalmente de Acuerdo")
  # rm(escala)
  # rm(cantidad_respuestas)
  # rm(numero_de_items)
  # rm(probabilidad)
  # rm(escala_interpretada)
  #Para probar la función antes de utilizarla
  
  genera_rangos<-function(escala_gr,numero_de_items_gr){
    limI=min(escala_gr)*numero_de_items_gr
    limS=max(escala_gr)*numero_de_items_gr
    rango=limS-limI
    tamano_intervalo=floor(rango/length(escala_gr)) 
    niveles=matrix(nrow = length(escala_gr),ncol = 2)
    controlLI<-limI
    for (i in 1:length(escala_gr)) {
      
      niveles[i,1]=controlLI
      niveles[i,2]=ifelse(i<length(escala_gr),
                          controlLI+tamano_intervalo-1,limS)
      controlLI=controlLI+tamano_intervalo
    }
    return(list(limS,limI,niveles))
  }
  
  genera_baremo<-function(numero=75,
                          niveles_baremo=matrix(rep(0,10),ncol = 2),
                          escala_interpretada_br){
    b=TRUE
    i=1
    while(b){
      nivel=escala_interpretada_br[1]
      if((numero >= niveles_baremo[i,1]) & (numero <= niveles_baremo[i,2])){
        
        nivel=escala_interpretada_br[i]
        b=FALSE
      }
      
      i=i+1
    }
    return(nivel)
  }
  
  genera_respuestas<-function(varx,escala_grp,
                              cantidad_items_grp,
                              probabilidad_grp){
    #probabilidad_ms=
    respuesta=varx*sample(escala_grp,
                          cantidad_items_grp,replace = TRUE,
                          prob = probabilidad_grp)
    #print("RESPUESTA")
    #print(respuesta)
    if(sum(respuesta)%%17==0){
      probabilidad_grp=c(.1,.1,.1,.3,.8)
      respuesta=sample(escala_grp,
                       cantidad_items_grp,
                       replace = TRUE,prob = probabilidad_grp)

    }else if(sum(respuesta)%%13==0){
      probabilidad_grp=c(.6,.2,.1,.05,.05)
      #print(paste("sum respuesta antes:",sum(respuesta)))
      respuesta=sample(escala_grp,
                       cantidad_items_grp,
                       replace = TRUE,
                       prob = probabilidad_grp)
      #print(paste("sum respuesta despues:",sum(respuesta)))
    }
    return(respuesta)  
  }
  
  
  genera_matriz_simulacion<-function(escala_ms,
                                     escala_interpretada_ms,
                                     probabilidad_ms,
                                     numero_de_items_ms,
                                     cantidad_respuestas_ms){
  
    gr<-genera_rangos(escala_ms,numero_de_items_ms)
    niveles<-gr[[3]]
    print(niveles)
    #crea el espacio (una matriz) para almacenar lo números que se vana a simular
    #rellena la matriz de unos para que al generar los aleatorios (sample) y combinar
    #la función apply con genera respuestas (que utiliza sample) se obtengan los valores deseados
    tbDatos<-matrix(rep(1,cantidad_respuestas_ms*numero_de_items_ms),nrow = cantidad_respuestas_ms,ncol = numero_de_items_ms)
    names(tbDatos)<-rep(paste("x",1:numero_de_items_ms,sep = ""))
    #genera los números aleatorios con base en la probabilidad, por renglones
    #se debe transponer porque apply genera los resultados transpuestos
    #si se ejecuta apply por columnas y en la función genera_respuestas
    #se genera un sólo valor, el valor es el mismo para cada respuesta en la 
    #columna. No se genera una encuesta con variabilidad.
    X <- t(apply(tbDatos,1,genera_respuestas,
               escala_grp = escala_ms,
               cantidad_items_grp = numero_de_items_ms,
               probabilidad_grp=probabilidad_ms))
    
    print(X)
    X <- data.frame(X,suma_v1=apply(X,1,sum))
    X <- data.frame(X,
                    baremo=sapply(X$suma_v1,FUN=genera_baremo,
                                  niveles_baremo=niveles,
                                  escala_interpretada_br=escala_interpretada_ms))
    return(X)
  }
  
  
  
  variable1<-genera_matriz_simulacion(escala_ms = escala,
                                      escala_interpretada_ms = escala_interpretada,
                                      probabilidad_ms = probabilidad,
                                      numero_de_items_ms = numero_de_items,
                                      cantidad_respuestas_ms = cantidad_respuestas)
  
  
  respuestas_consistentes=floor(cantidad_respuestas*(1-porcentaje_aleatorio/100))
  
  
  variable2<-variable1[1:respuestas_consistentes,1:numero_de_items2]
  
  #recodifica los valores para generar una correlación negativa
  if(corr_pos_neg=="-")
    variable2<- apply(variable2, 2, function(x) (max(escala)+1)-x) 
    
  
  respuestas_aleatorias=  cantidad_respuestas-respuestas_consistentes
  tamano_matriz_aleatoria=respuestas_aleatorias*numero_de_items2
  vector_aleatorio=sample(escala,tamano_matriz_aleatoria,replace = TRUE,prob = probabilidad)
  matriz_aleatoria=data.frame(matrix(vector_aleatorio,nrow = respuestas_aleatorias))
  
  # names(variable2)<-rep(paste("y",1:numero_de_items2,sep = ""))
  # print("Variable:")
  # print(names(variable2))
  # print("matriz:")
  # names(matriz_aleatoria)<-rep(paste("y",1:numero_de_items2,sep=""))
  # print(names(matriz_aleatoria))
  variable2<-rbind.data.frame(variable2,matriz_aleatoria)
  variable2<-data.frame(variable2,suma_v2=apply(variable2,1,sum))
  
  gr_v2<-genera_rangos(escala2,numero_de_items2)
  niveles_v2<-gr_v2[[3]]
  variable2 <- data.frame(variable2,
                  baremo=sapply(variable2$suma_v2,FUN=genera_baremo,
                                niveles_baremo=niveles_v2,
                                escala_interpretada_br=escala_interpretada2))

    
  X<-data.frame(variable1,variable2)
  
  
  
  return(X)
  
}

