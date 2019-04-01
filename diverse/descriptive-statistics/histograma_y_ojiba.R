histograma = function(X, which = 1) {
  if (which == 1) 
  {
    k = nclass.Sturges(X)
  }  #Si which = 1, calcula Histograma y Tabla de Frecuencia con Sturges.
  if (which == 2) 
  {
    k = nclass.scott(X)
  }  #Si which = 2, calcula Histograma y Tabla de Frecuencia con Scott.
  if (which == 3) 
  {
    k = nclass.FD(X)
  }  #Si which = 3, calcula Histograma y Tabla de Frecuencia con Freedman.
  Rango = ceiling(max(X) - min(X))  #Cálculo rango
  a = round(Rango/k, 1)  #Cálculo ancho de clase.
  c = abs(round(a * k - Rango, 1))
  
  LimClasInf = numeric(k)  #Cálculo límites de clase inferiores
  LimClasInf[1] = min(X) - c/2
  for (i in 2:k) {
    LimClasInf[i] = LimClasInf[i - 1] + a
  }
  LimClasInf = round(LimClasInf, 2)
  
  LimClasSup = numeric(k)  #Cálculo limites de clase superiores
  LimClasSup[1] = LimClasInf[1] + a
  for (i in 2:k) {
    LimClasSup[i] = LimClasSup[i - 1] + a
  }
  LimClasSup = round(LimClasSup, 2)
  
  MarClas = numeric(k)  #Cálculo marcas de clase
  for (i in 1:k) {
    MarClas[i] = (LimClasInf[i] + LimClasSup[i])/2
  }
  MarClas = round(MarClas, 2)
  
  Freq = numeric(k)
  for (i in 1:k) {
    Freq[i] = length(X[X >= LimClasInf[i] & X < LimClasSup[i]])
  }
  FreqAc = cumsum(Freq)  #Cálculo Frecuencias Acumuladas
  Rel = round(Freq/sum(Freq), 4)  #Cálculo Frecuencias Relativas
  RelAc = round(cumsum(Rel), 4)  #Cálculo de Frecuencias Relativas Acumuladas 
  
  # Tabla de Frecuencias
  TabFreq = data.frame(LimClasInf, LimClasSup, MarClas, Freq, FreqAc, Rel, 
                       RelAc)
  print(TabFreq)
  
  par(mfrow = c(1, 2))
  
  # Histograma con Polígono de Frecuencias
  TabFreq.bar = barplot(TabFreq$Freq, space = 0, font = 2, col.main = "darkgreen", 
                        main = "Histograma y Polígono de Frecuencias", xlab = "Datos", ylab = "Frecuencias", 
                        names.arg = MarClas, col = terrain.colors(8))
  lines(x = TabFreq.bar, y = TabFreq$Freq, col = "red")
  points(x = TabFreq.bar, y = TabFreq$Freq, col = "red")
  
  # Generando Ojiva
  plot(TabFreq$MarClas, TabFreq$RelAc, col = "red", ylab = "Frecuencias Relativas Acumuladas", 
       xlab = "Datos")
  lines(TabFreq$MarClas, TabFreq$RelAc, col = "red")
  title(main = "Ojiva", col.main = "darkgreen")
}

load('data.Rdata')
Peso<-data[!is.na(data$peso),"peso"] 

set.seed(3211)
X=round(rnorm(200,20,5),1) 
histograma(Peso) # Rutina 1