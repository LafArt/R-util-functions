
user<-Sys.info()[7]
user<-user[[1]]
rutautiles<-paste("C:/wmd/R-util-functions/diverse/descriptive-statistics/",sep = "")
source(file=paste(rutautiles,"genera_histograma.R",sep = ""),encoding = "UTF8")

#load_missing_packages(c("ggplot2","magrittr","outliers","RColorBrewer","grDevices"))


#paquete que genera una tabla de frecuencias. No hace verificación de valores atípicos 
#su construcción es en dos pasos nada intuitivos.
#se opta por hacer la propia
# library(agricolae)
# 
# paso1<-with(data,graph.freq(presionDiastolica,plot = F))
# tfpd<-table.freq(paso1)

load('data.Rdata')

iris$Sepal.Length

h<-genera_histograma(iris$Sepal.Length,tipoVariable = "continua",
                     colorRelleno = "white",probva=0.997,metodoNclases = "sg")

h[[1]]
h[[2]]
h[[3]]


h<-genera_histograma(data$presionDiastolica,
                     tipoVariable = "categorica",colorRelleno = "Red",probva = 0.999,metodoNclases = "laha")
h[[1]]
h[[2]]
h[[3]]

h<-genera_histograma(data$peso,tipoVariable = "categorica",colorRelleno = "Red",probva=0.997)
h[[1]]
h[[2]]
h[[3]]


#estructura de try catch
result = tryCatch({
  expr
}, warning = function(w) {
  warning-handler-code
}, error = function(e) {
  error-handler-code
}, finally = {
  cleanup-code
})