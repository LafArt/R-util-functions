LevenshteinD <-function(str.1,str.2){
  # str.1<-"casa"
  # str.2<-"tasa"
  
  
  str1=tolower(strsplit(str.1,split = "")[[1]]) 
  str2=tolower(strsplit(str.2,split = "")[[1]])
  porcentaje<-0;
  costo=0;
  m<-length(str1)+1
  n<-length(str2)+1
  d<-matrix(data = NA,nrow = m,ncol = n)
  d[,1]<-0:(m-1)
  d[1,]<-0:(n-1)
  
  for (i in 2:m) {
    for (j in 2:n) {
      costo<-ifelse(str1[i-1]==str2[j-1],0,1)
      d[i,j]<-min(min(d[i-1,j]+1,d[i,j-1]+1),d[i-1,j-1]+costo)
    }
    
  }
  
  if(m>n){
    porcentaje<-d[m,n]/length(str1)
  }else{
    porcentaje<-d[m,n]/length(str2)
  }
  return(list(d,porcentaje))
  
}

pr<-LevenshteinD("Gutiérrez","Gutierrez")

pr<-agrep("chiapas",catalogo,value=T)

actual<-c("jitotol","Bochil","Ixtapa","Ixhuatán","CAMPECHE")
catalogo<-c("Jitotol de zaragoza","Amatenango","Totoljava",
            "Berriosabal","Bochil","Ixtapangajoya","Ixtapalapa","Soyaló","Cumpich","Ixtapa del valle",
            "Ixtacomitán","Ixhuatlán del sureste","Ixhuatán de chiapas","Ixhuatán","Campeche")


resp<-sqldf("select Localidad from LocalidadesMXS where Localidad Like 'C%mp%ch%' ")

resp<-sqldf("select Localidad from tabascoLOC where Localidad = 'Campeche' ")

resp<-sqldf("select * from MunicipiosMX where idEst=4")



tabasco<-sqldf("select city from MAT2013 where state = 'CAMPECHE'")

tabascoLOC<-sqldf("select Localidad from LocalidadesMXS where Estado='Campeche' ")





actual<-tabasco$city[!is.na(tabasco$city)] 
catalogo<-trimws(tabascoLOC$Localidad,which = "both") 

actualvect<-c()

for (k in 1:length(actual)) {
  pesos<-c()
  for (m in 1:length(catalogo)) {
    pesos<-c(pesos,LevenshteinD(actual[k],catalogo[m])[[2]])
  }
  actualvect[k]<-which.min(pesos)
}

  dfMun<-data.frame(actual[1:length(actualvect)],catalogo[actualvect])
  dfMun

catalogo[2941]

actual

LevenshteinD("CAMPECHE","Cumpich")[[2]]

which.min(pesos)

which(catalogo=="Escárcega")


