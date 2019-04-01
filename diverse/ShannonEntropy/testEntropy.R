user<-Sys.info()[7]
user<-user[[1]]
rutacode<-paste("C:/Users/",user,"/Google Drive/R-util-functions/diverse/ShannonEntropy/",sep = "")
source(file=paste(rutacode,"informationGainMultiVariable.R",sep = ""),encoding = "UTF8")
source(file=paste(rutacode,"oneVectorEntropy.R",sep = ""),encoding = "UTF8")
source(file=paste(rutacode,"multiVectorEntropy.R",sep = ""),encoding = "UTF8")
source(file=paste(rutacode,"remainderMultiVariable.R",sep = ""),encoding = "UTF8")
source(file=paste(rutacode,"entropyWeightMultiVariable.R",sep = ""),encoding = "UTF8")
source(file=paste(rutacode,"totalTargetVectors.R",sep = ""),encoding = "UTF8")

datos<-read.csv(file="C:/Users/artur/Google Drive/DAIH/Paper komputer sapiens/ejemplo-enf.csv",header = T,sep = ",")

datos<-read.csv(file = paste("C:/Users/",user,"/Google Drive/R-util-functions/diverse/ShannonEntropy/breast_cancer.csv",sep = ""),header = T,sep = ",")
datos<-read.csv(file = paste("C:/Users/",user,"/Google Drive/R-util-functions/diverse/ShannonEntropy/ham-spam.csv",sep = ""),header = T,sep = ",")

datos[which(datos$v5=='?'),6]<-2


print(oneVectorEntropy(datos[,9]))
print(remainderMultiVariable(entropyWeightMultiVariable(datos)))

print(informationGainMultiVariable(datos))


user<-Sys.info()[7]
user<-user[[1]]
rutacode<-paste("C:/Users/",user,"/Google Drive/R-util-functions/diverse/ShannonEntropy/",sep = "")
source(file=paste(rutacode,"IGMV.R",sep = ""),encoding = "UTF8")

datos<-read.csv(file="C:/Users/artur/Google Drive/DAIH/Paper komputer sapiens/ejemplo-enf.csv",header = T,sep = ",")

print(IGMV(datos))
