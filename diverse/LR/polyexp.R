polyexp<-function(matrixVarInd, grado){
  matrixVarInd<-as.matrix(matrixVarInd)
  matRes<-matrix(nrow =dim(matrixVarInd)[1],ncol = 0)  
  nvi<-dim(matrixVarInd)[2]
   for(i in 1:grado){
      if(nvi>1){
        for(j in 0:i){
          if(nvi>2){
              for(k in 0:j){
                if(nvi>3){
                  for(l in 0:k){
                    if(nvi>4){
                        for(m in 0:l){
                          if(nvi>5){
                              for(n in 0:m){
                                if(nvi>6){
                                  for(o in 0:n){
                                    matRes<-cbind(matRes,(matrixVarInd[,1]^(i-j))*(matrixVarInd[,2]^(j-k))*(matrixVarInd[,3]^(k-l))*(matrixVarInd[,4]^(l-m))*(matrixVarInd[,5]^(m-n))*(matrixVarInd[,6]^(n-o))*(matrixVarInd[,7]^(o)))
                                  }
                                }else{
                                  matRes<-cbind(matRes,(matrixVarInd[,1]^(i-j))*(matrixVarInd[,2]^(j-k))*(matrixVarInd[,3]^(k-l))*(matrixVarInd[,4]^(l-m))*(matrixVarInd[,5]^(m-n))*(matrixVarInd[,6]^(n)))
                                }
                              }
                          }else{
                            matRes<-cbind(matRes,(matrixVarInd[,1]^(i-j))*(matrixVarInd[,2]^(j-k))*(matrixVarInd[,3]^(k-l))*(matrixVarInd[,4]^(l-m))*(matrixVarInd[,5]^(m)))
                          }
                        }
                    }else{
                      matRes<-cbind(matRes,(matrixVarInd[,1]^(i-j))*(matrixVarInd[,2]^(j-k))*(matrixVarInd[,3]^(k-l))*(matrixVarInd[,4]^(l)))
                    }
                  }
                }else{
                  matRes<-cbind(matRes,(matrixVarInd[,1]^(i-j))*(matrixVarInd[,2]^(j-k))*(matrixVarInd[,3]^(k)))
                }
              }    
          }else{
            matRes<-cbind(matRes,(matrixVarInd[,1]^(i-j))*(matrixVarInd[,2]^j))
          }
        }
      }else{
        matRes<-cbind(matRes,matrixVarInd[,1]^i)
      }
    } 
  return(data.frame(matRes))
}
  
