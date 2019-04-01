# recibe un dataframe como este:
#   # nivel         entropia    peso
#   # nunca            0.34     0.21
#   # algunas veces    0.56     0.12
#   # siempre          0.14     0.30
# calcula el remainder
#regresa un valor nuemerico

remainderOneVariable<-function(dfWE){
  sum(apply(dfWE[,2:3],1,prod))
}
