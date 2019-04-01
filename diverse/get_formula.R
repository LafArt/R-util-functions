#contruye una formula utilizando un vector de nombres que contiene todos los nombres de las
#variables en juego, incluyendo la variable objetivo
#el otro parametro indica cual es la variable objetivo que en este caso será la variable dependiente en la
#formula.
get_formula<-function(vec_names,var_objetivo){
  var_objetivo_f<-paste(var_objetivo,"~",sep = "")
  f<-as.formula(paste(var_objetivo_f,paste(vec_names[!vec_names %in% var_objetivo],collapse = " + "))) 
  return(f)
}