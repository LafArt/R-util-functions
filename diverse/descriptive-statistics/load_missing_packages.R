#require_pack: Is an string vector containing the list of required packages
load_missing_packages<-function(require_pack){
  new_pack <- require_pack[!(require_pack %in% installed.packages()[,"Package"])]
  if(length(new_pack)) install.packages(new_pack)
  no_loaded_pack<-require_pack[!(require_pack %in% (.packages()))]
  if(length(no_loaded_pack)) lapply(no_loaded_pack,require,character.only = TRUE)
  #eval(parse(text=paste("library(",list.of.packages,")"))) #este codigo tambien podria usarse para
  #cargar los paquetes que hacen falta siendo 'list.of.packages' un vector string con la lista de paquetes
  return(list(new_pack,no_loaded_pack))
}