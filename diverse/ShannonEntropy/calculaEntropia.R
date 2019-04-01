calculaEntropia<-function(variable){
  #Author: Charles Determan Jr.
  #Grade: Data Scientist at Healthgrades
  #linkedin: https://www.linkedin.com/in/cdeterman
  #edited by: Arturo Laflor
  
  freqs <- table(variable)/length(variable)
  -sum(freqs * log2(freqs))  
}
