library(ggplot2)
library(tidyverse)

source("C:/wmd/R-util-functions/Simula_Likert.R")

num_de_items=20
num_de_items2=16
escala_interpretada_1 = c("Muy malo","Malo","Regular","Bueno","Muy bueno")
escala_interpretada_2 = c("Muy baja","Baja","Media","Alta","Muy Alta")

entorno_familiar<-Simula_Likert(escala=1:5,
                                escala_interpretada =  escala_interpretada_1,
                                probabilidad =  c(.2,.2,.2,.2,.2),
                                numero_de_items =  num_de_items,
                                cantidad_respuestas =  120,
                                dsBase = NULL,
                                escala2 = 1:5,
                                escala_interpretada2 = escala_interpretada_2,
                                numero_de_items2 = num_de_items2,
                                porcentaje_aleatorio = 10,
                                corr_pos_neg = "+")

 

names(entorno_familiar)<-c(rep(paste("x",1:num_de_items,sep = "")),
            "suma_v1","Baremo_1",
            rep(paste("y",1:num_de_items2,sep = "")),
            "suma_v2","Baremo_2")

# ggplot(data=entorno_familiar,aes(x=suma_v1,y=suma_v2)) + 
#   geom_point(aes(color=Baremo_1)) + 
#   geom_smooth(method = "lm") + 
#   annotate("text", x = 60, y = 80,label = paste("r =", round(crr$estimate,4))) +
#   theme_bw() +
#   labs(x = "Variable 1", y = "Variable 2")


baremo_1<-factor(x=entorno_familiar$Baremo_1,
                 levels = escala_interpretada_1,
                 labels = c(1,2,3,4,5))
baremo_1<-factor(x=as.character(baremo_1),levels = c(1,2,3,4,5),
                   labels = escala_interpretada_1)

baremo_2<-factor(x=entorno_familiar$Baremo_2,
                 levels = escala_interpretada_2,
                 labels = c(1,2,3,4,5))
baremo_2<-factor(x=as.character(baremo_2),levels = c(1,2,3,4,5),
                 labels = escala_interpretada_2)


corrSpearman=cor.test(as.numeric(baremo_1),as.numeric(baremo_2),method = "spearman",exact = FALSE)

crr<-cor.test(entorno_familiar$suma_v1,entorno_familiar$suma_v2)
ggplot(data=entorno_familiar,aes(x=suma_v1,y=suma_v2)) + 
  geom_jitter(width = 1,height = 1,color="darkred") + 
  geom_smooth(method = "lm") + 
  annotate("text", x = min(entorno_familiar$suma_v1)+30, 
           y = max(entorno_familiar$suma_v1),
           label = paste("r =", round(crr$estimate,4),",", "p = ", sprintf("%.3f",round(crr$p.value,3)) )) +
  annotate("text", x = min(entorno_familiar$suma_v1)+30, 
           y = max(entorno_familiar$suma_v1)-10,
           label = paste("Rho =", round(corrSpearman$estimate,4),
                         ",", "p = ", sprintf("%.3f",round(corrSpearman$p.value,3)) )) +
  theme_bw() +
  labs(x = "Variable 1", y = "Variable 2")
