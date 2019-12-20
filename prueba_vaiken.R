

v1<-sample(1:5,13,replace = T,prob = c(0.0,0.0,0.0,0.4,0.6))
v2<-sample(1:5,13,replace = T,prob = c(0.20,0.10,0.1,0.2,0.5))
v3<-sample(1:5,13,replace = T,prob = c(0.1,0.1,0.1,0.3,.4))
v4<-sample(1:5,13,replace = T,prob = c(0.1,0.1,0.1,0.1,.50))
v5<-sample(1:5,13,replace = T)
v6<-sample(1:5,13,replace = T)
v7<-sample(1:5,13,replace = T)
v8<-sample(1:5,13,replace = T,prob = c(0.1,0.1,0.1,0.3,.40))
v9<-sample(1:5,13,replace = T)
v10<-sample(1:5,13,replace = T,prob = c(0.1,0.1,0.1,0.1,.50))

validez<-data.frame(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)

mean(v1)

val<-v.aiken(data=validez,lsLikert = 5,c(1,2,4,8,10),c(3,5,6,7,9))
val
