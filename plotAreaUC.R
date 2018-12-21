#author: LAHA
#date: 6/11/2018
#function to plot a funtion of x
plotAreaUC<-function(xfrom,xto,xby,xli,xls,f,colorAUC="gray",transpAUC=1){
  library(ggplot2)
  x<-seq(xfrom,xto, xby)
  y<- f(x) # dnorm(x,0,1)
  xddf <- data.frame(x=x,y=y)

  p<-ggplot(data=xddf,aes(x=x,y=y))+
    geom_line(size=1,color="black")+
    geom_ribbon(data=subset(xddf ,x>xli & x<xls),aes(ymax=y),ymin=0,
                fill=colorAUC,colour=NA,alpha=transpAUC)+
    scale_y_continuous(limits=c(min(y), max(y)))+
    geom_vline(data = data.frame(x),aes(xintercept=0),color="darkgray")+
    geom_hline(data = data.frame(y),aes(yintercept=0),color="darkgray")+
    theme_bw()
  
  p
}


xfrom<--6
xto<-6
xby<-.01
xli<-2.5
xls<-6
f<- function(x) {x^3}
colorAUC="green"
transpAUC=.5

plotAreaUC(xfrom,xto,xby,xli,xls,f,colorAUC,transpAUC=transpAUC)
# 
# 
# 7*sin(x)+x^2-4
# 
# y<-f(seq(xfrom,xto,xby))

# 
# x<-seq(-1,1, 0.01)
# y<- x^3 # dnorm(x,0,1)
# xddf <- data.frame(x=x,y=y)
# p<-ggplot(data=xddf,aes(x=x,y=y))+geom_line(size=1,color="black")+
#   geom_ribbon(data=subset(xddf ,x>0.2 & x<0.8),aes(ymax=y),ymin=0,
#               fill="gray",colour=NA,alpha=0.7)+
#   scale_y_continuous(limits=c(-1, 1))+theme_minimal()
# 
# p+geom_vline(data = data.frame(x),aes(xintercept=0),color="darkgray")+
#   geom_hline(data = data.frame(y),aes(yintercept=0),color="darkgray")