Distancia<-c(100,200,400,800,1500,5000,10000,42195)

tiempoh <- c(9.84,19.32,43.19,102.58,215.78,787.96,1627.34,7956.00)

tiempom <- c( 10.84,22.12,48.25,117.73,240.83,899.88,1861.63,8765.00)

data <- cbind(Distancia,tiempoh,tiempom)

data<-data.frame(data)

colnames(data) <- c("Distancia", "Tiempo (hombres)","Tiempo (Mujeres")

class(data)

obj <- lm(tiempoh~Distancia,data)

coef(obj)

obj2 <- lm(tiempom~Distancia,data)

coef(obj2)

library(ggplot2)

ggplot(data,aes(x=Distancia,y=tiempoh))+geom_point()+geom_smooth(method = lm,se=F)


##############################

library(tidyverse)

algoh<- data%>%
  mutate(centrada=Distancia-mean(Distancia)) %>%
  lm(tiempoh~centrada, data=.)
  
algoh[["coefficients"]]

algom<- data%>%
  mutate(centrada=Distancia-mean(Distancia)) %>%
  lm(tiempom~centrada, data=.)

algom[["coefficients"]]



 #Grafico
data%>%
  mutate(centrada=Distancia-mean(Distancia))%>%
  lm(tiempoh~centrada, data=.)%>%
  ggplot(.,aes(x=centrada,y=tiempoh))+geom_point()+geom_smooth(method = lm,se=F)


#Son los hombres más rápidos?

modh <- lm(tiempoh~Distancia,data = data)

sum(coef(modh)*c(1,3000))

predict(modh,newdata=data.frame(Distancia=3000))
















































