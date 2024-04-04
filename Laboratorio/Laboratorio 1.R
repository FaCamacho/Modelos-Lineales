library(readxl)
library(tidyverse)

#Cargamos los datos

condados <- read_excel("Laboratorio/condados.xlsx",na="NA")

class(condados)

condados<-as.data.frame(condados)

class(condados)

table(condados$estado)

#¿Cuántos condados hay por estado?

cuenta <- condados %>% group_by(.,estado) %>% count(.) 

#Nos quedamos con Florida

Florida <- condados %>% filter(.,estado == "Florida")

str(Florida$esp_vida)

#Graficamos la esperanza de vida en Florida

ggplot(data=Florida)+geom_histogram(mapping = aes(x=esp_vida))

ggplot(data=Florida)+geom_boxplot(mapping = aes(x=esp_vida))


#Variables explicativas X


variables_X <- Florida %>% select(c(esp_vida,pct_diabetes,pct_obesidad,pct_est_terc,pct_bpeso,ing_med_hog))


b <- summary(variables_X)
 


varianzas <- c(var(variables_X$pct_diabetes),var(variables_X$pct_obesidad),var(variables_X$pct_est_terc),var(variables_X$pct_bpeso),var(variables_X$ing_med_hog))


desvios <- c(sd(variables_X$pct_diabetes),sd(variables_X$pct_obesidad),sd(variables_X$pct_est_terc),sd(variables_X$pct_bpeso),sd(variables_X$ing_med_hog))

dispersion <- as.data.frame(rbind(varianzas,desvios))

minimo <- variables_X %>% summarise_all(.,list(min))

mediana <- variables_X %>% summarise_all(.,list(median))

media <- variables_X %>% summarise_all(.,list(mean))

maximo <- variables_X %>% summarise_all(.,list(max))

varianza <- variables_X %>% summarise_all(.,list(var))

desvio <- variables_X %>% summarise_all(.,list(sd))


as.data.frame(dispersion)

class(dispersion)

descriptiva <- rbind(minimo,mediana,media,maximo,varianza,desvio)

rownames(descriptiva)<-c("Min","Med","Mean","Max","Var","Sd")


  
  
  #Analisis descriptivo bivariado


ggplot(data=Florida)+geom_point(aes(x=esp_vida,y=pct_diabetes))

ggplot(data=Florida)+geom_point(aes(x=esp_vida,y=pct_obesidad))

ggplot(data=Florida)+geom_point(aes(x=esp_vida,y=pct_est_terc))

ggplot(data=Florida)+geom_point(aes(x=esp_vida,y=pct_bpeso))

ggplot(data=Florida)+geom_point(aes(x=esp_vida,y=ing_med_hog))



#Estime el valor de correlacion de la esperanza de vida con cada variable explicativa


round(cor(variables_X),4)




#Estime la correlacion de variables explicativas entre ellas




#Regresion lineal simple


mod1 <- lm(esp_vida ~ ing_med_hog, data = variables_X)

coef(mod1)

help(I)

mod2 <- lm(esp_vida ~ I(ing_med_hog/1000), data = variables_X)

coef(mod2)

#Cuanto vale R2?


summary(mod2)



0.5509**2

#Agregue la recta de regresion al diagrama de dispersion



ggplot(data=Florida,aes(x=esp_vida,y=pct_bpeso))+geom_point()+geom_smooth(method = "lm", se=F, col =2, size =2)

ggplot(data=Florida,aes(x=esp_vida,y=pct_bpeso))+geom_point()+geom_smooth(method = "lm", se=F, col =2, size =2)

ggplot(data=Florida,aes(x=esp_vida,y=pct_bpeso))+geom_point()+geom_smooth(method = "lm", se=F, col =2, size =2)

ggplot(data=Florida,aes(x=esp_vida,y=pct_bpeso))+geom_point()+geom_smooth(method = "lm", se=F, col =2, size =2)

ggplot(data=Florida, aes(x=esp_vida,y=ing_med_hog))+geom_point()+geom_smooth(method = "lm", se=F, col =2, size =2)





















































