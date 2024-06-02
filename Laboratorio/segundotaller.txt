#CARGAR DATOS, primero elegir el directorio donde guardamos el archivo (descargas) 

library(readxl)
condados <- read_excel("condados.xlsx")
View(condados)

head(condados)

#me quedo con la parte de la tabla donde la variable estado vale Alabama
library(tidyverse)
ALABAMA=condados%>%filter(., estado=="Alabama")
dim(ALABAMA)

#Generamos el modelo RLM, variable respuesta=esp_vida y 5 variables explicativas.
mod<-lm(esp_vida ~ pct_diabetes+pct_obesidad+pct_est_terc+ing_med_hog+pct_bpeso, data=ALABAMA )
#vemos los coeficientes de cada variable explicativa redondeando a 4
round(coef(mod),4)

#se hace lo mismo lo único que se divide el ingreso entre 1000 para poder interpretarlo mejor
alabama<-ALABAMA%>%mutate(imh=ing_med_hog/1000)
mod<-lm(esp_vida ~ pct_diabetes+pct_obesidad+pct_est_terc+imh+pct_bpeso, data=alabama )
coef(mod)

#calcular R2 explicación: las 5 variables explicativas explican el 52% de la variabilidad de la esperanza de vida (las variables expl explican el R2 de la variabilidad de y)

summary(mod)

names(summary(mod))

#creamos variables ygorro y residuos
y_gorro<-y_gorro=fitted(mod)
e_gorro=residuals(mod)

alabama<-cbind(alabama,y_gorro,e_gorro)

#graficamos relación entre ygroo y residuos
library(ggplot2)
ggplot(alabama, aes(x=y_gorro, y=e_gorro)) + geom_point()

#el modelo es singificativo para cualquier valor de alfa porque el p_valor es muy chico, RECHAZO EL MODELO (ES SIGNIFICTAIVO CUANDO: ALFA > P_VALOR) (ALFA LO VEMOS EN SUMMARY(MODELO) es F)

#CUANDO VAMOS A DECIR ALGO DEL MODELO TENEMOS QUE MIRAR LAS VARIABLES QUE SON SIGNIFICATIVAS, HAY QUE COMPARAR LA ÚKTIMA COLUMNA DE SUMMARY CON EL NIVEL DE SIGNIFICACION QUE QUEREMOS

#LOS GRADOS DE LIBERTAD SON LOS QUE ESTÁN AL LADO DE DF, SE PUEDE CALCULAR COMO n-k-1
#beta gorro lo podemos calcular como: el devio de beta por t, lo vemos en summary

drop1(mod)
#RSS: suma de residuos al cuadrado

drop1(mod, test="F")
#agregamos la columna del p_valor, se hace F al cuadrado y pasa a distribuir t con 1 grado de libertad
#nos da un summary mejor si tenemos variables cualitativas


#ponemos a prueba ver si tres de las variables explic pueden valer 0, para eso hacemos un nuevo modelo donde solo tomamos las demás vari expl
mod2<-lm(esp_vida~pct_obesidad+imh, data=alabama)
coef(mod2)
summary(mod2)

anova(mod2, mod)

#el p_valor es alto (0.295), por lo que no rechazo H0, es decir las variables si pueden valer 0, por lo que teniendo en cuenta unicamente las var de mod2 se puede hacer el modelo perfectamente

#de dond sale el p_valor? p= 1-pf(F, variables que valen 0, grados de lib)