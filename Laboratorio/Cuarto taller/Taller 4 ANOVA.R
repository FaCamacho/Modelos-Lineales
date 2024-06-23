

library(tidyverse)


library(readxl)
datos <- read_excel("audicion.xlsx")

str(datos)

datos <- as.data.frame(datos)

str(datos)

summary(datos)


#Analizar descriptivamente los datos

summary(datos$post)

hist(datos$post)

boxplot(datos$post)

#3

datos%>%
  group_by(tratamiento) %>%
  summarise(media=mean(post), desvio= sd(post),n_trat=n())
  
  ggplot(datos,aes(x=post))+geom_boxplot()

datos  %>%
  group_by(sexo) %>%
  summarise(media=mean(post), desvio= sd(post),
            n_trat=n())

#4

ggplot(datos,aes(x=pre,y=post))+geom_point()

with(datos,cor(pre,post))

#5

ggplot(datos,aes(x=pre,y=post,colour=tratamiento))+geom_point()

ggplot(datos,aes(x=pre,y=post,colour=sexo))+geom_point()



#ANOVA


#1

t.test(post~tratamiento,data=datos)

#EL p-valor aporta evidencia para decir que son distintos
#El IC no contiene el cero para la resta de Mu_ctr y Mu_esp

#2

mod1 <- lm(post~tratamiento,data=datos)

summary(mod1)

#3

mod2<-lm(post~tratamiento+sexo,data=datos)

summary(mod2)


#4

anova(mod1,mod2) #No es significativo y el p-valor es el mismo que de la variable sexo en el mod2

Anova(mod2)


#Hay diferencia entre el tratamiento por sexo?
datos  %>%
  group_by(sexo,tratamiento) %>%
  summarise(media=mean(post), desvio= sd(post),
            n_trat=n())

mod3 <- lm(post~tratamiento*sexo,data=datos)

summary(mod3)

Anova(mod3)

#ANCOVA

#1

mod1b <- lm(post~tratamiento+pre,data=datos)

summary(mod1b)

Anova(mod1b)

mod2b<-lm(post~tratamiento+sexo+pre,data=datos)

summary(mod2b)

Anova(mod2b)

mod3b <- lm(post~tratamiento*sexo+pre,data=datos)

summary(mod3b)

Anova(mod3b)

#El sexo no aporta al analisis del tratamiento
