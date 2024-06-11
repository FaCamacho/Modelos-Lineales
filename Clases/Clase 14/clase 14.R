# ANOVA a 1 via

# cargamos los datos de la vida util de los neumaticos
library(readxl)
datos <- read_excel("anova_1_via.xlsx")

datos <- as.data.frame(datos)
# En primer lugar corroboramos que los datos hayan sido cargados adecuadamente
str(datos)

# La variable que indica los grupos (tratamientos) puede ser un 'character' o un 'factor'

# En este tipo de analisis, suele resultar conveniente realizar box-plots para tener
# una primera aproximacion a los datos

library(ggplot2)
ggplot(datos, aes(x = marca, y = km_miles, fill = marca)) +
  geom_boxplot() +
  xlab('Marca') + 
  ylab('Kilometraje (miles)') +
  theme_bw()

# Parece que la vida util de los neumaticos presenta variaciones entre las marcas

# Llevamos a cabo la prueba de hipotesis
#    H0) mu_Apollo = mu_Bridgestone = mu_CEAT = mu_Falken
#    H1) al menos una difiere de las demas

# una forma de calcular los promedios (sin tidyverse)
aggregate(km_miles ~ marca, data = datos, FUN = mean)
aggregate(km_miles ~ marca, data = datos, FUN = sd)

# otra forma de calcular los promedios (con tidyverse)
library(dplyr)
datos %>%
  group_by(marca) %>%
  summarise(media = mean(km_miles),
            desvio = sd(km_miles))


#### CUando anulamos tau_j entonces Mu se interpreta como el promedio del grupo j
### las restantes tau son las difernecias del cada grupo respecto del promedio del grupo j


# Ahora llevemos a cabo la prueba, de manera formal

##################################
# Primer alternativa (tau_1 = 0) #
##################################

mod1 <- lm(km_miles ~ marca, data = datos)
summary(mod1)

model.matrix(mod1) #Como armó la matriz con la restricción

anova(mod1)

# el p-valor correspondiente a la prueba es 2.781x10-8

###################################
# Segunda alternativa (tau_3 = 0) #
###################################

# Para cambiar el grupo de referencia, es necesario que la variable sea un factor
datos$marca <- factor(datos$marca)

# y luego sí es posible realizar el cambio
datos$marca <- relevel(datos$marca, ref = 'Falken')

mod2 <- lm(km_miles ~ marca, data = datos)
summary(mod2)
anova(mod2)

# El estadistico F y su p-valor NO cambian frente a diferentes restricciones

#######################################
# Tercer alternativa (Reparametrizar) #
#######################################

mod3 <- lm(km_miles ~ marca - 1, data = datos)
summary(mod3)
anova(mod3)


# Debido a que estamos 'eliminando' la constante, para obtener el estadistico F
# es necesario comparar este modelo con el que NO tiene la variable que indica
# los grupos

mod0 <- lm(km_miles ~ 1, data = datos)
summary(mod0)
anova(mod0, mod1)


# mismo F y mismo p-valor

###########################
# COMPARACIONES MULTIPLES #
###########################

library(emmeans)

# comparaciones multiples
cm <- emmeans(mod1, ~ marca)

# Ajuste de Bonferroni
pairs(cm, adjust = 'bonferroni')
# Bonferroni's HSD
s2 <- sum(mod1$residuals^2)/56
qt(1 - 0.05/2/6, 56)*sqrt(s2*(1/15 + 1/15))

# el p.valor sin ajustar (usando la distribucion t)
2*(1 - pt(3.789, 56))
# el p-valor ajustado segun Bonferroni es 0.0022
2*(1 - pt(3.789, 56))*6

# Ajuste de Tukey
pairs(cm, adjust = 'tukey')
# Tukey's HSD
s2 <- sum(mod1$residuals^2)/56
qtukey(0.95, 4, 56)/sqrt(2)*sqrt(s2*(1/15 + 1/15))

# el p.valor sin ajustar (usando la distribucion t)
2*(1 - pt(3.789, 56))
# el p-valor ajustado segun Tukey es 0.0021
1 - ptukey(3.789*sqrt(2), 4, 56)

# Ajuste de Scheffe
pairs(cm, adjust = 'scheffe')
# Scheffe's HSD
s2 <- sum(mod1$residuals^2)/56
sqrt((4 - 1)*qf(1-0.05,3,56))*sqrt(s2*(1/15 + 1/15))

# el p.valor sin ajustar (usando la distribucion t)
2*(1 - pt(3.789, 56))
# el p-valor ajustado segun Scheffe es 0.0049
1 - pf((3.789^2)/3, 3, 56)



# letritas
library(multcomp)
cld(cm, Letters = letters)
cld(cm, Letters = letters, adjust = 'bonferroni')
cld(cm, Letters = letters, adjust = 'scheffe')

# Estos resultados son exactamente iguales si se usa mod1, mod2 o mod3

# No olvidemos ralizar la etapa de diagnostico
datos$s_i <- rstudent(mod1)
ggplot(datos, aes(x = marca, y = s_i, fill= marca)) +
  geom_boxplot() +
  xlab('Marca') + 
  ylab('residuos studentizados') +
  theme_bw()

# homoscedasticidad
library(skedastic)
breusch_pagan(mod1)

# normalidad
library(qqplotr)
ggplot(data = datos, aes(sample = s_i)) +
  stat_qq_band(fill = 2) +
  stat_qq_line(col = 2) +
  stat_qq_point() +
  xlab("Cuantiles teoricos")+
  ylab("Cuantiles empiricos")

shapiro.test(datos$s_i)


# prueba de Kruskal-Wallis
kruskal.test(km_miles ~ marca, data = datos)