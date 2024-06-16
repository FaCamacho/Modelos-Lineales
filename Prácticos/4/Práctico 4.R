
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





##################################
# segunda alternativa (tau_j = 0) #
##################################


# Para cambiar el grupo de referencia, es necesario que la variable sea un factor
datos$marca <- factor(datos$marca)


contrasts(datos$marca) <- contr.sum(4)

mod2 <- lm(km_miles ~ marca, contrasts = list(marca = 'contr.sum'),data = datos)
summary(mod2)
anova(mod2)

















