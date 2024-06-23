# Regresion Logistica

# cargamos los datos del diagnostico de enfermedad coronaria (CHD)
library(readxl)
datos <- read_excel("chd.xlsx")

# Nos fijamos que los datos se hayan cargado bien
str(datos)

# puede ser una buena costumbre, especificar a Y como factor,
# aclarando cual es si nivel de referencia

datos$CHD_f <- factor(datos$CHD, 
                      levels = c(0, 1),
                      labels = c('sano', 'enfermo'))


# Realizamos una inspeccion inicial de la presencia de enfermedad coronaria...
library(ggplot2)
ggplot(datos, aes(x = CHD_f, group = CHD_f)) +
  geom_bar() +
  xlab('Diagnostico') + 
  ylab('frecuencia') +
  theme_bw()

# ... y su posible dependencia de le edad de los pacientes
ggplot(datos, aes(x = age, y = CHD)) +
  geom_point() +
  xlab('Edad') + 
  ylab('P(CHD=1)') +
  theme_bw() -> p1
p1

# Ajustemos el modelo de regresion logistica
mod1 <- glm(CHD ~ age, data = datos, family = binomial(link = 'logit'))
summary(mod1)

# la interpretacion de b1
betas <- coef(mod1)
exp(betas[2])
#La 'CHANCE' de un diagnostico de CHD aumenta 12% x cada anio


# Agregamos la curva pi(edad) al grafico
xp <- 20:70
b0 <- betas[1]
b1 <- betas[2]
curva <- data.frame(age = xp, CHD = exp(b0 + b1*xp)/(1 +  exp(b0 + b1*xp)))

p1 + 
  geom_line(data = curva, col = 'red', size = 2)

# que habria pasado si hubiesemos ajustado el modelo de RLS

mod2 <- lm(CHD ~ age, data = datos)
b0 <- coef(mod2)[1]
b1 <- coef(mod2)[2]
recta <- data.frame(age = xp, CHD = b0 + b1*xp)

# grafiquemos la curva Y la recta
p1 + 
  geom_line(data = curva, col = 'red', size = 2) +
  geom_line(data = recta, col = 'blue', size = 2)

# si bien el ajuste no es "tan" malo entre 30 y 60, el modelo arroja
# probabilidades negativas e incluso mayores a 1 para ciertas edades.