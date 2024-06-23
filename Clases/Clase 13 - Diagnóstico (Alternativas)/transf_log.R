# Transformacion logaritmica
datos <- read.csv("animales.csv")

# trabajaremos con datos sobre el peso del cerebro en mamiferos
head(datos)
str(datos)

# la idea es explicar el peso del cerebro (gr) en funcion de:
#	i) el peso del animal (kg)
#	ii) su periodo de gestacion (dias)
#	iii) la cantidad promedio de crias

# descriptiva univariada
summary(datos[, -1])

# matriz de correlaciones
c1<-cor(datos[, -1])
round(c1, 3)
pairs(datos[,- 1]) # pares de diagramas de dispersion


# Estimamos el modelo

m1 <- lm(Brain ~ Body + Gestation + Litter, data = datos)
summary(m1)

# El modelo es significativo al 5% y
# todas las variables menos 'Litter' tambien lo son

# quitamos 'Litter' 
m2 <- update(m1, . ~ . -Litter)
summary(m2)

# el modelo final (por ahora) seria m2

# Pasemos a la etapa de diagnostico

# multicolinealidad
library(car)
vif(m1)
vif(m2)
# no hay problemas de multicolinealidad
# 'Si' los hubiera, distintas alternativas serian:
#    1) remover la/s variable/s
#    2) usar metodos de 'regularizacion'
#    3) usar minimos cuadrados parciales (no los vemos en este curso)
#    4) usar componentes principales (se ven en el curso Analisis Multivariado)

# homoscedasticidad

# predichos vs residuos
y_hat <- fitted(m2)
e_hat <- residuals(m2)
datos$y_hat <- y_hat
datos$e_hat <- e_hat

library(ggplot2)
ggplot(datos, aes(x = y_hat, y = e_hat)) +
  geom_point() +
  xlab(expression(hat(y))) +
  ylab(expression(hat(epsilon))) +
  geom_abline(intercept = 0, slope = 0, linetype = 'dashed', col = 2) +
  theme_bw()

# residuos vs Body
ggplot(datos, aes(x = Body, y = e_hat)) +
  geom_point() +
  xlab('Peso (kg)') +
  ylab(expression(hat(epsilon))) +
  geom_abline(intercept = 0, slope = 0, linetype = 'dashed', col = 2) +
  theme_bw()

# residuos vs Gestation
ggplot(datos, aes(x = Gestation, y = e_hat)) +
  geom_point() +
  xlab('Período de gestación (días)') +
  ylab(expression(hat(epsilon))) +
  geom_abline(intercept = 0, slope = 0, linetype = 'dashed', col = 2) +
  theme_bw()

# residuos vs Litter
ggplot(datos, aes(x = Litter, y = e_hat)) +
  geom_point() +
  xlab('Número de crías') +
  ylab(expression(hat(epsilon))) +
  geom_abline(intercept = 0, slope = 0, linetype = 'dashed', col = 2) +
  theme_bw()

# ¿Hay problemas de heteroscedasticidad?

library(skedastic)

# test de Breusch-Pagan
breusch_pagan(m2)

# test de White
white_lm(m2, interactions = TRUE)

# Existen problemas de heteroscedasticidad

# Algunas opciones a probar son
#    1) transformar Y
#    2) transformar X
#    3) utilizar errores standar robustos
#       library(sandwich)
#       coeftest(m2, vcov = vcovHC(m2, "HC0"))
#    4) modelar la varianza
#    5) MCG (X'VX)^1.X'VY

# transformemos la variable de respuesta (log(x), sqrt(x), 1/x u otra...)
m3 <- update(m2, log(Brain) ~ .)

#? existe heteroscedasticidad?
breusch_pagan(m3)

# Probemos transformando tambien las variables explicarivas
m4 <- lm(Brain ~ log(Body) + log(Gestation) + log(Litter), data = datos)

#? existe heteroscedasticidad?
breusch_pagan(m4)

# se puede hacer inferencia sin problemas
summary(m4)

# Probemos transformando tanto Y como las X
m5 <- update(m4, log(Brain) ~ .)

#? existe heteroscedasticidad?
breusch_pagan(m5)

# Ahora si vale hace inferencia
summary(m5)
# ?Los coeficientes se pueden interpretar?

# Si observamos los graficos de dispersion de las variables
# expresadas en terminos logaritmicos, la solucion se hace 
# aun mas clara
pairs(log(datos[, c('Brain', 'Body', 'Gestation', 'Litter')]))


# normalidad

r_i <- residuals(m5)  # residuos 'comunes'
s_i <- rstudent(m5)   # residuos 'studentizados'
e_i <- rstandard(m5)  # residuos 'estandarizados'
summary(data.frame(r_i, s_i, e_i))

# continuemos el analisis con los studentizados
boxplot(s_i)
plot(density(s_i))
eje <- seq(-4, 4, 0.01)
lines(eje, dnorm(eje, mean = 0,sd = 1), lwd = 2, col = 'red')

# test de normalidad
shapiro.test(s_i)
tseries::jarque.bera.test(s_i)
ks.test(s_i, 'pnorm')
