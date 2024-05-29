# Diagnostico (1era clase)

# Simulemos un caso donde hay multicolinealidad exacta.

n     <- 100
sigma <- 2
x1    <- rnorm(n)
x2    <- rnorm(n)
x3    <- rnorm(n)
x4    <- x1 + x3
y     <- 4 + 0.5*x1 - 0.7*x2 + 1.1*x3 + 0.3*x4 + rnorm(n, 0, sigma)

datos <- data.frame(y,x1,x2,x3,x4)

# al ajustar el modelo con las 4 variables explicativas...
mod <- lm(y ~ x1 + x2 + x3 + x4, data = datos)
summary(mod)


# valores propios
# especifiquemos a x4 como 'casi' CL de las demas
x4 <- 0.7*x1 -0.5*x2 + rnorm(n, 0, 0.02)

# definamos la matriz X
X4 <- cbind(x1, x2, x3, x4)
datos4 <- data.frame(y,x1,x2,x3,x4)

# inspeccionemos la matriz de correlacion
cor(X4)

# Y obtengamos los valores propios de X'X
xx4 <- t(X4)%*%X4
lambda4 <- eigen(xx4)$values

# numero de condicion
kA <- sqrt(max(lambda4)/min(lambda4))

# determinante de X'X
det(xx4)

# determnante de X'X luego de centrar las columnas 
x4_c <- scale(X4)
xx4_c <- t(x4_c)%*%x4_c
det(xx4_c/(n-1))

# determinante de la matriz de correlacion
det(cor(X4))

# ¿y que tal si hubiese 2 variables explicativas que fuesen ''casi'' CL de las demas?
x5 <- 2*x3 -1.6*x2 + runif(n, -0.1, 0.1)
X5 <- cbind(x1, x2, x3, x4, x5)
xx5 <- t(X5)%*%X5

# Al analizar los valores propios

lambda5 <- eigen(xx5)$values
lambda5 

# y al calcular el numero de condicion para cada casi
kA <- sqrt(lambda5[1]/rev(lambda5[-1]))

# determinante de X'X
det(xx5)

# determnante de X'X luego de centrar las columnas 
x5_c <- scale(X5, scale = TRUE)
xx5_c <- t(x5_c)%*%x5_c
det(xx5_c/(n-1))

# que es lo mismo que el determinante de la matriz de correlacion
det(cor(X5))

# si quisieramos calcular los factores de inflacion de varianza
library(car)
mod <- lm(y~x1+x2+x3+x4, datos4)
vif(mod)


# En el caso del conjunto de datos de los autos
data(mtcars)
X <- as.matrix(mtcars[,-1])
k <- ncol(X)

lambda <- eigen(t(X)%*%X)$values
sqrt(lambda[1]/lambda[-1])

mod <- lm(mpg ~ ., data = mtcars)
vif(mod)

# library(mctest)
# ?omcdiag



# Linealidad
library(readxl)
datos <- read_excel("datos_carmona.xlsx")

library(ggplot2)
ggplot(datos, aes(x = Densidad , y = Velocidad)) +
  geom_point()

mod <- lm(Velocidad ~ Densidad, data = datos)
datos$predichos <- fitted(mod)
datos$residuos  <- residuals(mod)

# grafico de predichos vs residuos
ggplot(datos, aes(x = predichos , y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0)

# grafico de residuos parciales
library(car)
crPlot(mod, variable = 'Densidad')

# un ejemplo un poco más interesante de residuos parciales
data(Prestige)
help(Prestige)

# Intentemos explicar el 'prestigo' de las ocupaiones envase al salario y la educacion
mod <- lm(prestige ~ income + education, data = Prestige)

# el grafico de predichos vs residuos
Prestige$predichos <- fitted(mod)
Prestige$residuos  <- residuals(mod)
ggplot(Prestige, aes(x = predichos , y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0)

# pero los graficos de residuos parciales...
res <- residuals(mod)
b <- coef(mod)
r_inc <- res + b[2]*Prestige$income
r_edu <- res + b[3]*Prestige$education

graf <- data.frame(variable = c(Prestige$income, Prestige$education),
                   res_parc = c(r_inc, r_edu),
                   panel    = rep(c('Ingreso','Educacion'), each = nrow(Prestige)))

ggplot(graf, aes(x = variable, y = res_parc)) +
  geom_point() +
  facet_grid(~panel, scales = 'free') +
  geom_smooth(method = lm, se = FALSE) +
  geom_smooth(col = 'red', se = FALSE, linetype = 'dashed') +
  theme_bw()



# Usando la funcion 'crPlot' es un poco mas facil
crPlot(mod, variable = 'income', pch = 16)
crPlot(mod, variable = 'education', pch = 16)

# es posible pensar que el efecto del ingreso se ajuste mejor con una parabola
mod1 <- update(mod, .~. + I(income^2))
crPlot(mod1, variable = 'income', pch = 16)
summary(mod1)

# o incluso que el efecto del ingreso sea logaritmico
mod2 <- update(mod, .~. -income + log(income))
crPlot(mod2, variable = 'log(income)', pch = 16)
summary(mod2)