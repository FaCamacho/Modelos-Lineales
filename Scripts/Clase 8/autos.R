# hoy hacemos inferencia con los datos de los autos del '74

library(ggplot2)

data(mtcars)
help(mtcars)

# cambiemos unidades
datos <- mtcars[,-which(colnames(mtcars) %in% c('mpg','wt'))]

datos$kpl <- mtcars$mpg*0.425144  # km por x litro
datos$peso <- mtcars$wt*0.4536    # peso en toneladas

# estadistica descriptiva
summary(datos)

ggplot(datos, aes(y = kpl)) +
  geom_boxplot() +
  theme_bw() +
  ylab('Rendimiento (km por litro)') +
  scale_x_discrete(breaks = NULL)

# matriz de correlaciones
c1 <- cor(datos)
round(c1, 2)

# si queremos visualizar las correlaciones, una opcion es:
# install.packages('ggcorrplot')

library(ggcorrplot)
ggcorrplot(c1, method = 'circle', type = 'upper')


# un modelo de RLM para explicar/predecir el rendimiento

mod <- lm(kpl ~ peso + qsec + am + carb, data = datos)

###################################
# SIGNIFICACION GLOBAL DEL MODELO #
###################################

k <- 4
C <- cbind(0,diag(k))
b <- rep(0, k)
betas <- coef(mod)
X <- model.matrix(kpl ~ peso + qsec + am + carb, data = datos)
n <- nrow(X)
q <- k
s2 <- sum(residuals(mod)^2)/(n - k - 1)

# estadistico
eFe <- t(C%*%betas - b) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*% (C%*%betas - b) /(q*s2)

# p-valor
1 - pf(eFe, k, n - k - 1)

# El modelo en su conjunto SI es significativo
# Al menos 1 de las variables aporta a explicar el rendimiento


#############################################
# SIGNIFICACION INDIVIDUAL DE CADA VARIABLE #
#############################################

# Veamos la significacion de la variable 'peso'
C <- matrix(c(0,1,0,0,0), nrow = 1)
b <- 0
q <- 1

# usndo el estadistico F
eFe <- t(C%*%betas - b) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*% (C%*%betas - b) /(q*s2)
# p-valor
1 - pf(eFe, q, n - k - 1)

# usando el estaistico t
V <- vcov(mod)
te <- betas[2]/sqrt(V[2,2])
# p-valor
2*(1 - pt(abs(te), n - k - 1))


# para observarlos todos hay dos alternativas
summary(mod)  # presenta los estadisticos t

library(car)
Anova(mod)   # presenta los estadisticos F

# CUIDADO, 'Anova' y 'anova' producen resultados distintos
# Anova realiza pruebas F 'parciales'
# anova realiza pruebas F 'secuenciales'



################################################
# SIGNIFICACION DE UN SUBCONJUNTO DE VARIABLES #
################################################

# Veamos la significacion conjunta de 'am' y 'carb'

C <- rbind(c(0,0,0,1,0), c(0,0,0,0,1))
b <- c(0,0)
q <- 2

# usando el estadistico F
eFe <- t(C%*%betas - b) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*% (C%*%betas - b) /(q*s2)
# p-valor
1 - pf(eFe, q, n - k - 1)

# otra alternativa es emplear el enfoque 'modelo completo' y 'modelo reducido'
mod_comp <- mod
mod_red  <- update(mod, .~. - am - carb)

anova(mod_red, mod_comp)



###########################################
# SIGNIFICACION DE UNA COMBINACION LINEAL #
###########################################

ingresos <- read.table('ingresos.txt', header = TRUE)

str(ingresos)

# estadistica descriptiva
summary(ingresos)

ggplot(ingresos, aes(y = ingresos)) +
  geom_boxplot() +
  theme_bw() +
  ylab('Salario ($)') +
  scale_x_discrete(breaks = NULL)

# matriz de correlaciones
c1 <- cor(ingresos)
round(c1, 2)

mod <- lm(ingresos ~ educacion + experiencia, data =ingresos)
summary(mod)

# ponemos a prueba la hipotesis de que ambas variables tienen el mismo efecto sobre el salario
C <- matrix(c(0,1,-1),nrow=1)
b <- 0
betas <- coef(mod)
X <- model.matrix(ingresos ~ educacion + experiencia, data =ingresos)
n <- nrow(X)
k <- 2
q <- 1
s2 <- sum(residuals(mod)^2)/(n - k - 1)

# estadistico
eFe <- t(C%*%betas - b) %*% solve(C %*% solve(t(X) %*% X) %*% t(C)) %*% (C%*%betas - b) /(q*s2)

# p-valor
1 - pf(eFe, k, n - k - 1)

# en este caso el modelo podria reducirse a
mod_red <- lm(ingresos ~ I(educacion + experiencia), data=ingresos)
summary(mod_red)

# A traves del enfoque 'modelo completo' y 'modelo reducido'
anova(mod_red, mod)