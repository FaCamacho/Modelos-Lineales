# Diagnostico - Homoscedasticidad

# Cargamos los datos del peso de los ninos
load('peso_ninos.RData')

# inspeccion inicial de los datos
summary(datos)
with(datos, cor(edad_meses, peso))
aggregate(peso ~ sexo, data = datos, FUN = mean)

# construimos un modelo
mod <- lm(peso ~ edad_meses + sexo, data = datos)

# extraemos los residuos
datos$r_i <- residuals(mod)  # residuos
datos$s_i <- rstandard(mod)  # studendizados INTERNAMENTE
datos$t_i <- rstudent(mod)   # studentizados EXTERNAMENTE

# PRIMER PASO PARA EXPLORAR HETEROSCEDASTICIDAD (predichos vs residuos)
library(ggplot2)
datos$pred <- fitted(mod)

ggplot(datos, aes(x = pred, y = t_i)) + 
  geom_point() +
  xlab('Predichos') +
  ylab('Residuos') +
  geom_abline(slope = 0, intercept = 0)

# SEGUNDO PASO, residuos vs x_j
ggplot(datos, aes(x = edad_meses, y = t_i)) + 
  geom_point() +
  xlab('Edad (meses)') +
  ylab('Residuos') +
  geom_abline(slope = 0, intercept = 0)

ggplot(datos, aes(x = sexo, y = t_i)) + 
  geom_boxplot() +
  xlab('Sexo') +
  ylab('Residuos')

# TERCER PASO. Test de hipotesis
# H0) E(eps^2_i) = sigma^2
# H1) E(eps^2_i) = sigma^2 * h(X_1, X_2, ..., X_k)

# ya disponemos de los residuos del modelo

# los elevamos al cuadrado
datos$r2_i <- datos$r_i^2

# ajustamos la regresion auxiliar
mod_aux <- lm(r2_i ~ edad_meses + sexo, data = datos)

# y calculamos el valor del estadistico BP
n <- nrow(mod_aux$model)
R2 <- summary(mod_aux)$r.squared
BP <- n*R2

# el p-valor es:
1 - pchisq(BP, length(coef(mod_aux)) - 1)


# esta prueba existe en varios paquetes
library(skedastic)
breusch_pagan(mod)
library(lmtest)
bptest(mod)

# dentro de estas librerias existen otras alternativas para realizar pruebas de homoscedasticidad