# Linealidad
library(mlbench)

# cargamos los datos el precio de las casa en Boston
data('BostonHousing')
str(BostonHousing)

# la idea es explicar el precio mediano de las casas (medv) en cada 
# tracto censal en funcion de:
#   i)  porcentaje de poblacion de clase baja (lstat)
#   ii) numero promedio de habitaciones (rm)

# Estimamos el modelo

mod <- lm(medv ~ lstat + rm, data = BostonHousing)
summary(mod)

# residuos vs preedichos
res <- residuals(mod)
pred <- fitted(mod)
library(ggplot2)
ggplot(data.frame(res, pred), aes(x = pred, y = res)) +
  geom_point() +
  xlab(expression(hat(y))) +
  ylab(expression(hat(epsilon))) +
  geom_abline(intercept = 0, slope = 0, linetype = 'dashed', col = 2) +
  theme_bw() +
  geom_smooth(se = FALSE)

# grafico de residuos parciales
library(car)
crPlot(mod, variable = 'lstat')

# puede que valga la pena incluir un termino cuadratico
mod2 <- update(mod, . ~ . + I(lstat^2) + I(rm^2))
summary(mod2)

res <- residuals(mod2)
pred <- fitted(mod2)
ggplot(data.frame(res, pred), aes(x = pred, y = res)) +
  geom_point() +
  xlab(expression(hat(y))) +
  ylab(expression(hat(epsilon))) +
  geom_abline(intercept = 0, slope = 0, linetype = 'dashed', col = 2) +
  theme_bw() +
  geom_smooth(se = FALSE)