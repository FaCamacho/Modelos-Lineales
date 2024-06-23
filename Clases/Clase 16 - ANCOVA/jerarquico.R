# Modelo lineal 'jeararquico'

# cargamos los datos del peso de los peces
library(readxl)
datos <- read_excel("precio.xlsx")
datos2 <- precio
# Nos fijamos que los datos se hayan cargado bien
str(datos)

# cambiamos la clase  de la variable 'zona'
datos2$zona <- factor(datos2$zona)

# indagamos sobre la relacion entre el precio y el area
library(ggplot2)
ggplot(datos2, aes(x = area, y = precio_miles)) + 
  geom_point() +
  theme_bw() +
  ylab('Precio (miles U$S)')

# Y al modelarlo obtenemos
mod0 <- lm(precio_miles ~ area, data = datos)
summary(mod0)

# indaguemos sobre la relacion entre precio y zona
ggplot(datos2, aes(x = zona, y = precio_miles, fill = zona)) + 
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = 'none') +
  ylab('Precio (miles U$S)')

# Parece existir un 'efecto' de las zonas
# Volvamos a realizar el diagrama de dispersion coloreando los puntos segun la zona
ggplot(datos2, aes(x = area, y = precio_miles, col = zona)) + 
  geom_point() +
  theme_bw() +
  ylab('Precio (miles U$S)')

# parecen coexistir 3 rectas, veamos como las ajusta ggplot
ggplot(datos2, aes(x = area, y = precio_miles, col = zona)) + 
  geom_point() +
  theme_bw() +
  ylab('Precio (miles U$S)') +
  geom_smooth(method = 'lm', se = FALSE)

# Parece 'sensato' pensar que estas rectas tienen:
#  1) diferente ordenada en el origen (beta_0)
#  2) diferente pendiente (beta_1)

# especificamos el modelo con diferente valor de beta_0 para cada zona
mod1 <- lm(precio_miles ~ zona + area, data = datos2)

# si lo comparamos con mod0 estamos realizando la prueba
#  H0) solo existe un valor de beta_0 comun a las 3 zonas
#  H1) al menos 1 zona tiene un valor de beta_0 diferente a las demas

anova(mod0, mod1)

# Descartando a mod0, y quedandonnos con mod1, realicemos la siguiente hipotesis
#  H0) la pendiente del 'area' es igual para las 3 zonas
#  H1) al menos 1 zona tiene un valor de la pendiente diferente a las demas

mod2 <- lm(precio_miles ~ zona + area + area:zona, data = datos2)
mod2 <- lm(precio_miles ~ zona*area, data = datos2)

# Para llevar a cabo la prueba anterior:
anova(mod1, mod2)

# De esta manera encontramos que existe evidencia de que el area incide sobre el
# precio de las casas, pero de diferente manera en cada zona residencial.

# Luego, podemos estimar las 3 pendientes y compararlas entre ellas
library(emmeans)
pendientes <- emtrends(mod2, pairwise ~ zona, var = 'area')
library(multcomp)
cld(pendientes, Letters = letters)
# El impacto de 1 m^2 extra es mayor en el centra y menor en la zona rural
