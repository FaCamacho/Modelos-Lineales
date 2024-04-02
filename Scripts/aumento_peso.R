# ejemplo del aumento de peso segun edad y nivel de actividad fisica

library(readxl)
datos <- read_excel("betas_parciales.xlsx")
str(datos)
summary(datos)

# las variables son:
#  wt_gain_gr: el aumento de peso el ultimo anio (en gramos)
#  age: edad (en anios enteros)
#  AF_index: nivel de actividad fisica (valor entre 0 y 10, 0 sedentario, 10 muy activo)


# queremos indagar sobre la relacion entre el aumento de peso y la actividad fisica

library(ggplot2)
ggplot(datos, aes(x = AF_index, y = wt_gain_gr)) +
  geom_point(size=2) + theme_bw() +
  xlab('Indice de Actividad Fisica') +
  ylab('Aumento de peso (gr)')

# Al estimar los coeficientes del modelo
mod1 <- lm(wt_gain_gr ~ AF_index, data = datos)
coef(mod1)

# Es probable que al aumentar la edad, las personas tengan
# cierta 'facilidad' a aumentar de peso.

ggplot(datos, aes(x = age, y = wt_gain_gr)) +
  geom_point(size=2) + theme_bw() +
  xlab('Edad') +
  ylab('Aumento de peso (gr)')

# Ajustemos el analisis segun la edad

mod2 <- update(mod1, .~. + age)
# esto es lo mismo que:
# mod2 <- lm(wt_gain_gr ~ AF_index + age, data = datos)

coef(mod2)

# la explicacion radica en:
with(datos, cor(age, AF_index))
ggplot(datos, aes(x = age, y = AF_index)) +
  geom_point(size=2) + theme_bw() +
  xlab('Edad') +
  ylab('Indice de Actividad Fisica')