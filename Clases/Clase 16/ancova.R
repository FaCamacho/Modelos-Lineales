# ANCOVA

# cargamos los datos del peso de las notas
library(readxl)
datos <- read_excel("notas.xlsx")
datos <- notas
# Nos fijamos que los datos se hayan cargado bien
str(datos)

# Pasamos a 'factor' la variable 'grupo'
datos$grupo <- as.factor(datos$grupo)

# Realizamos una inspeccion inicial de la nota final en funcion del grupo
library(ggplot2)
ggplot(datos, aes(x = grupo, y = nota_f, fill = grupo)) +
  geom_boxplot() +
  xlab('Grupo') + 
  ylab('Calificacion final') +
  theme_bw()

# ?Puede ser que las notas del grupo C sean mejores?

# Hagamos un diagrama de dispersion para analizar los supuestos del ANCOVA
# respecto de la covariable

ggplot(datos, aes(x = nota_i, y = nota_f)) +
  geom_point() +
  xlab('Calificacion previa') + 
  ylab('Calificacion final') +
  theme_bw() 

with(datos,cor(nota_i,nota_f))

ggplot(datos, aes(x = nota_i, y = nota_f, col = grupo)) +
  geom_point() +
  xlab('Calificacion previa') + 
  ylab('Calificacion final') +
  theme_bw() +
  geom_smooth(method = 'lm', se =FALSE)

# El supuesto de linealidad parece razonable
# El supueseto de igualdad de pendientes ...

# Ajustemos en primer lugar el modelo ANOVA a 1 via
mod1 <- lm(nota_f ~ grupo, data = datos)
# significacion del modelo = 
# significacion del factor ('grupo') =
# significacion de la prueba de igualdad de medias
anova(mod1)

# no parece haber diferencias entre las calificaciones finales de los grupos

# Incluyamos el efecto de la covariable
mod2 <- lm(nota_f ~ grupo + nota_i, data = datos)
# significacion de covariable y factor (ajustando por la presencia de c/u)
library(car)
anova(mod2) 

mod2 <- lm(nota_f ~ nota_i + grupo, data = datos)

anova(mod2) #dependiendo de cual sea el orden de las variables explicativas da distinto p-valor


Anova(mod2) #En este caso tengo el efecto de cada termino sin importar el orden


# Se logra reconocer la diferencia entre los promedios
# de las calificaciones finales porque:
#  a) se aumento la potencia del estadistico F, debido a que
#  b) se logro disminuir la variabilidad del termino de error

summary(mod1)$sigma #desvó de los errores
summary(mod2)$sigma

# Observese ademas que se obtiene una 'mejor' descripcion de la variabilidad

summary(mod1)$r.squared
summary(mod2)$r.squared

# Finalmente, queremos llevar a cabo las comparaciones multiples para
# determinar que grupos logran mejores o peores calificaciones

library(emmeans)
rf <- ref_grid(mod2, at = list(nota_i = c(40, 60, 80)), cov.keep = 'nota_i')
cm <- emmeans(rf, ~grupo|nota_i)
pairs(cm)

library(multcomp)
cld(cm, Letters = letters)

# los promedios SI dependen de la calificacion previa
# las diferencias entre ellos NO


# y si hubiesemos hecho esto otro...
cm2 <- emmeans(mod2, ~grupo)
cld(cm2, Letters = letters)

# para que valor de la nota inicial se esta haciendo la comparacion?

# Supuestos del ancova

# 1) linealidad entre Y y X
summary(mod2)
crPlot(mod2, variable = 'nota_i', pch = 16)

# 2) igualdad de pendientes
mod3 <- lm(nota_f~ nota_i + grupo + nota_i:grupo, data = datos)
anova(mod2, mod3)

# 3) la covariable NO afecta la media de cada grupo
anova(lm(nota_i ~ grupo, data = datos))