# ANOVA a 2 vias

# cargamos los datos de los tubos de luz
library(readr)
datos <- read_csv("tubos_de_luz.csv")

# Glass: es el tipo de material del tubo
# Temp : es la temperatura en el interior del tubo
# Light: es la cantidad de luz que emite el  tubo

# mejor que sean factores
datos$Glass <- factor(datos$Glass)
datos$Temp  <- factor(datos$Temp)

# Llevemos a cabo un analisis exploratorio inicial
library(ggplot2)
ggplot(datos, aes(x = Glass, y = Light, fill = Glass)) +
  geom_boxplot() +
  xlab('Material') + 
  ylab('Luz') +
  theme_bw() +
  theme(legend.position = 'bottom') -> p1
ggplot(datos, aes(x = Temp, y = Light, fill = Temp)) +
  geom_boxplot() +
  xlab('Temperatura') + 
  ylab('Luz') +
  theme_bw() +
  theme(legend.position = 'bottom')-> p2
library(cowplot)
plot_grid(p1, p2)

aggregate(Light ~ Temp + Glass, data = datos, FUN = mean)
aggregate(Light ~ Temp + Glass, data = datos, FUN = sd)

library(dplyr)
datos %>%
  group_by(Glass, Temp) %>%
  summarise(media = mean(Light),
            desvio = sd(Light),
            n = n())

# El modelo ANOVA a 2 vias se ajustara con 27 datos para estimar 5 parametros
# Permitira responder preguntas del estilo
#  ¿La cantidad de luz depende de la temperatura y/o el tipo de vidrio?
#  ¿Existe un efecto Temperatura?
#  ¿Existe un efecto Vidrio?

mod1 <- lm(Light ~ Glass + Temp, data = datos)
summary(mod1)

# Efectos principales
# utilizando el enfoque 'modelo completo - modelo reducido'
# significacion de Temp
mod1T <- update(mod1, .~. - Temp)
anova(mod1T, mod1)

# significacion de Glass
mod1G <- update(mod1, .~. - Glass)
anova(mod1G, mod1)

# o mucho mas sencillamente
library(car)
Anova(mod1)

# que en este tipo de modelos es lo mismo que
anova(mod1)

# para comparar Vidrios o Temperaturas
library(emmeans)
library(multcomp)
library(multcompView)

# comparamos niveles de vidrio
vidrio <- emmeans(mod1, ~Glass)
pairs(vidrio, adjust = 'Tukey')
cld(vidrio, Letters = letters)

# comparamos niveles de temperatura
temp <- emmeans(mod1, ~Temp)
pairs(temp, adjust = 'Tukey')
cld(temp, Letters = letters)



# Modelo a 2 vias CON interaccion

ggplot(datos, aes(x = Glass, y = Light, fill = Glass)) +
  geom_boxplot() +
  xlab('Temperatura') + 
  ylab('Luz') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  facet_grid(~Temp)

datos %>%
  group_by(Glass, Temp) %>% 
  summarise(media = mean(Light)) %>%
  ggplot(aes(x = Glass, y = media, color = Temp)) +
  geom_line(aes(group = Temp)) +
  geom_point() +
  theme_bw()

# como las 'rectas' no son paralelas, puede ser conveniente incluir la interaccion

# Al incluir la interaccion, el modelo permite responder algunas preguntas mas
#  ¿El efecto de la temperatura es distinto dependiendo del tipo de vidrio?
#  ¿El efecto del tipo de vidrio es distinto dependiendo de la temperatura?

# los siguientes son equivalentes
mod2 <- lm(Light ~ Glass + Temp + Glass:Temp, data = datos)
mod2 <- lm(Light ~ Glass * Temp, data = datos)
mod2 <- update(mod1, .~. + Glass:Temp)

# La interaccion es el primer efecto a testear
# a traves del enfoque 'modelo completo - modelo reducido'
anova(mod1, mod2)
# o simplemente
anova(mod2)

# Como la interaccion ES significativa, no es recomendable analizar los efectos principales
# pasamos a las comparaciones multiples

# si interesara analizar el efecto del vidrio en cada temperatura
cm1 <- emmeans(mod2, ~Glass|Temp)
pairs(cm1, adjust = 'Tukey')
cld(cm1, Letters = letters)

# o si interesara comparar temperaturas en cada vidrio
cm2 <- emmeans(mod2, ~Temp|Glass)
pairs(cm2, adjust = 'Tukey')
cld(cm2, Letters = letters)

# este analisis se corresponde a este grafico
datos %>%
  group_by(Glass, Temp) %>% 
  summarise(media = mean(Light)) %>%
  ggplot(aes(x = Temp, y = media, color = Glass)) +
  geom_line(aes(group = Glass)) +
  geom_point() +
  theme_bw()

# no olvidar la etapa de diagnostico
shapiro.test(rstudent(mod2))
library(lmtest)
bptest(mod2)