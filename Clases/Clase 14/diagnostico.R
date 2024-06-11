install.packages("car")
install.packages("skedastic")
install.packages("qqplotr")
install.packages("tseries")

library(car)


library(ggplot2)


library(skedastic)


library(qqplotr)


library(tseries)



library(readxl)
datos <- read_excel("diagnostico.xlsx")

names(datos)[2:3] <- c("gatos","perros")

mod <- lm(score ~gatos + perros +ratones+pajaros+insectos, data = datos)


#Esta es la correlacion de las x para con las otras x, queremos que sea bajo para evitar multicolinealidad
#Lo ideal es que sea menor a 5

vif(mod)

#Hay multicolinealidad

#Conseguimos los residuoes ajustados studentizados EXTERNAMENTE

datos$t_i <- rstudent(mod)

#Conseguimos los Y_hat

datos$pred <- fitted(mod)



#############################
#Control de homosedasticidad#
#############################

#Ploteamos los errores contra los predichos para tantear si hay homosedasticidad,
#la idea es que esté todo en una misma banda


ggplot(datos, aes(x = pred, y = t_i)) + 
  geom_point() +
  xlab('Predichos') +
  ylab('Residuos') +
  geom_abline(slope = 0, intercept = 0)


# Test de hipotesis
# H0) E(eps^2_i) = sigma^2 - 
# H1) E(eps^2_i) = sigma^2 * h(X_1, X_2, ..., X_k)

#En H0 afirmamos la homosedasticidad


breusch_pagan(mod)

#Re bajo el p-valor, rechazamos H0

##########################
#Para analizar normalidad#
##########################

# PRUEBA DE HIPOTESIS
# H0) Los errores son normales
# H1) Los errores NO son normales


# para construir el Q-Q plot y ver la distribución 

n <- nrow(datos)

z_i <- qnorm(seq(n)/(n + 1))

qq <- data.frame(teoricos = z_i,empiricos = sort(datos$t_i))

ggplot(qq, aes(x = teoricos, y = empiricos)) +
  geom_point() +
  xlab('Cuantiles teoricos') +
  ylab('Cuantiles empiricos') +
  geom_abline(slope = 1, intercept = 0, col = 2, size = 1.5)

#Los residuos empiricos se alejan bastante de los residuos normalizados


shapiro.test(datos$t_i)
tseries::jarque.bera.test(datos$t_i)


#Rechazo H0



#######################
#Control de linealidad#
#######################

#Acá la idea es graficar cada variable y ver su comportamiento

crPlot(mod,"gatos")
crPlot(mod,"perros")
crPlot(mod,"ratones")
crPlot(mod,"pajaros")
crPlot(mod,"insectos")


######################
#Análisis de atipicos#
######################

h_i <- influence(mod)$hat
D_i <- cooks.distance(mod)
df <- data.frame(i = 1:nrow(datos),
                 h_i = h_i,
                 D_i = D_i)


# leverage
ggplot(df, aes(x = i, y = h_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = h_i)) +
  xlab('') +
  ylab(expression(h[i])) +
  geom_abline(slope = 0, intercept = 2*5/50, col = 2, linetype = 'dashed')

# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')


#Al graficar según la distancia de Cook vemos los valores más atípicos
#En este caso el más atípico es el 47

################################
#Arreglamos la muticolinealidad#
################################

#El viff había dado re alto

vif(mod)

#Podemos asumir que la variable "ratones" es combinación lineal de las otras, entonces la sacamos

datos1 <- datos[,-4]

#Hacemos otra vez el modelo pero sin los ratones

mod1 <- lm(score ~gatos + perros +pajaros+insectos, data = datos1)



vif(mod1)

#El viff dio por debajo de 5 y descartamos que las X sean combinación lineal

############################
##Ahora arreglamos el resto#
############################


#volvemos a cosneguir los residuoes ajustados  studentizados EXTERNAMENTE 
#pero con nuestra base sin los ratones

datos1$t_i <- rstudent(mod1)

#volvemos a cosneguir los Y_hat
#pero con nuestra base sin los ratones


datos1$pred <- fitted(mod1)




#Graficamos la homosedasticidad y vemos que quitando "ratones" no se arregló

ggplot(datos1, aes(x = pred, y = t_i)) + 
  geom_point() +
  xlab('Predichos') +
  ylab('Residuos') +
  geom_abline(slope = 0, intercept = 0)


#Intervenir quitando la observación atipica num47

#datos1$I47 <- 0
#datos1$I47[47]<-1

datos1 <- datos1[-47,]


#Hacemos otro modelo para testear homosedasticidad
#Ahora ya no hay ratones y agregamos la intervención a la observación 47

mod1 <- lm(score ~gatos + perros +pajaros+insectos, data = datos1)

datos1$t_i <- rstudent(mod1)

datos1$pred <- fitted(mod1)

breusch_pagan(mod1)



#Todavía no corregimos la homosedasticidad

h_i <- influence(mod1)$hat
D_i <- cooks.distance(mod1)
df <- data.frame(i = 1:nrow(datos1),
                 h_i = h_i,
                 D_i = D_i)


# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')



#Repetimos la iteración quitando atípicos
#Ese es uno de los caminos a seguir y es el que hicimos en clase pero podríamos hacer otros perfectamente



#Intervenir quitando la observación atipica num97

datos2 <- datos1[-97,]

mod2 <- lm(score ~gatos + perros +pajaros+insectos, data = datos2)

datos2$t_i <- rstudent(mod2)

datos2$pred <- fitted(mod2)


breusch_pagan(mod2)

#Aún no corregimos homosedasticidad

h_i <- influence(mod2)$hat
D_i <- cooks.distance(mod2)
df <- data.frame(i = 1:nrow(datos2),
                 h_i = h_i,
                 D_i = D_i)


# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')


#Intervenir quitando la observación atipica num13

datos3 <- datos2[-13,]

mod3 <- lm(score ~gatos + perros +pajaros+insectos, data = datos3)

datos3$t_i <- rstudent(mod3)

datos3$pred <- fitted(mod3)

breusch_pagan(mod3)

#Aún no corregimos homosedasticidad pero estamos cerca

h_i <- influence(mod3)$hat
D_i <- cooks.distance(mod3)
df <- data.frame(i = 1:nrow(datos3),
                 h_i = h_i,
                 D_i = D_i)


# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')


#Intervenir quitando la observación atipica num45

datos4 <- datos3[-45,]

mod4 <- lm(score ~gatos + perros +pajaros+insectos, data = datos4)

datos4$t_i <- rstudent(mod4)

datos4$pred <- fitted(mod4)

breusch_pagan(mod4)




#Ya no rechazamos H0 ->

# Test de hipotesis
# H0) E(eps^2_i) = sigma^2 - 
# H1) E(eps^2_i) = sigma^2 * h(X_1, X_2, ..., X_k)

#Pero en el gráfico vemos que los errores son heterosedasticos

ggplot(datos4, aes(x = pred, y = t_i)) + 
  geom_point() +
  xlab('Predichos') +
  ylab('Residuos') +
  geom_abline(slope = 0, intercept = 0)


h_i <- influence(mod4)$hat
D_i <- cooks.distance(mod4)
df <- data.frame(i = 1:nrow(datos4),
                 h_i = h_i,
                 D_i = D_i)
t_i

# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')


#Todavía hay atípicos pero probemos como quedó la multicolinealidad

vif(mod4)

#Sigue baja, ahora la normalidad


n <- nrow(datos4)

z_i <- qnorm(seq(n)/(n + 1))

qq <- data.frame(teoricos = z_i,empiricos = sort(datos4$t_i))

ggplot(qq, aes(x = teoricos, y = empiricos)) +
  geom_point() +
  xlab('Cuantiles teoricos') +
  ylab('Cuantiles empiricos') +
  geom_abline(slope = 1, intercept = 0, col = 2, size = 1.5)


shapiro.test(datos4$t_i)
tseries::jarque.bera.test(datos4$t_i)

#Rechazamos HO) "los errores son normales"


#Ya que estamos probemos sacar otro, como para ver que pasa....

#Quitamos el 26

datos5 <- datos4[-26,]

mod5 <- lm(score ~gatos + perros +pajaros+insectos, data = datos5)

datos5$t_i <- rstudent(mod5)

datos5$pred <- fitted(mod5)

plot(mod5)

breusch_pagan(mod5)

h_i <- influence(mod5)$hat
D_i <- cooks.distance(mod5)
df <- data.frame(i = 1:nrow(datos5),
                 h_i = h_i,
                 D_i = D_i)


# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')




n <- nrow(datos5)

z_i <- qnorm(seq(n)/(n + 1))

qq <- data.frame(teoricos = z_i,empiricos = sort(datos5$t_i))

ggplot(qq, aes(x = teoricos, y = empiricos)) +
  geom_point() +
  xlab('Cuantiles teoricos') +
  ylab('Cuantiles empiricos') +
  geom_abline(slope = 1, intercept = 0, col = 2, size = 1.5)


shapiro.test(datos5$t_i)
tseries::jarque.bera.test(datos5$t_i)




#Quitamos el 62


datos6 <- datos5[-62,]

mod6 <- lm(score ~gatos + perros +pajaros+insectos, data = datos6)

datos6$t_i <- rstudent(mod6)

datos6$pred <- fitted(mod6)


breusch_pagan(mod6)

h_i <- influence(mod6)$hat
D_i <- cooks.distance(mod6)
df <- data.frame(i = 1:nrow(datos6),
                 h_i = h_i,
                 D_i = D_i)


# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')

#Chequeamos la normalidad


n <- nrow(datos6)

z_i <- qnorm(seq(n)/(n + 1))

qq <- data.frame(teoricos = z_i,empiricos = sort(datos6$t_i))

ggplot(qq, aes(x = teoricos, y = empiricos)) +
  geom_point() +
  xlab('Cuantiles teoricos') +
  ylab('Cuantiles empiricos') +
  geom_abline(slope = 1, intercept = 0, col = 2, size = 1.5)


shapiro.test(datos6$t_i)
tseries::jarque.bera.test(datos6$t_i)

ggplot(qq, aes(x = empiricos, y=..density..)) +
  geom_histogram(breaks = seq(-3, 3, 1), col = 'white') +
  xlab('Residuos studentizados')

# PRUEBA DE HIPOTESIS
# H0) Los errores son normales
# H1) Los errores NO son normales

#No rechazo H0)

#Chequeamos la linealidad

crPlot(mod6,"gatos")
crPlot(mod6,"perros")
crPlot(mod6,"pajaros")
crPlot(mod6,"insectos")


#Casi está arreglada la linealidad y la homosedasticidad todavía está fallando

#Quitamos el 5


datos7 <- datos6[-5,]

mod7 <- lm(score ~gatos + perros +pajaros+insectos, data = datos7)

datos7$t_i <- rstudent(mod7)

datos7$pred <- fitted(mod7)


breusch_pagan(mod7)

h_i <- influence(mod7)$hat
D_i <- cooks.distance(mod7)
df <- data.frame(i = 1:nrow(datos7),
                 h_i = h_i,
                 D_i = D_i)


# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')



# Quitamos el 38



datos8 <- datos7[-which.max(D_i),]

mod8 <- lm(score ~gatos + perros +pajaros+insectos, data = datos8)

datos8$t_i <- rstudent(mod8)

datos8$pred <- fitted(mod8)


breusch_pagan(mod8)

h_i <- influence(mod8)$hat
D_i <- cooks.distance(mod8)
df <- data.frame(i = 1:nrow(datos8),
                 h_i = h_i,
                 D_i = D_i)


# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')




#Quitamos el 92


datos9 <- datos8[-which.max(D_i),]

mod9 <- lm(score ~gatos + perros +pajaros+insectos, data = datos9)

datos9$t_i <- rstudent(mod9)

datos9$pred <- fitted(mod9)


breusch_pagan(mod9)

# Test de hipotesis
# H0) E(eps^2_i) = sigma^2 - 
# H1) E(eps^2_i) = sigma^2 * h(X_1, X_2, ..., X_k)

#No rechazo H0)

h_i <- influence(mod9)$hat
D_i <- cooks.distance(mod9)
df <- data.frame(i = 1:nrow(datos9),
                 h_i = h_i,
                 D_i = D_i)


# Distancia de Cook
ggplot(df, aes(x = i, y = D_i)) +
  geom_point() +
  geom_segment(aes(x = i, xend = i, y = 0, yend = D_i)) +
  xlab('') +
  ylab(expression(D[i])) +
  geom_abline(slope = 0, intercept = 4/50, col = 2, linetype = 'dashed')
which.max(D_i)



#Ahora a chequear todo

# -> Multicolinealidad

vif(mod9)

#Normalidad

# PRUEBA DE HIPOTESIS
# H0) Los errores son normales
# H1) Los errores NO son normales


# para construir el Q-Q plot y ver la distribución 

n <- nrow(datos9)

z_i <- qnorm(seq(n)/(n + 1))

qq <- data.frame(teoricos = z_i,empiricos = sort(datos9$t_i))

ggplot(qq, aes(x = teoricos, y = empiricos)) +
  geom_point() +
  xlab('Cuantiles teoricos') +
  ylab('Cuantiles empiricos') +
  geom_abline(slope = 1, intercept = 0, col = 2, size = 1.5)

shapiro.test(datos$t_i)
tseries::jarque.bera.test(datos$t_i)

ggplot(qq, aes(x = empiricos, y=..density..)) +
  geom_histogram(breaks = seq(-3, 3, 1), col = 'white') +
  xlab('Residuos studentizados')


#Rechazo H0

#No podemos asumir el supuesto de normalidad 