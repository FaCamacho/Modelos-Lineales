library(readxl)
datos <- read_excel("diagnostico.xlsx")

names(datos)[2:3] <- c("gatos","perros")

mod <- lm(score ~gatos + perros +ratones+pajaros+insectos, data = datos)



summary(mod)

install.packages("car")
library(car)

#Esta es la correlacion de las x para con las otras x, queremos que sea bajo

vif(mod)


#Sacamos los residuoes ajustados  studentizados EXTERNAMENTE

res <- rstudent(mod)

datos$t_i <- res

#sacamos los Y_hat

Y_hat <- fitted(mod)

plot(x=res,y=Y_hat)

##############################
#Control de homosedasticidad#
##############################



# Test de hipotesis
# H0) E(eps^2_i) = sigma^2 - 
# H1) E(eps^2_i) = sigma^2 * h(X_1, X_2, ..., X_k)

library(ggplot2)
datos$pred <- fitted(mod)

ggplot(datos, aes(x = pred, y = t_i)) + 
  geom_point() +
  xlab('Predichos') +
  ylab('Residuos') +
  geom_abline(slope = 0, intercept = 0)

#No hay varianza constante, rechazo homosedasticidad

install.packages("skedastic")
library(skedastic)

breusch_pagan(mod)

##############################
#Para analizar normalidad#####
##############################

# PRUEBA DE HIPOTESIS
# H0) Los errores son normales
# H1) Los errores NO son normales

install.packages("qqplotr")
library(qqplotr)
install.packages("tseries")
library(tseries)




# para construir el Q-Q plot
n <- nrow(datos)
z_i <- qnorm(seq(n)/(n + 1))
qq <- data.frame(teoricos = z_i,
                 empiricos = sort(datos$t_i))

ggplot(qq, aes(x = teoricos, y = empiricos)) +
  geom_point() +
  xlab('Cuantiles teoricos') +
  ylab('Cuantiles empiricos') +
  geom_abline(slope = 1, intercept = 0, col = 2, size = 1.5)



shapiro.test(datos$t_i)
tseries::jarque.bera.test(datos$t_i)
ks.test(datos$t_i, 'pnorm')



##############################
#Graficos parciales para linealidad
##############################

crPlot(mod,"gatos")
crPlot(mod,"perros")
crPlot(mod,"ratones")
crPlot(mod,"pajaros")
crPlot(mod,"insectos")


##############################
#Para atipicos influyentes###
##############################

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

################################
#Arreglamos la muticolinealidad#
################################

datos1 <- datos[,-4]

mod1 <- lm(score ~gatos + perros +pajaros+insectos, data = datos1)

vif(mod1)



#######################################
##AHora arreglamos la homosedasticidad#
#######################################




#Sacamos los residuoes ajustados  studentizados EXTERNAMENTE

res1 <- rstudent(mod1)

datos1$t_i <- res1

#sacamos los Y_hat

datos1$pred <- fitted(mod1)

Y_hat1 <- fitted(mod1)

plot(x=res,y=Y_hat)

ggplot(datos1, aes(x = pred, y = t_i)) + 
  geom_point() +
  xlab('Predichos') +
  ylab('Residuos') +
  geom_abline(slope = 0, intercept = 0)


#Intervenir quitando la atipica 47

datos1$I47 <- 0
datos1$I47[47]<-1

mod2 <- lm(score ~gatos + perros +pajaros+insectos+I47, data = datos1)

breusch_pagan(mod2)





breusch_pagan(mod)




