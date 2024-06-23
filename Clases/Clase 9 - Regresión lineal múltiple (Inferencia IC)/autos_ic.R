#hoy hacemos intervalos de confianza con los datos de los autos del '74

library(ggplot2)

data(mtcars)
help(mtcars)

# cambiemos unidades
datos <- mtcars[,-which(colnames(mtcars) %in% c('mpg','wt'))]

datos$kpl <- mtcars$mpg*0.425144
datos$peso <- mtcars$wt*0.4536


# un modelo de RLM para explicar/predecir el rendimiento

mod <- lm(kpl ~ peso + qsec, data = datos)

n <- nrow(mtcars)
k <- 2
s2 <- sum(residuals(mod)^2)/(n - k - 1)
X <- model.matrix(kpl ~ peso + qsec, data = datos)
xx1 <- solve(t(X) %*% X)

beta_hat <- coef(mod)
alfa <- 0.05
valor_tabla <- qt(1 - alfa/2, n - k - 1)

# IC95% para 'peso'
lambda <- c(0, 1, 0)
LB <- lambda %*% beta_hat
s_peso <- sqrt(s2*t(lambda) %*% xx1 %*% lambda)

c(LB - valor_tabla*s_peso , LB + valor_tabla*s_peso)

# IC95% para 'qsec'
lambda <- c(0, 0, 1)
LB <- lambda %*% beta_hat
s_qsec <- sqrt(s2*t(lambda) %*% xx1 %*% lambda)

c(LB - valor_tabla*s_qsec , LB + valor_tabla*s_qsec)

confint(mod, level = 1 - alfa)


###################
# RESPUESTA MEDIA #
###################

x0 <- c(1, 2.000, 20)

y0_hat <- x0 %*% beta_hat
s_RM <- sqrt(s2*t(x0)%*%xx1%*%x0)

alfa <- 0.05
valor_tabla <- qt(1 - alfa/2, n - k - 1)

c(y0_hat - valor_tabla*s_RM , y0_hat + valor_tabla*s_RM)

predict(mod, 
        newdata = data.frame(peso = 2, qsec = 20), 
        interval = 'confidence')

###########################
# INTERVALO DE PREDICCION #
###########################

x0 <- c(1, 2.000, 20)

y0_hat <- x0 %*% beta_hat
s_FO <- sqrt(s2*(1 + t(x0)%*%xx1%*%x0))

alfa <- 0.05
valor_tabla <- qt(1 - alfa/2, n - k - 1)

c(y0_hat - valor_tabla*s_FO , y0_hat + valor_tabla*s_FO)

predict(mod, 
        newdata = data.frame(peso = 2, qsec = 20), 
        interval = 'prediction')


#################################################
# INTERVALOS BASADOS EN BOOTSTRAP (PARAMETRICO) #
#################################################

x0 <- c(1, 2.000, 20)
y_hat <- fitted(mod)
res   <- residuals(mod)

# numero de 'remuestras' bootstrap
B <- 1000

# creamos matrices donde almacenar las 'replicas'
est <- matrix(NA, B, k + 1)
colnames(est) <- names(beta_hat)
y0  <- matrix(NA, B, 2)
colnames(y0) <- c('RM','FO')

for (i in 1:B){
  datos_i <- datos
  datos_i$kpl <- y_hat + sample(res, replace = TRUE)
  
  mod_i <- update(mod, data = datos_i)
  
  est[i, ] <- coef(mod_i)
  y0[i, 1] <- x0 %*% est[i, ]
  y0[i, 2] <- x0 %*% est[i, ] + sample(res, 1)
}

# calculamos los percentiles alfa/2 y 1 - alfa/2 para cada conjunto de replicas
alfa <- 0.05

# IC95%
apply(est, 2, quantile, probs = c(alfa/2, 1- alfa/2))

# IC95% RM y FO
apply(y0, 2, quantile, probs = c(alfa/2, 1- alfa/2))


####################################################
# INTERVALOS BASADOS EN BOOTSTRAP (NO PARAMETRICO) #
####################################################

x0 <- c(1, 2.000, 20)
n <- nrow(X)

# numero de 'remuestras' bootstrap
B <- 10000

# creamos matrices donde almacenar las 'replicas'
est <- matrix(NA, B, k + 1)
colnames(est) <- names(beta_hat)
y0  <- matrix(NA, B, 2)
colnames(y0) <- c('RM','FO')

for (i in 1:B){
  s <- sample(1:n, n, replace = TRUE)
  datos_i <- datos[s,]
  
  mod_i <- update(mod, data = datos_i)
  
  est[i, ] <- coef(mod_i)
  y0[i, 1] <- x0 %*% est[i, ]
  y0[i, 2] <- x0 %*% est[i, ] + sample(res, 1)
}

# calculamos los percentiles alfa/2 y 1 - alfa/2 para cada conjunto de replicas
alfa <- 0.05

# IC95%
apply(est, 2, quantile, probs = c(alfa/2, 1- alfa/2))

# IC95% RM y FO
apply(y0, 2, quantile, probs = c(alfa/2, 1- alfa/2))

# distribucion en el muestreo de una FO con x0 = c(1, 2.000, 20)
ggplot(as.data.frame(y0), aes(x=FO)) + geom_histogram() + theme_bw()