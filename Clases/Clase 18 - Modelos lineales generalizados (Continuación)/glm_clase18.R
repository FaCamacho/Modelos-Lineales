# GLM logistico 
#  estimacion
#  precision
#  prediccion
#  bondad de ajuste

# cargamos los datos del bajo peso al nacer
library(MASS)
data(birthwt)

# Nos interesa determinar los factores asociados a que el peso de los
# ninos al nacer sea menor a 2.5kg (variable 'low')
# Para eso nos valemos de las variables
#  age (Edad de la madre)
#  lwt (Peso de la madre)
#  race (Raza de la madre)
#  smoke (Si fumo durante el embarazo)
#  ht (Si tiene historial de hipertension)
#  ui (Si tiene irritabilidad uterina)
#  ftv (Visitas al ginecologo durante el 1er trimestre)

# Codificamos algunas variables
birthwt$low   <- factor(birthwt$low, labels = c('normal','bajo peso'))
birthwt$race  <- factor(birthwt$race, labels = c('blanca','afro','otra'))
birthwt$smoke <- factor(birthwt$smoke, labels = c('no','si'))
birthwt$ht    <- factor(birthwt$ht, labels = c('no','si'))
birthwt$ui    <- factor(birthwt$ui, labels = c('no','si'))


# Una descripcion univariada de nuestra Y
table(birthwt$low)

# Y de forma bivariada con las X
library(ggplot2)
ggplot(birthwt, aes(x = race, fill = low)) +
  geom_bar(position = 'fill') +
  scale_y_continuous(labels=scales::percent) +
  xlab('Raza') + ylab('')

ggplot(birthwt, aes(x = ht, fill = low)) +
  geom_bar(position = 'fill') +
  scale_y_continuous(labels=scales::percent) +
  xlab('Hipertension') + ylab('')

ggplot(birthwt, aes(x = smoke, fill = low)) +
  geom_bar(position = 'fill') +
  scale_y_continuous(labels=scales::percent) +
  xlab('Fumo durante el embarazo') + ylab('')

ggplot(birthwt, aes(x = ui, fill = low)) +
  geom_bar(position = 'fill') +
  scale_y_continuous(labels=scales::percent) +
  xlab('Irritabilidad uterina') + ylab('')


# Ajustamos el modelo de regrsion logistica
efe <- low ~ age + lwt + race + smoke + ht + ui + ftv
mod <- glm(efe,
           data = birthwt,
           family = binomial())

# pseudoR2

pseudoR2 <- function(mod){
  logL1 <- logLik(mod)
  logL0 <- logLik(update(mod, .~1))
  L1    <- exp(logL1)
  L0    <- exp(logL0)
  n     <- length(fitted(mod))
  dat   <- data.frame(p = predict(mod, type = 'response'), y = mod$y)
  
  # McFadden
  R2_MF <- 1 - (logL1/logL0)
  # Cox & Snell
  R2_CS <- 1 - (L0/L1)^{2/n}
  # Nagelkerke
  R2_Ngk <- (1 - (L0/L1)^{2/n})/(1 - L0^{2/n})
  # Tjur
  R2_tjur <- diff(sort(aggregate(p~y, data = dat, FUN = mean)$p))
  r2 <- c('MCFadden' = R2_MF,
          'Cox % Snell' = R2_CS,
          'Nagelkerke' = R2_Ngk,
          'Tjur' = R2_tjur)
  return(r2)
}
pseudoR2(mod)

# estimacion
X       <- model.matrix(efe, birthwt)
beta    <- rep(0, ncol(X))
maxiter <- 10
g_inv   <- function(x) exp(x)/(1 + exp(x))
Y       <- as.numeric(birthwt$low)-1

B <- matrix(NA, nrow = maxiter, ncol = length(beta))
colnames(B) <- colnames(X)
rownames(B) <- paste('iter', 1:maxiter, sep = '_')

for(k in 1:maxiter){
  B[k, ] <- beta
  # predictor lineal
  eta <- X%*%beta
  # E(Y|X)
  mu  <- g_inv(eta)
  # Funcion de varianza V(Y|X)
  W   <- diag(as.numeric(mu*(1 - mu)))
  # inversa del anterior
  W_inv <- solve(W)
  # working response
  z     <- eta + W_inv%*%(Y - mu)
  # actualizacion de beta
  beta  <- solve(t(X)%*%W%*%X) %*% t(X)%*%W%*%z
}

# comparar con:
coef(mod)

# Var(estimaciones)
eta <- X%*%beta
mu  <- g_inv(eta)
W   <- diag(as.numeric(mu*(1 - mu)))
Vbeta <- solve(t(X)%*%W%*%X)

# los devios standard son:
sqrt(diag(Vbeta))
# comparar con
summary(mod)$coefficients[,2]


# prediccion
p_eta <- predict(mod)
p_mu  <- predict(mod, type = 'response')

# como mu = g^{-1}(u) = e^u/(1+e^u)
identical(p_mu, g_inv(p_eta))

# Asignemos 'etiquetas'
umb <- 0.3
birthwt$pred <- ifelse(p_mu > umb, 'bajo peso', 'normal')

# comparamos predicciones con observaciones
with(birthwt, table(low, pred))

acierto <- (87 + 42)/189
aciertoN <- 87/(87 + 43)
aciertoBP <- 42/(42 + 17)

# valores predictivos (positivo y negativo)
# estos valores apuntan a responder las siguientes preguntas:
#  ¿si el modelo dice "bajo peso" que probabilidad hay de que el bebe tenga bajo peso?
#  ¿si el modelo dice "normal" que probabilidad hay de que el bebe tenga peso normal?

VPN <- 87/(87 + 17)
VPP <- 42/(42 + 43)

