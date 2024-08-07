# efecto de omitir una variable relevante

# numero de observaciones
n <- 100

# numero de simulaciones
N <- 1000

# matriz donde almacenar las estimaciones de cada simulacion
betas <- matrix(NA, N, 2)

# parametros
sigma2 <- 1
b0 <- 2
b1 <- -1
b2 <- 1

# parametros para prediccion
x1p <- 2
x2p <- 0.5
y_pred <- b0 + b1*x1p + b2*x2p

# vector para almacenar la prediccion de cada simulacion
pred <- rep(NA, N)

for (i in 1:N){
  # se 'inventan' variables explicativas (no ortogonales)
  x1 <- rnorm(n, 5, 1)
  x2 <- -x1 + rnorm(n, 2, 2)
  
  # se construye la Y
  epsilon <- rnorm(n, 0, sqrt(sigma2))
  Y <- b0 + b1*x1 + b2*x2 + epsilon
  
  # se estima el modelo:
  # Y = b0* + b1*x1 + eps*
  b_hat <- coef(lm(Y~x1))
  betas[i,] <- b_hat
  
  # prediccion
  pred[i] <- b_hat[1] + b_hat[2]*x1p
}

# estimamos el sesgo
apply(betas, 2, mean) - c(b0, b1)

# estimamos el sesgo de la prediccion
mean(pred) - y_pred
# y de su varianza
var(pred)



# si hubieramos incluido a x2 en el modelo

for (i in 1:N){
  # se 'inventan' variables explicativas (no ortogonales)
  x1 <- rnorm(n, 5, 1)
  x2 <- -x1 + rnorm(n, 2, 2)
  
  # se construye la Y
  Y <- b0 + b1*x1 + b2*x2 + rnorm(n, 0, sqrt(sigma2))
  
  # se estima el modelo:
  # Y = b0* + b1*x1 + eps*
  b_hat <- coef(lm(Y~x1+x2))
  betas[i,] <- b_hat[1:2]
  
  # prediccion
  pred[i] <- b_hat[1] + b_hat[2]*x1p+ b_hat[3]*x2p
}

# estimamos el sesgo de las estimaciones
apply(betas,2,mean) - c(b0, b1)

# estimamos el sesgo de la prediccion
mean(pred) - y_pred
# y de su varianza
var(pred)