# pruebas de aleatorizacion (permutaciones)

# simulemos datos
n <- 27

set.seed(12345)

x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
X  <- cbind(x1,x2,x3)

# les doy 'un poco' de correlacion
L <- matrix(c(1,-0.2,0.4,-0.2,1,0.7,0.4,0.7,1),3,3)
X <- cbind(1, X%*%chol(L))

# genero Y SIN efecto de x2 en la media
y <- X%*%c(-3,0.7,0,1.2)+ runif(n,-1,1)

# notese que los errores NO son normales

datos <- data.frame(y=y, x1=X[,2], x2=X[,3], x3=X[,4])
mod   <- lm(y~., data = datos)
summary(mod)

# aleatoricemos 'P' veces
P <- 1000

# funcionas auxiliares
h <- function(y, x, X){
  # Esta funcion auxiliar 'remueve' el efecto de las columnas
  # de X en x y en Y
  if (class(X) != 'matrix') X <- as.matrix(X)
  H <- X%*%solve(t(X)%*%X)%*%t(X)
  y1 <- y - y%*%H
  x1 <- x - x%*%H
  return(list(ye    = as.numeric(y1),
              equis = as.numeric(x1)))
}
estima <- function(h){
  # Esta funcion auxiliar estima el parametro de la pendiente
  # en una RLS que no incluye constante.
  # Usa como insumo el resultado de la funcion h()
  sum(h$ye*h$equis)/sum(h$equis^2)
}
desvio <- function(h){
  # Esta funcion auxiliar estima el desvio del estimador de la pendiente
  # en un modelo de RLS que no incluye la constante.
  # Usa como insumo el resultado de la funcion h()
  b <- sum(h$ye*h$equis)/sum(h$equis^2)
  r <- h$ye - b*h$equis
  n <- length(h$ye)
  sqrt((sum(r^2)/(n-1))/sum(h$equis^2))
}

est <- matrix(NA, P, 3)

for (i in 1:P){
  # se quita el efecto de las demas variables...
  h1 <- h(datos$y, datos$x1, cbind(1,datos[,c('x2','x3')]))
  h2 <- h(datos$y, datos$x2, cbind(1,datos[,c('x1','x3')]))
  h3 <- h(datos$y, datos$x3, cbind(1,datos[,c('x1','x2')]))
  
  # se lleva a cabo la permutacion aleatoria...
  h1$ye <- sample(h1$ye)
  h2$ye <- sample(h2$ye)
  h3$ye <- sample(h3$ye)
  
  # estima...
  b1 <- estima(h1)
  b2 <- estima(h2)
  b3 <- estima(h3)
  
  # se obtiene el desvio del estimador
  sb1 <- desvio(h1)
  sb2 <- desvio(h2)
  sb3 <- desvio(h3)
  
  # y se calcula el estadistico 't'
  t1 <- b1/sb1
  t2 <- b1/sb2
  t3 <- b1/sb3
  
  # los almacenamos
  est[i,] <- c(t1, t2, t3)
}

# p-valor x1
hist(est[,1])
t_1 <- coef(summary(mod))[2,3]
abline(v = t_1, col = 2, lwd = 2)
pv1 <- mean(est[,1]>abs(t_1)|est[,1]< -abs(t_1))

# p-valor x2
hist(est[,2])
t_2 <- coef(summary(mod))[3,3]
abline(v = t_2, col = 2, lwd = 2)
pv2 <- mean(est[,2]>abs(t_2)|est[,2]< -abs(t_2))


# p-valor x3
hist(est[,3])
t_3 <- coef(summary(mod))[4,3]
abline(v = t_3, col = 2, lwd = 2)
pv3 <- mean(est[,3]>abs(t_3)|est[,3]< -abs(t_3))

summary(mod)
c(pv1,pv2,pv3)


# Una funcion que haga todo el trabajo
randomize <- function(mod, P = 1000){
  # Esta funcion aproxima los p-valores de las pruebas de significacion
  # individual mediante 'P' permutaciones aleatorias 
  t_stat <- coef(summary(mod))[,3]
  k   <- length(t_stat)
  est <- matrix(NA, P, k)
  mm  <- mod$model
  Y   <- mm[,1]
  X   <- cbind(1,mm[,-1])
  
  for (i in 1:P){
    tes <- rep(NA, k)
    for (j in 1:k){
      # se quita el efecto de las demas variables
      h1 <- h(Y, X[,j], X[,-j])
      # se permuta
      h1$ye <- sample(h1$ye)
      # se estima el parametro
      b1 <- estima(h1)
      # se estima el desvio
      sb1 <- desvio(h1)
      # se almacena el estadistic 't'
      tes[j] <- b1/sb1 
    }
    est[i,] <- tes
  }
  # se calculan los p-values bilaterales
  t_stat <- matrix(rep(abs(t_stat), P),nrow = P, byrow = TRUE)
  pv <- apply(est > t_stat, 2, mean)
  pv <- pv + apply(est < -t_stat,2,mean)
  
  smod <- coef(summary(mod))
  smod <- cbind(smod, pv)
  colnames(smod)[5] <- 'Pr(>|t|)_rand'
  return(smod)
}

round(randomize(mod),4)

