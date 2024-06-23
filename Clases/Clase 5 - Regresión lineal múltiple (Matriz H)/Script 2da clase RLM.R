# ejemplo de las ventas y los medios de publicidad (otra vez)

library(datarium)   # cargamos la libreria que contiene los datos
data(marketing)     # cargamos los datos

# extraemos la variable de respuesta (Y) la matriz (X)

f <- sales ~ youtube + facebook + newspaper
Y <- marketing$sales
X <- model.matrix(f, marketing)
n <- nrow(X)

# matriz de proyeccion H
H <- X %*% solve(t(X) %*% X) %*% t(X)
dim(H)

# ¿que pasa si proyectamos las columna de X usando H?
H %*% X[,1]
H %*% X[,2] # comparar com marketing$youtube
H %*% X[,3] # comparar com marketing$facebook
H %*% X[,4] # comparar com marketing$newspaper

# valores propios (y de paso averiguamos el rango de X)
round(eigen(H)$values,4)

# sin obtener el vector de estimaciones, obtengamos predichos y residuos
y_hat <- H%*%Y
eps_hat <- (diag(n) - H)%*%Y

# grafico de dispersion entre Y y los predichos
library(ggplot2)
obs_pred <- data.frame(ventas = Y, predichos = y_hat)
ggplot(obs_pred, aes(x = ventas, y = predichos)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color="red", size = 2) +
  ylab('ventas predichas') + 
  xlab('ventas')

# grafico de dispersion entre residuos y los predichos
hats <- data.frame(residuos = eps_hat, predichos = y_hat)
ggplot(hats, aes(x = predichos, y = residuos)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 0, color="red", size = 2) +
  ylab('ventas predichas') + 
  ylab('residuos')

# a partir del objeto 'lm' 
mod <- lm(f, data = marketing)
y_hat <- fitted(mod)
eps_hat <- residuals(mod)


# estimamos sigma2
k <- 3
s2 <- t(Y) %*% (diag(n) - H) %*% Y / (n - k - 1)

# el desvio standard seria
sqrt(s2)
summary(mod)

# calculemos el R2
u <- matrix(1, n, 1)
H1 <- u %*% solve(t(u)%*% u) %*% t(u)

SCExp <- t(Y) %*% (H - H1) %*% Y
SCTot <- t(Y) %*% (diag(n) - H1) %*% Y
R2 <- SCExp / SCTot
