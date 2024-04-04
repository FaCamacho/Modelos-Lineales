library(gcookbook)
data(heightweight)

# creamos el vector Y y la matriz X
Y <- heightweight$weightLb*0.4535
X <- cbind(1, heightweight$heightIn*2.45)

# obtenemos el estimador MCO
b_mco <- solve(t(X) %*% X) %*% t(X) %*% Y

# Si en la matriz X optaramos por 'centrar' la altura:
heightweight$heightIn_c <- heightweight$heightIn - mean(heightweight$heightIn)
X_c <- cbind(1, heightweight$heightIn_c*2.45)

# volvemos a obtenemos el estimador MCO
b_mco2 <- solve(t(X_c) %*% X_c) %*% t(X_c) %*% Y

# cambia la estimacion de beta_0 pero la estimacion de beta_1 se mantiene inalterada

# estimacion de sigma2
rgX <- 2
n <- nrow(X_c)
scr <- t(Y - X_c%*%b_mco2) %*% (Y - X_c%*%b_mco2)
s2 <- scr/(n - rgX)

# matriz de varianzas
cov_b_mco <- as.numeric(s2) * solve(t(X_c) %*% X_c)

# realicemos una prueba de hipótesis que nos permita establecer si existe
# una relacion lineal entre la altura y el peso

# H0) beta_altura = 0
# H0) beta_altura != 0

# estadistico
T_obs <- (b_mco2[2] - 0)/sqrt(cov_b_mco[2,2])

# valor critico
alfa <- 0.05
T_crit <- qt(1 - alfa/2, n - rgX)

# p-valor
2*(1 - pt(T_obs, n - rgX))

# sumas de cuadrados
SCT <- sum((Y - mean(Y))^2)
y_hat <- X_c%*%b_mco2
SCRes <- t(Y - y_hat) %*% (Y - y_hat)
SCExp <- t(y_hat - rep(b_mco2[1], n)) %*% (y_hat - rep(b_mco2[1], n))

R2 <- SCExp/SCT
R2 <- 1 - SCRes/SCT
# el 60% de la variabilidad del peso esta explicada por la altura

# relacion entre R2 y r
cor(Y,X[,2])
cor(Y,X[,2])^2


# ahora hacemos lo mismo pero a traves de la funcion 'lm'
datos <- data.frame(peso = Y, altura = X[,2], altura_c = X_c[,2])
mod <- lm(peso ~ altura, data = datos)
summary(mod)

# si trabajamos con la altura centrada
datos <- data.frame(peso = Y, altura = X[,2], altura_c = X_c[,2])
mod_c <- lm(peso ~ altura_c, data = datos)
summary(mod_c)

