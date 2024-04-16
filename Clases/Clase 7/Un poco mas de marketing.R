# ejemplo de las ventas y los medios de publicidad (otra vez mas)

library(datarium)   # cargamos la libreria que contiene los datos
data(marketing)     # cargamos los datos

# extraemos la variable de respuesta (Y) la matriz (X)

f <- sales ~ youtube + facebook + newspaper

mod <- lm(f, marketing)

# queremos evaluar la significacion del modelo
# esto equivale a poner a prueba la siguiente hipotesis
# H0) beta_youtube = beta_facebook = beta_newspaper = 0
# H1) al menos un beta es distinto de 0


# numerador
C <- cbind(0,diag(3))
b <- rep(0,3)
q <- 3
betas <- coef(mod)
X <- model.matrix(f, marketing)
xx1 <- solve(t(X) %*% X)
cxx1c1 <- solve(C %*% xx1 %*% t(C))
CB_b <- C %*% betas - b

num <- t(CB_b) %*% cxx1c1 %*% CB_b/q

# denominador
res <- residuals(mod)
n <- nrow(X)
k <- ncol(X) - 1
den <- sum(res^2)/(n - k - 1)

# estadistico F
eFe <- num/den

# valor critico
alfa <- 0.05
qf(1-alfa, q, n - k -1)

# p-valor
1 - pf(eFe, q, n - k -1)

# que podemos obtener mas facilmente con
summary(mod)


# en casos como este, cuando se rechaza H0, el siguiente paso es
# realizar 'k' pruebas del estilo
#  H0) beta_j = 0
#  H1) beta_j != 0
# Llamadas pruebas de significacion de cada variable