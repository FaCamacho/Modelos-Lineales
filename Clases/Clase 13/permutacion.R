# prueba t basada en permutaciones

data(sleep)

# extra son las horas de sueño extra con uno de los dos
# medicamentos indicados en la columna "group"

sleep
xb1 <- mean(sleep$extra[sleep$group==1])
xb2 <- mean(sleep$extra[sleep$group==2])

# este es el valor del estadistico
d <- xb1 - xb2

# permutamos B veces
B <- 1000
d_i <- rep(NA, B)

for(i in 1:B){
  sleep$g_i <- sample(sleep$group)
  xb1_i <- mean(sleep$extra[sleep$g_i==1])
  xb2_i <- mean(sleep$extra[sleep$g_i==2])
  d_i[i]<- xb1_i - xb2_i
}

hist(d_i)
abline(v=d, lwd=2, col=2)

# y aproximamos el p-valor
mean(abs(d_i)>=abs(d))

#entonces
