# Diagnostico - Normalidad

# Cargamos los datos del peso del ahorro
library(faraway)
data(savings)

# Veamos la ayuda
help(savings)

# Nos vamos a saltear la etapa descriptiva y pasaremos directamente al modelo lineal

mod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
summary(mod)

# obtengamos los residuos
savings$t_i <- rstudent(mod)
n <- nrow(savings)

# para construir el Q-Q plot
z_i <- qnorm(seq(n)/(n + 1))
qq <- data.frame(teoricos = z_i,
                 empiricos = sort(savings$t_i))
library(ggplot2)
ggplot(qq, aes(x = teoricos, y = empiricos)) +
  geom_point() +
  xlab('Cuantiles teoricos') +
  ylab('Cuantiles empiricos') +
  geom_abline(slope = 1, intercept = 0, col = 2, size = 1.5)

# alternativamente
library(qqplotr)
ggplot(data = qq, aes(sample = empiricos)) +
  stat_qq_band(fill = 2) +
  stat_qq_line(col = 2) +
  stat_qq_point() +
  xlab("Cuantiles teoricos")+
  ylab("Cuantiles empiricos")


# histogramas
hist(rstudent(mod))

# Y modificando los 'bins'
hist(rstudent(mod), breaks = 10)
hist(rstudent(mod), breaks = seq(-3.5,3.5,1))

# alternativamente
ggplot(qq, aes(x = empiricos, y=..density..)) +
  geom_histogram(bins = 6, col = 'white') +
  xlab('Residuos studentizados')

ggplot(qq, aes(x = empiricos, y=..density..)) +
  geom_histogram(breaks = seq(-3, 3, 1), col = 'white') +
  xlab('Residuos studentizados')


# PRUEBA DE HIPOTESIS
# H0) Los errores son normales
# H1) Los errores NO son normales

shapiro.test(savings$t_i)
tseries::jarque.bera.test(savings$t_i)
ks.test(savings$t_i, 'pnorm')

# 'Contaminemos' los datos con alguna observacion atipica
savings$sr[21]
savings$sr[21] <- savings$sr[21] + 13
mod21 <- update(mod, data = savings)

shapiro.test(rstudent(mod21))

ggplot(data.frame(stud = rstudent(mod21)),
       aes(x = stud, y=..density..)) +
  geom_histogram(col = 'white', bins = 7) +
  xlab('Residuos studentizados')
