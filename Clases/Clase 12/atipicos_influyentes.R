# Diagnostico - Datos influyentes

# Continuemos con los datos de la tasa de ahorro
library(faraway)
data(savings)

mod <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

# medidas de influencia
h_i <- influence(mod)$hat
D_i <- cooks.distance(mod)
df <- data.frame(i = 1:nrow(savings),
                 h_i = h_i,
                 D_i = D_i)

library(ggplot2)

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

# Los candidatos a ser etiquetados como observaciones influyentes son
# las observaciones 23 y 49

# Observemos como cambia el vector de parametros
influ <- influence(mod)$coefficients[c(23,49),]
betas <- rbind(coef(mod), influ)

round(betas, 5)

# al quitar ambas observaciones
mod2 <- update(mod, data = savings[-c(23,49),])
round(rbind(coefficients(mod),coefficients(mod2)),5)
