# Estimaciones con variables explicativas ortogonales

# En la practica rara vez sucede al emplear variables explicativas
# cuantitativas. No obstante, como se profundizara al estudiar
# modelos de analisis de varianza, al 'comparar grupos' es mas facil
# obtener situaciones donde las x_j sean ortogonales.

data(iris)
str(iris)

# queremos determinar el largo del petalo de la flor de iris
# difiere entre las especies.

library(ggplot2)
ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_boxplot() +
  xlab('Especie') +
  ylab('Largo del petalo (cm)') +
  theme_bw()+
  theme(legend.position = 'none')

# En un ambito de modelos lineales, nos interesa
# largo_petalo = b0 + b1*especie + epsilon

# Pero con el cuidado de que la variable 'Species' es CUALITATIVA.
# Hay varias maneras de introducirla al modelo

f <- Petal.Length ~ Species 
f_sc <- Petal.Length ~ Species - 1

# obtengamos la matriz de disenio de cada caso
X <- model.matrix(f, iris)
X_sc <- model.matrix(f_sc, iris)

# si las variables son ortogonales, X'X deberia ser diagonal
t(X) %*% X
t(X_sc) %*% X_sc

# en X, las columnas indican:
#  una especie de referencia (setosa)
#  la diferencia entre versicolor y setosa
#  la diferencia entre virginica y setosa

# mientras que en X_sc indican:
#  la especie setosa
#  la especie versicolor
#  la especie virginica

# al ajustar el modelo de RLM sobre X_sc se obtienen los coeficientes
mod <- lm(f_sc, data = iris)
coef(mod)

# que deberian coincidir con las estimaciones de cada submodelo por separado
x_set <- X_sc[,1] 
x_vers <- X_sc[,2]
x_virg <- X_sc[,3]

petalo <- iris$Petal.Length

sum(x_set*petalo)/sum(x_set^2)
sum(x_vers*petalo)/sum(x_vers^2)
sum(x_virg*petalo)/sum(x_virg^2)