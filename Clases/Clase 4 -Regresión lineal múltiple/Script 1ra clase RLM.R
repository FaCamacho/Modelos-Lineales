# ejemplo de las ventas y los medios de publicidad

library(datarium)   # cargamos la libreria que contiene los datos
data(marketing)     # cargamos los datos

head(marketing)
dim(marketing)

# queremos estimar los coeficientes del modelo que relaciona las ventas
# con el presupuesto de cada medio de publicidad

mod <- lm(sales ~ facebook + youtube + newspaper, data = marketing)

# extraemos los coeficientes con la funcion 'coef'

coef(mod)

# notemos como estos coeficientes son diferentes a los obtenidos luego de
# realizar 3 modelos de RLS.

rls1 <- lm(sales ~ facebook, data = marketing)
rls2 <- lm(sales ~ youtube, data = marketing)
rls3 <- lm(sales ~ newspaper, data = marketing)

coef(rls1)
coef(rls2)
coef(rls3)

#Graficas
with(marketing,plot(youtube, sales,pch=16))
with(marketing,plot(facebook, sales,pch=16))
with(marketing,plot(newspaper, sales,pch=16))



# mas adelante veremos que esto se debe a que los coeficientes del modelo RLM
# son 'parciales' y solo en un caso muy especial se da que los coeficientes del
# modelo RLM coindicen con los de 'k' regresiones lineales simples.