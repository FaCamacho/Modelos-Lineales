
#Cargar datos
library(readxl)
base <- read_excel("biometria_peces.xlsx")

head(base)

str(base)

class(base)

summary(base)

#hacer Peso_gr como cúmero
Peso_gr<-as.numeric(base$Peso_gr)

#Quitar Peso_gr en caracteres
base <- base[,-2]

#Agregar Peso_gr numérico
base <- cbind(base,Peso_gr)

summary(base)

