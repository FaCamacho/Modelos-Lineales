library(readxl)
library(tidyverse)

condados <- read_excel("Laboratorio/condados.xlsx",na="NA")

class(condados)

condados<-as.data.frame(condados)

class(condados)

table(condados$estado)

cuenta <- condados %>% group_by(.,estado) %>% count(.) 


Florida <- condados %>% filter(.,estado == "Florida")

str(Florida$esp_vida)

#as.numeric(Florida$esp_vida)

ggplot(data=Florida)+geom_histogram(mapping = aes(x=esp_vida))

ggplot(data=Florida)+geom_boxplot(mapping = aes(x=esp_vida))





