## Regresión logistica simple para empezar, y luego vemos si añadimos más variables (multiple), o binaria, analisis de conglomerados?
## 1 es blade, 0 es flake

library(plyr)
BaseDatosFinal$Tipo <- mapvalues(BaseDatosFinal$Tipo, from = c("blade", "flake"), to = c(1,0))

## Las variables más asociadas son la longitud con el peso, los tres asteriscos indican que es más significativa la correlación.

library(correlation)
resultados <- correlation(BaseDatosFinal)
resultados

## utilizar glm() en vez de lm(), porque lm es gaussian y con glm puede ser binomial. Primero pasar la variable binaria a factor

Tipo_1 <- as.factor(BaseDatosFinal$Tipo)
modelo <- glm(Tipo_1 ~ BaseDatosFinal$Anchura + BaseDatosFinal$Grosor, data = BaseDatosFinal, family = binomial(link = "logit"))
modelo
modelo_2 <- glm(Tipo_1 ~ BaseDatosFinal$Anchura + BaseDatosFinal$Peso, data = BaseDatosFinal, family = binomial(link = "logit"))
modelo_2

## Creamos un vector con la columna tipo y grosor

BDatos_Final_2 <- data.frame(BaseDatosFinal$Tipo, BaseDatosFinal$Grosor)
BDatos_Final_2

modelo_3 <- glm(Tipo_1 ~ BaseDatosFinal$Peso, data = BaseDatosFinal, family = binomial(link = "logit"))
modelo_3
