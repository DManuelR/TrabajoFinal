## 1 es blade, 0 es flake

library(plyr)
BaseDatosFinal$`Hecho sobre:` <- mapvalues(BaseDatosFinal$`Hecho sobre:`, from = c("blade", "flake"), to = c(1,0))

## Creamos un vector sin la columna Hecho sobre

BDatos_Final_2 <- data.frame(BaseDatosFinal$Anchura, BaseDatosFinal$Grosor, BaseDatosFinal$Longitud, BaseDatosFinal$Peso)
BDatos_Final_2

## Las variables más asociadas son la longitud con el peso, los tres asteriscos indican que es más significativa la correlación.

library(correlation)
resultados <- correlation(BDatos_Final_2)
resultados
