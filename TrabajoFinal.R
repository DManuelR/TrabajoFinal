## Regresión logistica simple para empezar, y luego vemos si añadimos más variables (multiple), o binaria, analisis de conglomerados?
## Renombramos, 1 es blade (lamina), 0 es flake (lasca)

library(plyr)
BaseDatosFinal$Tipo <- mapvalues(BaseDatosFinal$Tipo, from = c("blade", "flake"), to = c(1,0))

## Creo graficos de distribución según variables

library(ggplot2)

ggplot(BaseDatosFinal, aes(x = Longitud, y = Anchura, color = Tipo)) + geom_point()
ggplot(BaseDatosFinal, aes(x = Grosor, y = Peso, color = Tipo)) + geom_point()

# Creamos la variable Tipo_fac para tener la variable Tipo como factorial

BaseDatosFinal$Tipo_fac <- as.factor(BaseDatosFinal$Tipo)
BaseDatosFinal$Tipo_fac

# Realizamos modelos logísticos simples

model <- glm(Tipo_fac ~ Anchura, data = BaseDatosFinal, family = binomial())
summary(model)

model2 <- glm(Tipo_fac ~ Longitud, data = BaseDatosFinal, family = binomial())
summary(model2)

model3 <- glm(Tipo_fac ~ Grosor, data = BaseDatosFinal, family = binomial())
summary(model3)

model4 <- glm(Tipo_fac ~ Peso, data = BaseDatosFinal, family = binomial())
summary(model4)

# Con la columna Pr(>|z|) podemos ver que ninguno de los modelos simples da significativo.Podemos deducir que no existe una relación directa entre las variables y el tipo de instrumento.

# Modelos logísticos multiples.

model_1 <- glm(Tipo_fac ~ Anchura + Longitud, data = BaseDatosFinal, family = binomial())
summary(model_1)
model_2 <- glm(Tipo_fac ~ Grosor + Peso, data = BaseDatosFinal, family = binomial())
summary(model_2)
model_3 <- glm(Tipo_fac ~ Anchura + Longitud + Grosor, data = BaseDatosFinal, family = binomial())
summary(model_3)
model_4 <- glm(Tipo_fac ~ Anchura + Longitud + Grosor + Peso, data = BaseDatosFinal, family = binomial())
summary(model_4)
model_5 <- glm(Tipo_fac ~ Longitud + Grosor + Peso, data = BaseDatosFinal, family = binomial())
summary(model_5)
model_6 <- glm(Tipo_fac ~ Longitud + Grosor, data = BaseDatosFinal, family = binomial())
summary(model_6)
model_7 <- glm(Tipo_fac ~ Longitud + Peso, data = BaseDatosFinal, family = binomial())
summary(model_7)


# Ninguna de las combinaciones probadas parecen dar resultados significativos, algunas en las que está presente la longitud y no la anchura parece que dan un poco de relación significativa pero seguramente sea por azar

# Creo la variable proporción longitud/anchura, que es la que tiene más sentido arqueológico en principio. 

BaseDatosFinal$Longitud_Anchura <- BaseDatosFinal$Longitud/BaseDatosFinal$Anchura

# Modelo simple

model5 <- glm(Tipo_fac ~ Longitud_Anchura, data = BaseDatosFinal, family = binomial())
summary(model5)

# Modelo multiple

model_8 <- glm(Tipo_fac ~ Longitud_Anchura + Peso, data = BaseDatosFinal, family = binomial())
summary(model_8)
model_9 <- glm(Tipo_fac ~ Longitud_Anchura + Grosor, data = BaseDatosFinal, family = binomial())
summary(model_9)

# Vemos que el mejor modelo es el simple (AIC más bajo y que el resto de variables no parece dar explicación del tipo de instrumento.)
#En ese si sale significativo aunque poco, seguramente por el tamaño de la muestra. Por lo que podemos decir que existe una relación entre la proporción longitud/anchura y el tipo de instrumento.

# Vemos la proporcion promedio de cada tipo.

mean(BaseDatosFinal$Longitud_Anchura [BaseDatosFinal$Tipo == "0"]) # La proporcion es 1 por lo que anchura y largo son iguales en los "flake".

mean(BaseDatosFinal$Longitud_Anchura [BaseDatosFinal$Tipo == "1"]) # La proporcion es 1,33 por lo que la anchura es dos tercios de la longitud en "blade".


