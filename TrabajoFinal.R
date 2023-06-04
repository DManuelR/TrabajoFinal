
library(plyr)
BaseDatosFinal$Tipo <- mapvalues(BaseDatosFinal$Tipo, from = c("blade", "flake"), to = c(1,0))

## 

library(ggplot2)

ggplot(BaseDatosFinal, aes(x = Longitud, y = Anchura, color = Tipo)) + geom_point()
ggplot(BaseDatosFinal, aes(x = Grosor, y = Peso, color = Tipo)) + geom_point()

# 

BaseDatosFinal$Tipo_fac <- as.factor(BaseDatosFinal$Tipo)
BaseDatosFinal$Tipo_fac

# 

model <- glm(Tipo_fac ~ Anchura, data = BaseDatosFinal, family = binomial())
summary(model)

model2 <- glm(Tipo_fac ~ Longitud, data = BaseDatosFinal, family = binomial())
summary(model2)

model3 <- glm(Tipo_fac ~ Grosor, data = BaseDatosFinal, family = binomial())
summary(model3)

model4 <- glm(Tipo_fac ~ Peso, data = BaseDatosFinal, family = binomial())
summary(model4)

# 

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

#

BaseDatosFinal$Longitud_Anchura <- BaseDatosFinal$Longitud/BaseDatosFinal$Anchura

#

model5 <- glm(Tipo_fac ~ Longitud_Anchura, data = BaseDatosFinal, family = binomial())
summary(model5)

# 

model_8 <- glm(Tipo_fac ~ Longitud_Anchura + Peso, data = BaseDatosFinal, family = binomial())
summary(model_8)
model_9 <- glm(Tipo_fac ~ Longitud_Anchura + Grosor, data = BaseDatosFinal, family = binomial())
summary(model_9)

#

mean(BaseDatosFinal$Longitud_Anchura [BaseDatosFinal$Tipo == "0"]) # La proporcion es 1 por lo que anchura y largo son iguales en los "flake".

mean(BaseDatosFinal$Longitud_Anchura [BaseDatosFinal$Tipo == "1"]) # La proporcion es 1,33 por lo que la anchura es dos tercios de la longitud en "blade".

#

t.test(BaseDatosFinal$Anchura[BaseDatosFinal$Tipo == "1"], BaseDatosFinal$Longitud[BaseDatosFinal$Tipo == "1"])
t.test(BaseDatosFinal$Anchura[BaseDatosFinal$Tipo == "0"], BaseDatosFinal$Longitud[BaseDatosFinal$Tipo == "0"])




