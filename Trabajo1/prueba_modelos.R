# PRUEBA DE MODELOS GAMLSS
library(gamlss)
library(gamlss.dist)
library(dplyr)
library(tibble)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(MASS)
library(tree)
library(Metrics)

# hacer base de prueba
base_prueba <- datos %>% 
  select(-indice,-DIRECTORIO)

# particion datos
index = createDataPartition(y=datos$hijos, p=0.75, list = FALSE)

train.set = datos[index,]
test.set = datos[-index,]


# nuevas bases
base_nueva <- datos %>% 
  select(I_UGASTO, I_HOGAR,  arriendo, CANT_PERSONAS_HOGAR,
         num_dormitorios, tipo_serv_sanitario, conyuges, num_cuartos, obtencion_agua_alimento, hijos)

train.set = base_nueva[index,]
test.set = base_nueva[-index,]



# prueba árboles de decisión
fit <- rpart(formula=as.factor(hijos)~I_UGASTO + I_HOGAR + arriendo + CANT_PERSONAS_HOGAR+
               num_dormitorios+tipo_serv_sanitario+conyuges+num_cuartos+obtencion_agua_alimento, data=train.set, method="class") 
summary(fit)
rpart.plot(fit)


x0 <- c(4300000.0, 3960000.0, 400000,5,3,2,1,3,5)	
prediccion = predict(fit, x0)

# accuracy
accuracy(test.set$hijos, round(prediccion))

# prueba random forest
fit1 <- randomForest(as.factor(hijos)~I_UGASTO + I_HOGAR + arriendo + CANT_PERSONAS_HOGAR+
                       num_dormitorios+tipo_serv_sanitario+conyuges+num_cuartos+obtencion_agua_alimento,   data=train.set)
print(fit) # view results
importance(fit) # importance of each predictor

# predicción y accuracy
prediccion1 = predict(fit1, test.set)
accuracy(test.set$hijos, prediccion1)



# creando un data frame para probar el modelo
nombre <- c("I_UGASTO", "I_HOGAR",  "arriendo", "CANT_PERSONAS_HOGAR",
              "num_dormitorios", "tipo_serv_sanitari", "conyuges", "num_cuartos", "obtencion_agua_alimento")
valor <- c(33400000.0, 3634652.8, 430000, 5,3,2,1,4,2)
x0 <- list(valor)
typeof(test.set[1,])
predict(fit1, x0)


prediccion2 <- predict(fit1, data.frame(I_UGASTO = 33400000.0, I_HOGAR = 3634652.8, arriendo = 430000, 
                                        CANT_PERSONAS_HOGAR = 4, num_dormitorios=3, 
                                        tipo_serv_sanitario=1, conyuges =1,
                                        num_cuartos=2, obtencion_agua_alimento=1))

