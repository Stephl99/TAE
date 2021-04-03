library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(Metrics)


load(file.choose())

# selección de variables
datos <- datos %>% select(I_HOGAR,I_UGASTO,arriendo,CANT_PERSONAS_HOGAR,
                 num_cuartos,num_dormitorios,tipo_serv_sanitario,
                 conyuges,obtencion_agua_alimento, hijos)

# particion datos
index = createDataPartition(y=datos$hijos, p=0.75, list = FALSE)

train.set = datos[index,]
test.set = datos[-index,]

# árbol de decisión
modelo1 <- rpart(formula=as.factor(hijos)~I_UGASTO+I_HOGAR+arriendo+
                         CANT_PERSONAS_HOGAR+num_dormitorios+
                         tipo_serv_sanitario+conyuges+
                         num_cuartos+obtencion_agua_alimento,
                 data=train.set, method="class")
summary(modelo1)
rpart.plot(modelo1)

# accuracy
prediccion1 <- predict(modelo1, test.set)
accuracy(test.set$hijos, round(prediccion1))

# random forest
modelo2 <- randomForest(as.factor(hijos)~I_UGASTO+ I_HOGAR+arriendo+
                             CANT_PERSONAS_HOGAR+num_dormitorios+
                             tipo_serv_sanitario+conyuges+
                             num_cuartos+obtencion_agua_alimento,
                     data=train.set)
print(modelo2) # view results
importance(modelo2) # importance of each predictor

# predicción y accuracy
prediccion2 <- predict(modelo2, test.set)
accuracy(test.set$hijos, prediccion2)



# creando un data frame para probar el modelo
nombre <- c("I_UGASTO", "I_HOGAR",  "arriendo", "CANT_PERSONAS_HOGAR",
            "num_dormitorios", "tipo_serv_sanitari", "conyuges", "num_cuartos", "obtencion_agua_alimento")
valor <- c(33400000.0, 3634652.8, 430000, 5,3,2,1,4,2)
x0 <- list(valor)
typeof(test.set[1,])
predict(modelo2, x0)


prediccion2 <- predict(modelo2, data.frame(I_UGASTO = 33400000.0, I_HOGAR = 3634652.8, arriendo = 430000,
                                        CANT_PERSONAS_HOGAR = 4, num_dormitorios=3,
                                        tipo_serv_sanitario=1, conyuges =1,
                                        num_cuartos=2, obtencion_agua_alimento=1))
