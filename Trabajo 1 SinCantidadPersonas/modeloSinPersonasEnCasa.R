library(dplyr)
library(caret)
library(randomForest)
library(Metrics)

load("datos.rdata")

definitive_data <- datos %>% select(I_UGASTO,I_HOGAR,arriendo,num_dormitorios,
                    tipo_serv_sanitario,conyuges,num_cuartos,
                    obtencion_agua_alimento, hijos)

index = createDataPartition(y=definitive_data$hijos, p=0.75, list = FALSE)

train.set = definitive_data[index,]
test.set = definitive_data[-index,]

definitive_model <- randomForest(as.factor(hijos)~ I_UGASTO + I_HOGAR + arriendo +
                      num_dormitorios + tipo_serv_sanitario+conyuges +
                      num_cuartos+obtencion_agua_alimento, data=train.set)

prediccion1 = predict(definitive_model, test.set)
accuracy(test.set$hijos, prediccion1)

saveRDS(definitive_model, "definitive_model.rds")