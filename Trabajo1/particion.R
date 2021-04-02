setwd("C:/Users/ASUS/Desktop/T1-TAE")
load("C:/Users/ASUS/Desktop/T1-TAE/datos.RData")

library(caret)

datos_train <- datos %>%
        select(I_UGASTO,I_HOGAR,arriendo,CANT_PERSONAS_HOGAR,
               num_dormitorios, tipo_serv_sanitario,conyuges,
               num_cuartos,obtencion_agua_alimento,hijos)

datos_test <- datos %>%
        select(I_UGASTO,I_HOGAR,arriendo,CANT_PERSONAS_HOGAR,
               num_dormitorios, tipo_serv_sanitario,conyuges,
               num_cuartos,obtencion_agua_alimento)

#' particion datos en conjunto de entrenamiento y conjunto de
#' prueba

index = createDataPartition(y=datos$hijos, p=0.75, list = FALSE)

train.set = datos_train[index,]
test.set = datos_test[-index,]

save(train.set, file = "./train.RData")
save(test.set, file = "./test.RData")



