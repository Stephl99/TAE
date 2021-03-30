# Análisis descriptivo de los datos
setwd("C:/Users/s540/Documents/U/Semestres/2021-1S/TAE/TRABAJO 1")
library(Amelia)
load("./datos.RData")
summary(datos)
#' Valores faltantes:
#' P5022
#' P5030
#' P5047
#' P8536
#' educacion_padre
#' educacion_madre
#' es_campesino
#' condicion_vida
#' estrato
#  En la variable es_campesino hay valores sin sentido (valen 9)
missmap(datos, main = "Valores faltantes vs. Observados")

# buscando na's 

moda = getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

# número de na's por columnas
colSums(is.na(datos))

# como todas son categóricas se procederá a reemplazar con la moda
# na.omit(datos)

# ubicación servicio sanitario
moda_ubic_serv_sanitario <- moda(datos$ubic_serv_sanitario)
datos$ubic_serv_sanitario[is.na(datos$ubic_serv_sanitario)] <- moda_ubic_serv_sanitario

# educación madre
moda_educacion_madre <- moda(datos$educacion_madre)
datos$educacion_madre[is.na(datos$educacion_madre)] <- moda_educacion_madre

# condición de vida
moda_condicion_vida <- moda(datos$condicion_vida)
datos$condicion_vida[is.na(datos$condicion_vida)] <- moda_condicion_vida

# agua 24 horas
moda_agua_24h <- moda(datos$agua_24h)
datos$agua_24h[is.na(datos$agua_24h)] <- moda_agua_24h

# tipo energía
moda_tipo <- moda(datos$tipo_energia)
datos$tipo_energia[is.na(datos$tipo_energia)] <- moda_tipo

# educación padre
moda_educacion_padre <- moda(datos$educacion_padre)
datos$educacion_padre[is.na(datos$educacion_padre)] <- moda_educacion_padre

# estrato
moda_estrato <- moda(datos$estrato)
datos$estrato[is.na(datos$estrato)] <- moda_estrato

# servicio sanitario
moda_serv_sanitario_es  <- moda(datos$serv_sanitario_es)
datos$serv_sanitario_es[is.na(datos$serv_sanitario_es)] <- moda_serv_sanitario_es

colSums(is.na(datos))

save(datos, file = "./datos.RData")