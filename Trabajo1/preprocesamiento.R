#' Trabajo 1: TAE - Inicio
#' En este script se creó un archivo de datos en términos del hogar que
#'  contienen a la variable respuesta y algunas variables sobre servicios
#'  del hogar
#'

setwd("C:/Users/ASUS/Desktop/T1-TAE")
library(dplyr)

# Lectura de datos -------------
# viviendas
vivienda <- read.csv("./Datos de la vivienda.csv", header = T, sep = ";")
# hogares
serv_hogar <- read.csv("./Servicios del hogar.csv", header = T, sep = ";")
condiciones_vida <- read.csv("./Condiciones de vida del hogar y tenencia de bienes.csv", header = T, sep = ";")
tenencia <- read.csv("./Tenencia y financiación de la vivienda que ocupa el hogar.csv", header = T, sep = ";")
energeticos <- read.csv("./Uso de energéticos del hogar.csv", header = T, sep = ";")

# personas
composicion <- read.csv("./Caracteristicas y composicion del hogar.csv", header = T, sep = ";")
trabajo <- read.csv("./Fuerza de Trabajo.csv", header = T, sep = ";")
tic <- read.csv("./Tecnologías de información y comunicación.csv", header = T, sep = ";")

serv_hogar %>% rename(DIRECTORIO = ï..DIRECTORIO) -> serv_hogar
composicion %>% rename(DIRECTORIO = ï..DIRECTORIO) -> composicion


# Creación de la variable respuesta ----------

# Union de servicios del hogar con composicion del hogar
union_servhogar_composicion <- merge(serv_hogar, composicion,
                                     by.x =  c("DIRECTORIO", "SECUENCIA_ENCUESTA"),
                                     by.y = c("DIRECTORIO", "SECUENCIA_P"))



# creacion de id unico por hogar
for (i in 93993) {
        indice <- paste(union_servhogar_composicion$DIRECTORIO,
                        union_servhogar_composicion$SECUENCIA_ENCUESTA,
                        sep = "")
}


union_servhogar_composicion %>%
        mutate(indice = indice) -> union_servhogar_composicion

union_servhogar_composicion %>%
        relocate(indice) -> union_servhogar_composicion

union_servhogar_composicion %>%
        relocate(P6051) -> union_servhogar_composicion

# conteo de hijos por hogar
union_servhogar_composicion %>%
        filter(P6051 == 3) %>%
        count(indice) -> hijos_hogar
hijos_hogar <- hijos_hogar %>% rename(hijos = n)
nrow(hijos_hogar)

for (i in 93993) {
        indice <- paste(serv_hogar$DIRECTORIO,
                        serv_hogar$SECUENCIA_ENCUESTA,
                        sep = "")
}

serv_hogar <- serv_hogar %>% mutate(indice = indice) %>%
        relocate(indice)

#' unir número de hijos con la base de datos serv_hogar, que esta en
#'  términos del hogar
serv_hogar <- merge(serv_hogar, hijos_hogar,
                    by =  c("indice"), all = TRUE)

serv_hogar <- serv_hogar %>% relocate(hijos)
serv_hogar$hijos[is.na(serv_hogar$hijos)] <- 0


# Transformación de algunas variables desde la base de datos de -----
# servicios del hogar y seleccion e una base de datos inicial

#' renombrar, conversión a factores de algunas variables de la base
#' de datos de servicios del hogar

serv_hogar <- serv_hogar %>% rename(num_cuartos = P5000,
                                    num_dormitorios = P5010)
serv_hogar$P5016S5[is.na(serv_hogar$P5016S5)] <- 0
serv_hogar$P5016S6[is.na(serv_hogar$P5016S6)] <- 0
serv_hogar$P5016S2[is.na(serv_hogar$P5016S2)] <- 0
serv_hogar$P5016S3[is.na(serv_hogar$P5016S3)] <- 0
serv_hogar$P5016S4[is.na(serv_hogar$P5016S4)] <- 0

serv_hogar$P5016S5 <- as.factor(serv_hogar$P5016S5)
serv_hogar$P5016S6 <- as.factor(serv_hogar$P5016S6)
serv_hogar$P5016S2 <- as.factor(serv_hogar$P5016S2)
serv_hogar$P5016S3 <- as.factor(serv_hogar$P5016S3)
serv_hogar$P5016S4 <- as.factor(serv_hogar$P5016S4)

serv_hogar$P3000 <- as.factor(serv_hogar$P3000)
serv_hogar$P5666 <- as.factor(serv_hogar$P5666)
serv_hogar$P8526 <- as.factor(serv_hogar$P8526)
serv_hogar$P5022 <- as.factor(serv_hogar$P5022)
serv_hogar$P5030 <- as.factor(serv_hogar$P5030)
serv_hogar$P3000 <- as.factor(serv_hogar$P3000)

serv_hogar$P1892S1[is.na(serv_hogar$P1892S1)] <- 0
serv_hogar$P1892S2[is.na(serv_hogar$P1892S2)] <- 0
serv_hogar$P1892S3[is.na(serv_hogar$P1892S3)] <- 0
serv_hogar$P1892S4[is.na(serv_hogar$P1892S4)] <- 0

serv_hogar$P1892S1 <- as.factor(serv_hogar$P1892S1)
serv_hogar$P1892S2 <- as.factor(serv_hogar$P1892S2)
serv_hogar$P1892S3 <- as.factor(serv_hogar$P1892S3)
serv_hogar$P1892S4 <- as.factor(serv_hogar$P1892S4)
serv_hogar$P1893 <- as.factor(serv_hogar$P1893)
serv_hogar$P5041 <- as.factor(serv_hogar$P5041)
serv_hogar$P5046 <- as.factor(serv_hogar$P5046)

serv_hogar$P5012S1 <- as.factor(serv_hogar$P5012S1)
serv_hogar$P5012S2 <- as.factor(serv_hogar$P5012S2)
serv_hogar$P5012S3 <- as.factor(serv_hogar$P5012S3)
serv_hogar$P5012S8 <- as.factor(serv_hogar$P5012S8)
serv_hogar$P5012S4 <- as.factor(serv_hogar$P5012S4)
serv_hogar$P5012S5 <- as.factor(serv_hogar$P5012S5)
serv_hogar$P5012S6 <- as.factor(serv_hogar$P5012S6)
serv_hogar$P5012S7 <- as.factor(serv_hogar$P5012S7)

serv_hogar$P8530 <- as.factor(serv_hogar$P8530)
serv_hogar$P5047 <- as.factor(serv_hogar$P5047)
serv_hogar$P5069 <- as.factor(serv_hogar$P5069)
serv_hogar$P764 <- as.factor(serv_hogar$P764)
serv_hogar$P8536 <- as.factor(serv_hogar$P8536)

# Seleccion de variables desde la base de datos de servicios del hogar
base <- serv_hogar %>% select(indice, DIRECTORIO,hijos,num_cuartos,
                              num_dormitorios,P5016S5,
                              P5016S6,P5016S2,P5016S3,P5016S4,
                              P3000,P5666,P8526,P5022,P5030,P3000,P1892S1,
                              P1892S2,P1892S3,P1892S4,P1893,P5041,P5046,
                              P5012S1,P5012S2,P5012S3,P5012S8,P5012S4,P5012S5,
                              P5012S6,P5012S7,P8530,P5047,P5069,P764,
                              P8536, I_HOGAR, I_UGASTO, CANT_PERSONAS_HOGAR)

base$I_HOGAR <- as.numeric(gsub(",",".",base$I_HOGAR))
base$I_UGASTO <- as.numeric(gsub(",",".",base$I_UGASTO))









# Creación de nuevas variables en términos del hogar -----

# A partir de los datos: Características y composición del hogar

composicion <- composicion %>% rename(DIRECTORIO = ï..DIRECTORIO)
for (i in 93993) {
        indice <- paste(composicion$DIRECTORIO,
                        composicion$SECUENCIA_ENCUESTA,
                        sep = "")
}

composicion <- composicion %>%
        mutate(indice = indice) %>%
        relocate(indice)

#' conyuges:
#' 0 - No tiene
#' 1 - si vive en el hogar
#' 2 - No vive en el hogar
conyuges <- composicion %>% filter(P6051 == 1) %>% select(P6071)
conyuges$P6071[is.na(conyuges)] <- 0
base <- base %>% mutate(conyuges = conyuges)


#' padre vive en el hogar:
#' 1 - si
#' 2 - no
#' 3 - fallecido
padre_hogar <- composicion %>% filter(P6051 == 1) %>% select(P6081)
base <- base %>% mutate(padre_hogar = padre_hogar)

# educación padre
# 1 - primaria incompleta
# 2 - primaria completa
# 3 - secundaria incompleta
# 4 - secundaria completa
# 5 - tecnica incompleta
# 6 - tecnica completa
# 7 - universidad incompleta
# 8 - universitaria completa
# 9 - Ninguno
# 10 - No sabe
base <- base %>% mutate(educacion_padre = composicion %>%
                                filter(P6051 == 1) %>% select(P6087))
# base$educacion_padre <- as.factor(base$educacion_padre)

#' madre vive en el hogar:
#' 1 - si
#' 2 - no
#' 3 - fallecido
madre_hogar <- composicion %>% filter(P6051 == 1) %>% select(P6083)
base <- base %>% mutate(madre_hogar = madre_hogar)

# educación madre
# 1 - primaria incompleta
# 2 - primaria completa
# 3 - secundaria incompleta
# 4 - secundaria completa
# 5 - tecnica incompleta
# 6 - tecnica completa
# 7 - universidad incompleta
# 8 - universitaria completa
# 9 - Ninguno
# 10 - No sabe
base <- base %>% mutate(educacion_madre = composicion %>%
                                filter(P6051 == 1) %>% select(P6088))
# base$educacion_madre <- as.factor(base$educacion_madre)

# cultura
# 1 Indígena
# 2 Gitano (a) (Rom)
# 3 Raizal del archipiélago de San Andrés, Providencia y Santa Catalina
# 4 Palenquero (a) de San Basilio
# 5 Negro (a), mulato (a) (afrodescendiente), afrocolombiano(a)
# 6 Ninguno de los anteriores
base <- base %>% mutate(cultura = composicion %>%
                                filter(P6051 == 1) %>% select(P6080))
# base$cultura <- as.factor(base$cultura)

# es_campesino
# 1 Si
# 2 No
# 3 No informa
base <- base %>% mutate(es_campesino = composicion %>%
                                filter(P6051 == 1) %>% select(P2061))
# base$es_campesino <- as.factor(base$es_campesino)

#' condicion vida: ¿En cuál escalón diría usted que se encuentra
#' parado(a) en este momento?
#' 10 Mejor vida
# 9
# 8
# 7
# 6
# 5
# 4
# 3
# 2
# 1
# 0 Peor vida
base <- base %>% mutate(condicion_vida = composicion %>%
                                filter(P6051 == 1) %>% select(P1927))


# A partir de los datos: Datos de vivienda

vivienda <- vivienda %>% rename(DIRECTORIO = ï..DIRECTORIO)

# La llave entre vivienda y hogar es el directorio

vivienda_hogar <- merge(base, vivienda, by = "DIRECTORIO")

# Nuevas variables:

# Región
# 1 Caribe
# 2 Oriental
# 3 Central
# 4 Pacífica(sin valle)
# 5 Bogotá
# 6 Antioquia
# 7 Valle del cauca
# 8 San Andrés
# 9 Orinoquía - amazonía
base <- base %>% mutate(region = vivienda_hogar %>% select(REGION))

# Tipo de vivienda:
# 1 Casa
# 2 Apartamento
# 3 Cuarto(s)
# 4 Vivienda tradicional indigena
# 5 Otro (carpa, contenedor, vagón, embarcación, cueva, refugio natural, etc)

base <- base %>% mutate(tipo_vivienda = vivienda_hogar %>% select(P1070))

# Energía eléctrica
# 0 - no
# 1 - si
base <- base %>% mutate(energia = vivienda_hogar %>% select(P8520S1))
base$energia <-  ifelse(base$energia == 2,0,1)

# estrato
# 1 Bajo - Bajo
# 2 Bajo
# 3 Medio - Bajo
# 4 Medio
# 5 Medio - Alto
# 6 Alto
# 8 Planta eléctrica
# 9 No conoce el estrato o no cuenta con recibo de pago.
# 0 Recibos sin estrato o el servicio es pirata
base <- base %>% mutate(estrato = vivienda_hogar %>% select(P8520S1A1))

# acueducto
# 0 - no
# 1 - si

base <- base %>% mutate(acueducto = vivienda_hogar %>% select(P8520S5))
base$acueducto <-  ifelse(base$acueducto == 2,0,1)


# alcantarillado
# 0 - no
# 1 - si
base <- base %>% mutate(alcantarillado = vivienda_hogar %>% select(P8520S5))
base$alcantarillado <-  ifelse(base$alcantarillado == 2,0,1)

#'  A partir de la base de datos "Tenencia y financiación de la vivienda
#'   que ocupa el hogar"

# vivienda_ocupada: ¿La vivienda ocupada por este hogar es?
# 1. Propia, totalmente pagada
# 2. Propia, la están pagando
# 3. En arriendo o subarriendo
# 4. Con permiso del propietario, sin pago alguno (usufructuario)
# 5. Posesión sin título (ocupante de hecho)
# 6. Propiedad colectiva
base <- base %>% mutate(vivienda_ocupada = tenencia %>%
                                select(P5095))

# arriendo: cuanto pagan mensualmente por arriendo
base <- base %>% mutate(arriendo = tenencia %>%
                                select(P5140))
base$arriendo[is.na(base$arriendo)] <- 0

head(base)

#' A partir de la base de datos "Uso de energéticos del hogar"

# Número de computadores en el hogar
base <- base %>% mutate(num_computadores = energeticos %>%
                                select(P5529))
base$num_computadores[is.na(base$num_computadores)] <- 0

# A partir de la base de datos "Fuerza de trabajo"
trabajo %>% rename(DIRECTORIO = ï..DIRECTORIO) -> trabajo

for (i in 93993) {
        indice_p <- paste(composicion$DIRECTORIO,
                          composicion$SECUENCIA_P,
                          composicion$SECUENCIA_ENCUESTA,
                          sep = "")
}
composicion <- composicion %>% mutate(indice_p = indice_p)


# id trabajo
for (i in 93993) {
        indice_p <- paste(trabajo$DIRECTORIO,
                          trabajo$SECUENCIA_P,
                          trabajo$SECUENCIA_ENCUESTA,
                          sep = "")
}
trabajo <- trabajo %>% mutate(indice_p = indice_p)

trabajo_composicion <- merge(composicion,trabajo,
                             by = c("indice_p"),
                             all.x = T)

# fisico_t : el trabajo exige mucho esfuerzo fisico
# 0 - no
# 1 - si

base <- base %>% mutate(fisico_t = trabajo_composicion %>%
                                filter(P6051 == 1) %>%
                                select(P1709S1))
base$fisico_t[is.na(base$fisico_t)] <- 0

# intelectual_t:el trabajo exige mucho esfuerzo intelectual
# 0 - no
# 1 - si
base <- base %>% mutate(intelectual_t = trabajo_composicion %>%
                                filter(P6051 == 1) %>%
                                select(P1709S2))
base$intelectual_t[is.na(base$intelectual_t)] <- 0


# Exportar los datos preprocesados ---------------

datos <- as.data.frame(base)
save(datos, file = "./datos.RData")
