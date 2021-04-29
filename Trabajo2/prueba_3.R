library(MASS)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(tidyverse)

# lectura de la base de datos
setwd("C:/Users/s540/Desktop/TAE/Trabajo2")
datos <- read.csv(file.choose(), sep = ";", header = T)

# semilla
set.seed(12345)

# solo variables numericas
sin_categoricos <- datos %>% select(!c(impo_cv,expo_vt,cxp,cxc,
                                      totalinventory,tiene_ventas_fisicas,
                                      tiene_ventas_electronicas,
                                      rotacion_inventarios,rotacion_cxc,
                                      rotacion_cxp,ciclo_negocio,
                                      ciclo_financiero))


# omitir valores faltantes
sum(is.na(sin_categoricos))

# gráfica de correlación
cormat <- round(cor(sin_categoricos[,-1]), 2)
ggcorrplot(cormat, hc.order = TRUE, 
           type = 'lower', outline.color = 'white')

# las variables están altamente correlacionadas
# por esa razón el uso de PCA es una buena opción
# explicación breve de en qué consiste el método


# análisis de componentes principales

# aplicame a todas las columnas la función var
apply(sin_categoricos[,-1], 2, var)

# centrar los datos y escalarlos
acp <- prcomp(sin_categoricos[,-1], 
              center = TRUE, scale = TRUE)


# Screeplot
plot(acp,type = 'l')

summary(acp)

# la varianza explicada disminuye drásticamente después de la PC3 (o la PC4?).
# se considerará este número para el análisis

pr_var <- acp$sdev^2
pve <- pr_var / sum(pr_var)
# plot cumulativo
plot(cumsum(pve), xlab = "Componentes principales",
     ylab = "Proporción acumulada de la varianza explicada",
     ylim = c(0,1),
     type = 'b')
# plot con las 35 componentes
plot(pve, xlab = "Componentes principales",
     ylab = "Proporción acumulada de la varianza explicada",
     ylim = c(0,1),
     type = 'b')

# si se elige k=3, se obtendrá menos del 40% de la varianza total
# de los datos. Este número no debería ser el indicado.

# qué tal un k=8? 

# investigar un poco de la regla de Kaiser (para elegir un k)

# juntando las PC
pc1 <- apply(acp$rotation[,1]*sin_categoricos[,-1], 1, sum)
pc2 <- apply(acp$rotation[,2]*sin_categoricos[,-1], 1, sum)
pc3 <- apply(acp$rotation[,3]*sin_categoricos[,-1], 1, sum)
pc4 <- apply(acp$rotation[,4]*sin_categoricos[,-1], 1, sum)
pc5 <- apply(acp$rotation[,5]*sin_categoricos[,-1], 1, sum)
pc6 <- apply(acp$rotation[,6]*sin_categoricos[,-1], 1, sum)
pc7 <- apply(acp$rotation[,7]*sin_categoricos[,-1], 1, sum)
pc8 <- apply(acp$rotation[,8]*sin_categoricos[,-1], 1, sum)


sin_categoricos <- sin_categoricos %>% mutate(
  pc1 = pc1,
  pc2 = pc2,
  pc3 = pc3,
  pc4 = pc4,
  pc5 = pc5,
  pc6 = pc6,
  pc7 = pc7,
  pc8 = pc8
)

sin_categoricos <- sin_categoricos %>% select(nit, pc1,
                          pc2,
                          pc3,
                          pc4,
                          pc5,
                          pc6,
                          pc7,
                          pc8)



# ejecutar algoritmo k-means
k <- kmeans(sin_categoricos[,-1], centers = 8, iter.max = 100)

# mostrar los centroides
k$centers

# cantidad de clientes en cada segmento
table(k$cluster)

# construyendo la visualización
centers <- k$centers
mini <- apply(sin_categoricos[,-1], 2, min)
maxi <- apply(sin_categoricos[,-1], 2, max)
mini <- as.numeric(mini)
maxi <- as.numeric(maxi)
centersde <- t(mini+t(centers)*(maxi-mini))
data_centers <- data.frame(centersde)
parcoord(data_centers, col=1:10, var.label = TRUE)
par(xpd = TRUE)
