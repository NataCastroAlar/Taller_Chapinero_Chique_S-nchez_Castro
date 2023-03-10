
rm(list = ls())

require("pacman")
p_load("tidyverse","sf","geojsonio")
p_load("leaflet")
library(readr)

train <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Repositorios/Taller_Chapinero_Chique_Sanchez_Castro/Data/train.csv")
test  <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Repositorios/Taller_Chapinero_Chique_Sanchez_Castro/Data/test.csv")

##CLUSTERING

set.seed(101011)
train_sample <-train %>% sample_frac(size=1/20)  #una fracción de los datos para rapidez en clase, usted use todos
db<- train_sample  %>%  select(geometry) #me quedo sólo con la geometría
head(db)

db_sample<-st_distance(db) #matriz de distancias
head(db_sample)

##Datos completos

#db_PS<- train_sf  %>%  select(geometry) #me quedo sólo con la geometría
#head(db_PS)

#db_PS<-st_distance(db_PS) #matriz de distancias
#head(db_PS)


db_sample<-units::drop_units(db_sample) #elimina las unidades de la matriz
head(db_sample)

k2 <- kmeans(db_sample, centers = 2, nstart = 25)
str(k2)

train_sample<- train_sample %>% mutate(clusters2=factor(k2$cluster))

ggplot() +
  geom_sf(data=train_sample,aes(col=clusters2)) + #graficamos las predicciones
  theme_bw()

#Numero de clusters a elegir

#Método del codo

# función que calcula la SSR within-cluster 
wss <- function(k) {
  kmeans(db_sample, k, nstart = 25 )$tot.withinss
}

# Calculamos y graficamos para k = 1 hasta k = 12
wss_values <- sapply(1:12,wss)

plot(1:12, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Número de clusters (K)",
     ylab="SSR within-clusters total")


p_load("cluster")
# función para extraer el coeficiente de silhouette

avg_sil <- function(k) {
  km.res <- kmeans(db_sample, centers = k, nstart = 25)
  ss <- cluster::silhouette(km.res$cluster, dist(db_sample))
  mean(ss[, 3])
}


# Calcular el coeficiente de silhouette para  k = 2 hasta k = 12
valores_sil <-  sapply(2:12,avg_sil)

plot(2:12, valores_sil,
     type = "b", pch = 19, frame = FALSE, 
     xlab="Número de clusters (K)",
     ylab = "Coeficiente de Silhouette")
