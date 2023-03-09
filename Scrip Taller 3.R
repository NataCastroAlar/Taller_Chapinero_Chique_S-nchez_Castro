rm(list = ls())


require("pacman")
p_load("tidyverse","sf","geojsonio")
p_load("leaflet")
library(readr)

p_load(tidyverse, readr, skimr, fastDummies, rpart, caret, glmnet, MLmetrics,
       rstudioapi, # Get the location of this script
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       stargazer, # Report LM models
       osmdata)

train <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Taller 3/Datos/train.csv")
test <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Taller 3/Datos/test.csv")
submission <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Taller 3/Datos/submission_template.csv")

train_sf <- sf::st_as_sf(
  train,
  # "coords" is in x/y order -- so longitude goes first!
  coords = c("lon", "lat"),
  remove=FALSE,
  # Set our coordinate reference system to EPSG:4326,
  # the standard WGS84 geodetic coordinate reference system
  crs = 4326
)

class(train_sf)

#graficar puntos 
ggplot() +
  geom_sf(data=train_sf)+
  theme_bw()

#mapa interactivo 1
map_PS<-leaflet()  %>% 
  addTiles()  %>% 
  addCircleMarkers(data=train_sf)
map_PS

#mapa interactivo 2
map2_PS<-leaflet()  %>% 
  addProviderTiles(providers$Stamen.Toner)  %>% 
  addCircles(data=train_sf)
map2_PS

##LIMPIAR BASE

train$description <- tolower(train$description)
train$description <- iconv(train$description, from = "UTF-8", to = "ASCII//TRANSLIT")

##CLUSTERING

set.seed(101011)
train_sample<-train_sf  %>% sample_frac(size=1/20)  #una fracción de los datos para rapidez en clase, usted use todos
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

#Limpiando base

# * Veamos los NAs de la base

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

## * Imputando datos faltantes

# *** En train_hogares
train$rooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

train$bathrooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

train$rooms[is.na(train$rooms)] <- 3
train$bathrooms[is.na(train$bathrooms)] <- 3

sapply(train, function(x) sum(is.na(x)))

# *** En test_hogares
test$rooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

test$bathrooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

test$rooms[is.na(test$rooms)] <- 3
test$bathrooms[is.na(test$bathrooms)] <- 3

sapply(train, function(x) sum(is.na(x)))

sapply(test_hogares, function(x) sum(is.na(x)))


#Superlearner

train<- train  %>% mutate(logprice=log(price))

p_load("caret")
set.seed(1011)
inTrain <- createDataPartition(
  y = train$logprice,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)


train_is <- train[ inTrain,]
test_is  <- train[-inTrain,]
colnames(train_is)


p_load("SuperLearner")

#Modelos disponibles
listWrappers()

ySL<-train_is$logprice
XSL<- train_is  %>% select(rooms, bedrooms, bathrooms, property_type)

sapply(train_is, function(x) sum(is.na(x)))
sapply(test_is, function(x) sum(is.na(x)))

sl.lib <- c("SL.randomForest", "SL.lm") #lista de los algoritmos a correr

# Fit using the SuperLearner package, 
install.packages("randomForest")

fitY <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

fitY

test_is <- test_is  %>%  mutate(yhat_Sup=predict(fitY, newdata = data.frame(test_is), onlySL = T)$pred)
head(test_is$yhat_Sup)

with(test_is,mean(abs(logprice-yhat_Sup))) #MAE

test<- test  %>% mutate(Pred=exp(yhat_Sup))
colnames(test)

Submission1 <- test %>%
  select(property_id, Pred)

Submission1 <- Submission1 %>%
  rename(Price = Pred)

setwd("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Taller 3/Datos")

write.csv(Submission1, file="submission1.csv", row.names = F)





