rm(list = ls())

require("pacman")
p_load("tidyverse","sf","geojsonio")
p_load("leaflet")
library(readr)

train <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Repositorios/Taller_Chapinero_Chique_Sanchez_Castro/Data/Bogota_train.csv")
test  <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Repositorios/Taller_Chapinero_Chique_Sanchez_Castro/Data/Bogota_test.csv")

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

#Limpiando base

# * Veamos los NAs de la base

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

## * Imputando datos faltantes

# *** En train
train$rooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

train$bathrooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

train$rooms[is.na(train$rooms)] <- 3
train$bathrooms[is.na(train$bathrooms)] <- 2

sapply(train, function(x) sum(is.na(x)))

# *** En test
test$rooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

test$bathrooms %>%
  table(useNA = "ifany") %>%
  prop.table() %>%
  round(3)*100

test$rooms[is.na(test$rooms)] <- 3
test$bathrooms[is.na(test$bathrooms)] <- 2

sapply(test, function(x) sum(is.na(x)))


#Superlearner

bd<- train %>% mutate(logprice=log(price))

p_load("caret")
set.seed(1011)
inTrain <- createDataPartition(
  y = bd$logprice,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)


bdtrain_is <- bd[ inTrain,]
bdtest_is  <- bd[-inTrain,]
colnames(bdtrain_is)


p_load("SuperLearner")

modelo <- lm(price ~rooms + bathrooms + property_type + year + distancia_parque + 
               distancia_avenida_principal + distancia_comercial, data = bdtrain_is)

head(modelo)

p_load("stargazer")

stargazer(modelo, title = "Resultados de la regresión lineal", type = "text")

#Modelos disponibles
listWrappers()

ySL<- bdtrain_is$price
XSL<- bdtrain_is  %>% select(rooms, bathrooms, property_type, year, 
                             distancia_parque, distancia_avenida_principal, distancia_universidad, distancia_comercial)


sl.lib <- c("SL.randomForest", "SL.lm") #lista de los algoritmos a correr

# Fit using the SuperLearner package, 
install.packages("randomForest")

fitY <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

fitY

bdtest_is <- bdtest_is  %>%  mutate(yhat_Sup=predict(fitY, newdata = data.frame(bdtest_is), onlySL = T)$pred)
head(bdtest_is$yhat_Sup)

test <- test  %>%  mutate(yhat_Sup=predict(fitY, newdata = data.frame(test), onlySL = T)$pred)
head(test$yhat_Sup)

with(bdtest_is,mean(abs(price-yhat_Sup))) #MAE

test<- test  %>% mutate(Pred=(yhat_Sup))
colnames(test)

Submission3 <- test %>%
  select(property_id, yhat_Sup)

Submission3 <- Submission3 %>%
  rename(Price = yhat_Sup)

setwd("Desktop/MAESTRIA 2023/Big Data and Machine Learning/9. Talleres/Taller 3/Datos/")

write.csv(Submission3, file="submission3.csv", row.names = F)

##VARIABLE DE AREA
substr(bd$description, 1, 500)
s
library(stringi)
# Eliminamos tildes

bd$description <- tolower(bd$description)
bd$description <- iconv(bd$description, from = "UTF-8", to = "ASCII//TRANSLIT")

substr(bd$description, 1, 500)

library(stringr)

Metros <- str_extract(bd$description, "\\d+\\s*(mts|m2|metros)")

bd_new <- cbind(bd, area = Metros)

Area_sin_texto <- gsub("m2", "", Metros)
Area_sin_texto <- gsub("[[:alpha:]]", "", Area_sin_texto)
as.numeric(Area_sin_texto)

bd_new <- cbind(bd, area = Area_sin_texto)

sapply(bd_new, function(x) sum(is.na(x)))

bd_new$Superficie <- ifelse(is.na(bd_new$surface_covered), bd_new$area, bd_new$surface_covered)

sapply(bd_new, function(x) sum(is.na(x)))







