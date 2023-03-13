rm(list = ls())

require("pacman")
p_load("tidyverse","sf","geojsonio")
p_load("leaflet")
p_load("skimr")
library(readr)

train <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Repositorios/Taller_Chapinero_Chique_Sanchez_Castro/Data/Bogota_train.csv")
test  <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Repositorios/Taller_Chapinero_Chique_Sanchez_Castro/Data/Bogota_test.csv")


#Limpiando base

skim(train)
skim(test)

# * Veamos los NAs de la base

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

## * Imputando datos faltantes

library(ggplot2)
p_load(VIM)

train <-  kNN(train, variable = c("bathrooms"), k = 6)
train$bathrooms <- round(train$bathrooms,0)
summary(train$bathrooms)

test <-  kNN(test, variable = c("bathrooms"), k = 6)
test$bathrooms <- round(test$bathrooms,0)
summary(test$bathrooms)

# Generando variables

train$rooms[is.na(train$rooms)] <- 0
train$rooms_tot <- apply(train[, c("rooms", "bedrooms")], 1, max)

test$rooms[is.na(test$rooms)] <- 0
test$rooms_tot <- apply(test[, c("rooms", "bedrooms")], 1, max)

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

# Creando variable area desde descripción

##---->Para train
substr(train$description, 1, 500)

library(stringi)

##Volvemos todo minuscula y eliminamos tildes tildes
train$description <- tolower(train$description)
train$description <- iconv(train$description, from = "UTF-8", to = "ASCII//TRANSLIT")
substr(train$description, 1, 500)

Metros <- str_extract(train$description, "\\d+\\s*(mts|m2|metros)")

train <- cbind(train, area = Metros)

Area_sin_texto <- gsub("m2", "", Metros)
Area_sin_texto <- gsub("[[:alpha:]]", "", Area_sin_texto)
as.numeric(Area_sin_texto)

train <- cbind(train, area_m2 = Area_sin_texto)
sapply(train, function(x) sum(is.na(x)))
train$area_tot <- ifelse(is.na(train$surface_covered), train$area_m2, train$surface_covered)

sapply(train, function(x) sum(is.na(x)))

##---->Para test

substr(test$description, 1, 500)

##Volvemos todo minuscula y eliminamos tildes tildes
test$description <- tolower(test$description)
test$description <- iconv(test$description, from = "UTF-8", to = "ASCII//TRANSLIT")
substr(test$description, 1, 500)

Metros <- str_extract(test$description, "\\d+\\s*(mts|m2|metros)")

test <- cbind(test, area = Metros)

Area_sin_texto <- gsub("m2", "", Metros)
Area_sin_texto <- gsub("[[:alpha:]]", "", Area_sin_texto)
as.numeric(Area_sin_texto)

test <- cbind(test, area_m2 = Area_sin_texto)
sapply(test, function(x) sum(is.na(x)))
test$area_tot <- ifelse(is.na(test$surface_covered), test$area_m2, test$surface_covered)

sapply(test, function(x) sum(is.na(x)))

#Imputando faltantes en area con KNN

train <-  kNN(train, variable = c("area_tot"), k = 6)

train$area_tot_num <- as.numeric(train$area_tot)
train$area_tot <- round(train$area_tot_num,0)

summary(train$area_tot)

test <-  kNN(test, variable = c("area_tot"), k = 6)

test$area_tot_num <- as.numeric(test$area_tot)
test$area_tot <- round(test$area_tot_num,0)
summary(test$area_tot)

sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

#Selección de variables para el modelo

train <- select(train, property_id,  bathrooms, area_tot, bathrooms_imp, 
                lat, property_type, lon, distancia_parque, distancia_universidad, 
                rooms_tot, area_tot_imp,  price, distancia_avenida_principal)

test <- select(test, property_id,  bathrooms, area_tot, bathrooms_imp, 
                lat, property_type, lon, distancia_parque, distancia_universidad, 
                rooms_tot, area_tot_imp,  price, distancia_avenida_principal)

#Eliminando outliers
# Calcular cuartiles y rango intercuartil

train_new <- train
test_new <- test

#-----> para train

iqr_area_tot <- IQR(train_new$area_tot)

lim_inf <- quantile(train_new$area_tot, 0.25) - 1.5 * iqr_area_tot
lim_sup <- quantile(train_new$area_tot, 0.75) + 1.5 * iqr_area_tot

outliers_area_tot <- train_new$area_tot < lim_inf | train_new$area_tot > lim_sup

train_sin_outliers <- train_new[!outliers_area_tot,]

#-----> para test

iqr_area_tot <- IQR(test_new$area_tot)

lim_inf <- quantile(test_new$area_tot, 0.25) - 1.5 * iqr_area_tot
lim_sup <- quantile(test_new$area_tot, 0.75) + 1.5 * iqr_area_tot

outliers_area_tot <- test_new$area_tot < lim_inf | test_new$area_tot > lim_sup

test_sin_outliers <- test_new[!outliers_area_tot,]

ggplot(train_sin_outliers, aes(x = area_tot)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia")

ggplot(test_sin_outliers, aes(x = property_type)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia")



ggplot(data = train_sin_outliers, aes(x = area_tot, y = price)) +
  geom_point()

##------------------------Estadísticas descriptivas--------------------------###

summary(train_sin_outliers)
p_load(stargazer)

stargazer(train_sin_outliers, header = FALSE, type = "text", title = "Variables en la base Train")

ggplot(data = train_sin_outliers, aes(x=area_tot, y = price)) + 
  geom_point(color = "#023e8a", size = 0.3) + 
  labs (x = "Area", y = "Precio") +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar)

ggplot(data = train_sin_outliers, aes(x=rooms_tot, y = price)) + 
  geom_point(color = "darkblue", size = 0.3) + 
  labs (x = "Area", y = "Precio") +
  scale_y_continuous(labels = scales::dollar)

ggplot(data = train_sin_outliers, aes(x=distancia_avenida_principal, y = price)) + 
  geom_point(color = "darkblue", size = 0.3) + 
  theme_grey() +
  labs (x = "Area", y = "Precio") +
  scale_y_continuous(labels = scales::dollar)

ggplot(data = train_sin_outliers, aes(y = price, x = as.factor(tipo_propiedad))) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  labs (x = "Tipo Propiedad", y = "Precio")

ggplot(data = train_sin_outliers, aes(x = area_tot)) +
  geom_histogram()

#---------------convertir los valores caracter a lógicos--------------------

tipo_propiedad <- model.matrix(~ property_type - 1, train_sin_outliers)

# unir las variables dummies al data frame original
train_sin_outliers <- cbind(train_sin_outliers, tipo_propiedad)

# verificar el nuevo data frame
train_sin_outliers

train_sin_outliers$tipo_propiedad <- 
  factor(ifelse(train_sin_outliers$property_typeCasa ==1, "Casa", "Apartamento"))

##---------------------------Regresión Lineal--------------------------------###

sapply(train_sin_outliers, function(x) sum(is.na(x)))

modelo <- lm(price ~area_tot + lat + lon + distancia_universidad + area_tot_imp +
               distancia_avenida_principal + bathrooms + bathrooms_imp + property_type + 
               distancia_parque + rooms_tot, data = train_sin_outliers)

head(modelo)

p_load("stargazer")

stargazer(modelo, title = "Resultados de la regresión lineal", type = "text")


##-----------Superlearner--------##

bd<- train_sin_outliers %>% mutate(logprice=log(price)) 

p_load("caret")
p_load("SuperLearner")

set.seed(1011)
inTrain <- createDataPartition(
  y = bd$logprice,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

bdtrain_is <- bd[ inTrain,]
bdtest_is  <- bd[-inTrain,]
colnames(bdtrain_is)

#Modelos disponibles
listWrappers()

ySL<- bdtrain_is$price
XSL<- bdtrain_is  %>% select(area_tot, lat, lon, distancia_universidad, area_tot_imp,
                             distancia_avenida_principal, bathrooms, bathrooms_imp, property_type,  
                             distancia_parque, rooms_tot)

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

MAE_S4 <- with(bdtest_is,mean(abs(price-yhat_Sup))) #MAE
MAE_S4

#Redondear precio predicho

bdtest_is$price_round <- round(bdtest_is$yhat_Sup/1e5)*1e5
head(bdtest_is$price_round)

MAE_S4R <- with(bdtest_is,mean(abs(price-price_round))) #MAE
MAE_S4R

# Redondear en test
test$price_round <- round(test$price/1e4)*1e4

head(test$price_round)

test$price <- ifelse(is.na(test$price), test$yhat_Sup, test$price)


##------------Submission----------#####

test<- test  %>% mutate(Pred=(yhat_Sup))
colnames(test)

Submission4 <- test %>%
  select(property_id, yhat_Sup)

Submission4 <- Submission4 %>%
  rename(Price = yhat_Sup)

setwd("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Repositorios/Taller_Chapinero_Chique_Sanchez_Castro/Data/")

write.csv(Submission4, file="submission4.csv", row.names = F)
