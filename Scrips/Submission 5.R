###SUBMISION 5##

rm(list = ls())

require("pacman")
p_load("tidyverse","sf","geojsonio")
p_load("leaflet")
p_load("skimr")
library(readr)

train <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Repositorios/Taller_Chapinero_Chique_Sanchez_Castro/Data/Bogota_train.csv")
test2 <- read_csv("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Repositorios/Taller_Chapinero_Chique_Sanchez_Castro/Data/Bogota_test.csv")

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

test2 <-  kNN(test2, variable = c("bathrooms"), k = 6)
test2$bathrooms <- round(test2$bathrooms,0)
summary(test2$bathrooms)

#-------> Outliers para bathrooms
#train
ggplot(train, aes(x = bathrooms)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia")

iqr_bathrooms <- IQR(train$bathrooms)
lim_inf <- quantile(train$bathrooms, 0.25) - 1.5 * iqr_bathrooms
lim_sup <- quantile(train$bathrooms, 0.75) + 1.5 * iqr_bathrooms

outliers_bathrooms <- train$bathrooms < lim_inf | train$bathrooms > lim_sup
train <- train[!outliers_bathrooms,]
#test
ggplot(test, aes(x = bathrooms)) +
  geom_histogram(binwidth = 0.5, color = "darkblue", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia") +
  theme_minimal()
  
iqr_bathrooms <- IQR(test$bathrooms)

lim_inf <- quantile(test$bathrooms, 0.25) - 1.5 * iqr_bathrooms
lim_sup <- quantile(test$bathrooms, 0.75) + 1.5 * iqr_bathrooms

outliers_bathrooms <- test$bathrooms < lim_inf | test$bathrooms > lim_sup
test <- test[!outliers_bathrooms,]

##---------------Creando varible para total cuartos---------------------------##

train$rooms[is.na(train$rooms)] <- 0
train$rooms_tot <- apply(train[, c("rooms", "bedrooms")], 1, max)

test2$rooms[is.na(test2$rooms)] <- 0
test2$rooms_tot <- apply(test2[, c("rooms", "bedrooms")], 1, max)

sapply(train, function(x) sum(is.na(x)))
sapply(test2, function(x) sum(is.na(x)))

#-------> Outliers para rooms_tot
#train
ggplot(train, aes(x = rooms_tot)) +
  geom_histogram(binwidth = 1.0, color = "black", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia")

iqr_rooms_tot <- IQR(train$rooms_tot)

lim_inf <- quantile(train$rooms_tot, 0.25) - 1.5 * iqr_rooms_tot
lim_sup <- quantile(train$rooms_tot, 0.75) + 1.5 * iqr_rooms_tot

outliers_rooms_tot <- train$rooms_tot < lim_inf | train$rooms_tot > lim_sup

train <- train[!outliers_rooms_tot,]
#test
ggplot(test, aes(x = rooms_tot)) +
  geom_histogram(binwidth = 1.0, color = "darkblue", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia") +
  theme_minimal()

iqr_rooms_tot <- IQR(test$rooms_tot)

lim_inf <- quantile(test$rooms_tot, 0.25) - 1.5 * iqr_rooms_tot
lim_sup <- quantile(test$rooms_tot, 0.75) + 1.5 * iqr_rooms_tot

outliers_rooms_tot <- test$rooms_tot < lim_inf | test$rooms_tot > lim_sup

test <- test[!outliers_rooms_tot,]

##--------------------Creando varible para area-------------------------------##
##train
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

##test

substr(test2$description, 1, 500)

##Volvemos todo minuscula y eliminamos tildes tildes
test2$description <- tolower(test2$description)
test2$description <- iconv(test2$description, from = "UTF-8", to = "ASCII//TRANSLIT")
substr(test2$description, 1, 500)

Metros <- str_extract(test2$description, "\\d+\\s*(mts|m2|metros)")

test2 <- cbind(test2, area = Metros)

Area_sin_texto <- gsub("m2", "", Metros)
Area_sin_texto <- gsub("[[:alpha:]]", "", Area_sin_texto)
as.numeric(Area_sin_texto)

test2 <- cbind(test2, area_m2 = Area_sin_texto)
sapply(test2, function(x) sum(is.na(x)))
test2$area_tot <- ifelse(is.na(test2$surface_covered), test2$area_m2, test2$surface_covered)

sapply(test2, function(x) sum(is.na(x)))

#Imputando faltantes en area_tot con KNN

train <-  kNN(train, variable = c("area_tot"), k = 6)

train$area_tot_num <- as.numeric(train$area_tot)
train$area_tot <- round(train$area_tot_num,0)

summary(train$area_tot)

test2 <-  kNN(test2, variable = c("area_tot"), k = 6)

test2$area_tot_num <- as.numeric(test2$area_tot)
test2$area_tot <- round(test2$area_tot_num,0)
summary(test2$area_tot)

sapply(train, function(x) sum(is.na(x)))
sapply(test2, function(x) sum(is.na(x)))

#--------------> Outliers de Area
#train
ggplot(train, aes(x = area_tot)) +
  geom_histogram(binwidth = 0.5, color = "darkblue", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia") +
  theme_minimal()

iqr_area_tot <- IQR(train$area_tot)

lim_inf <- quantile(train$area_tot, 0.25) - 1.5 * iqr_area_tot
lim_sup <- quantile(train$area_tot, 0.75) + 1.5 * iqr_area_tot

outliers_area_tot <- train$area_tot < lim_inf | train$area_tot > lim_sup

train <- train[!outliers_area_tot,]
#test
ggplot(test, aes(x = area_tot)) +
  geom_histogram(binwidth = 0.5, color = "darkblue", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia") +
  theme_minimal()

iqr_area_tot <- IQR(test$area_tot)

lim_inf <- quantile(test$area_tot, 0.25) - 1.5 * iqr_area_tot
lim_sup <- quantile(test$area_tot, 0.75) + 1.5 * iqr_area_tot

outliers_area_tot <- test$area_tot < lim_inf | test$area_tot > lim_sup

test <- test[!outliers_area_tot,]

#--------------> Outliers de Precio
#train

iqr_price <- IQR(train$price)

lim_inf <- quantile(train$price, 0.25) - 1.5 * iqr_price
lim_sup <- quantile(train$price, 0.75) + 1.5 * iqr_price

outliers_price <- train$price < lim_inf | train$price > lim_sup

train_op <- train[!outliers_price,]

ggplot(data = train_op, aes(x=area_tot, y = price)) + 
  geom_point(color = "#023e8a", size = 0.3) + 
  labs (x = "Area", y = "Precio") +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar)


##------------------Selección de variables para el modelo---------------------##

train <- select(train, property_id,  bathrooms, area_tot, bathrooms_imp, 
                lat, property_type, lon, distancia_parque, distancia_universidad, 
                rooms_tot, area_tot_imp,  price, distancia_avenida_principal)

test2 <- select(test2, property_id,  bathrooms, area_tot, bathrooms_imp, 
               lat, property_type, lon, distancia_parque, distancia_universidad, 
               rooms_tot, area_tot_imp,  price, distancia_avenida_principal)

train_op <- select(train_op, property_id,  bathrooms, area_tot, bathrooms_imp, 
                lat, property_type, lon, distancia_parque, distancia_universidad, 
                rooms_tot, area_tot_imp,  price, distancia_avenida_principal)

#---------------convertir los valores caracter a lógicos----------------------##
#Train
tipo_propiedad <- model.matrix(~ property_type - 1, train)

# unir las variables dummies al data frame original
train <- cbind(train, tipo_propiedad)

# verificar el nuevo data frame
train

train$tipo_propiedad <- 
  factor(ifelse(train$property_typeCasa ==1, "Casa", "Apartamento"))

#test

tipo_propiedad <- model.matrix(~ property_type - 1, test2)

# unir las variables dummies al data frame original
test2 <- cbind(test2, tipo_propiedad)

# verificar el nuevo data frame
test2

test2$tipo_propiedad <- 
  factor(ifelse(test2$property_typeCasa ==1, "Casa", "Apartamento"))

#Train_op
tipo_propiedad <- model.matrix(~ property_type - 1, train_op)

# unir las variables dummies al data frame original
train_op <- cbind(train_op, tipo_propiedad)

# verificar el nuevo data frame
train_op

train_op$tipo_propiedad <- 
  factor(ifelse(train_op$property_typeCasa ==1, "Casa", "Apartamento"))


##------------------------Estadísticas descriptivas--------------------------###

ggplot(train_sin_outliers, aes(x = area_tot)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia")

ggplot(test_sin_outliers, aes(x = property_type)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(title = "Histograma de datos", x = "Valores", y = "Frecuencia")

ggplot(data = train_sin_outliers, aes(x = area_tot, y = price)) +
  geom_point()


summary(train)
p_load(stargazer)

stargazer(train_sin_outliers, header = FALSE, type = "text", title = "Variables en la base Train")

ggplot(data = train, aes(x=area_tot, y = price)) + 
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


##---------------------------Regresión Lineal--------------------------------###

sapply(train_op, function(x) sum(is.na(x)))

modelo <- lm(price ~area_tot + lat + lon + distancia_universidad + area_tot_imp +
               distancia_avenida_principal + bathrooms + bathrooms_imp + tipo_propiedad + 
               distancia_parque + rooms_tot, data = train_op)

head(modelo)

p_load("stargazer")

stargazer(modelo, title = "Resultados de la regresión lineal", type = "text")


##-----------Superlearner--------##

bd<- train_op %>% mutate(logprice=log(price)) 

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
                             distancia_avenida_principal, bathrooms, bathrooms_imp, tipo_propiedad,  
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

MAE_S5 <- with(bdtest_is,mean(abs(price-yhat_Sup))) #MAE
MAE_S5

test2 <- test2  %>%  mutate(yhat_Sup=predict(fitY, newdata = data.frame(test2), onlySL = T)$pred)
head(test2$yhat_Sup)

#Redondear precio predicho

bdtest_is$price_round <- round(bdtest_is$yhat_Sup/1e5)*1e5
head(bdtest_is$price_round)

MAE_S5R <- with(bdtest_is,mean(abs(price-price_round))) #MAE
MAE_S5R

# Redondear en test
test2$price_round <- round(test2$yhat_Sup/1e5)*1e5
head(test2$price_round)

test$price <- ifelse(is.na(test$price), test$yhat_Sup, test$price)

summary(test)


# Distintos hiperparametros -----------------------------------------------

#Bosques con SL.ranger de 500, 750 y 1000 arboles
custom_ranger_500 = create.Learner("SL.ranger", params = list(num.trees = 500))
custom_ranger_750 = create.Learner("SL.ranger", params = list(num.trees = 750))
custom_ranger_1000 = create.Learner("SL.ranger", params = list(num.trees = 1000))

# Bosques con distinto numero de variables 1, 3, 6, 9, 11(bagging)
custom_rf = create.Learner("SL.randomForest",
                           tune = list(mtry = round(c(1,3,6,9,11))))


# EN, ridge y lasso
custom_glmnet = create.Learner("SL.glmnet", tune = list(alpha = seq(0, 1, length.out=5)))


# Fijar la libreria
sl.lib2 <- c("SL.randomForest", "SL.lm",custom_ranger_500$names,custom_ranger_750$names,custom_ranger_1000$names,custom_glmnet$names,custom_rf$names)



# Entrenar el modelo
fitY_long <- SuperLearner(Y = ySL, X = data.frame(XSL),
                          method = "method.NNLS", SL.library = sl.lib2)

# Resultado del Modelo
fitY_long


##------------Submission----------#####

test2<- test2  %>% mutate(Pred=(price_round))
colnames(test2)

Submission5 <- test2 %>%
  select(property_id, Pred)

Submission5 <- Submission5 %>%
  rename(Price = Pred)

setwd("~/Desktop/MAESTRIA 2023/Big Data and Machine Learning/Repositorios/Taller_Chapinero_Chique_Sanchez_Castro/Data/")

write.csv(Submission5, file="submission5N.csv", row.names = F)


