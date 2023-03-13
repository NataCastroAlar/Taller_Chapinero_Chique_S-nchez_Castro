
###########################
#MODELOS ARBOLES
#GRUPO 6
#TALLER 3 - PRECIOS CHAPINERO
###########################

rm(list = ls())

library(readr)
library(pacman)
p_load(tidyverse, readr, skimr, fastDummies, caret, glmnet, MLmetrics)

#Importar datos
Bogota_train <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Archivos R/DATOS TALLERES/Bogota_train.csv")
train<-Bogota_train
Bogota_test <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Archivos R/DATOS TALLERES/Bogota_test.csv")
test<-Bogota_test


## Con variable superficie
Bogota_train_superficie <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Archivos R/DATOS TALLERES/Bogota_train_superficie.csv")

Bogota_test_superficie <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/Archivos R/DATOS TALLERES/Bogota_test_superficie.csv")

p_load(VIM)

Bogota_train_superficie <-  kNN(Bogota_train_superficie, variable = c("Superficie"), k = 6)

Bogota_train_superficie$Superficie <- round(Bogota_train_superficie$Superficie,0)


superficie<-Bogota_train_superficie%>%
select(property_id, Superficie)

train <- train %>%
left_join(superficie, by = c("property_id"))
 summary(train)

train<-train %>%
   mutate(property_type_d = if_else(property_type == "Casa", 1, 0))

library(stargazer)
 p_load(stargazer, tidyverse,rvest,skimr)


train_ED<-train%>%
  select(price, Superficie, property_type_d, bedrooms, bathrooms, Superficie, distancia_parque, distancia_comercial, distancia_avenida_principal, distancia_universidad )

stargazer(as.data.frame(train_ED), type="text", title= "Estadísticas",
           digits=3, summary.stat=c("min", "p25", "mean", "p75", "max", "median", "sd"))

colSums(is.na(train))
colSums(is.na(test))


###ESCOGEMOS LA MEDIA PARA IMPUTAR EN ROOMS Y SURFACE COVER------------------------------

# Imputación Manual

train$rooms[is.na(train$rooms)] <- mean(train$rooms, na.rm = T)
train$surface_covered[is.na(train$surface_covered)] <- mean(train$surface_covered, na.rm = T)
train$ bathrooms[is.na(train$bathrooms)] <- mean(train$bathrooms, na.rm = T)
train$Superficie[is.na(train$Superficie)] <- mean(train$Superficie, na.rm = T)


test$rooms[is.na(test$rooms)] <- mean(test$rooms, na.rm = T)
test$surface_covered[is.na(test$surface_covered)] <- mean(test$surface_covered, na.rm = T)



#SELECCIONAMOS VARIABLES POTENCIALES PARA EL MODELO----------------------------------------

train<-train%>%
  select(price, bedrooms, bathrooms, surface_covered, Superficie, property_type, lat, lon, rooms, distancia_parque, distancia_comercial, distancia_avenida_principal, distancia_universidad)
test<-test%>% 
  select(property_id, price, bedrooms, bathrooms, surface_covered, lat, lon, rooms, property_type, distancia_parque, distancia_comercial, distancia_avenida_principal, distancia_universidad)

#FACTORES
train<-train%>%
  mutate(tipo=factor(property_type))

test<-test%>%
  mutate(tipo=factor(property_type))

##REVISAMOS DATOS-------------------------------------------------------------------

summary(train)

#ELIMINAMOS OUTLIERS------------------------------------------------------------------------

#Eliminamos outliers precios
Q1 <- quantile(train$price, .25)
Q3 <- quantile(train$price, .75)
IQR <- IQR(train$price)
train <- subset(train, train$price > (Q1 - 1.5*IQR) & train$price < (Q3 + 1.5*IQR))

#Eliminamos outliers bedrooms
train<-train%>%
  filter(bedrooms <10 & bedrooms >0)

#Eliminamos outliers distancia parque
Q1_DP <- quantile(train$distancia_parque, .25)
Q3_DP <- quantile(train$distancia_parque, .75)
IQR_DP <- IQR(train$distancia_parque)
train <- subset(train, train$distancia_parque> (Q1_DP - 1.5*IQR_DP) & train$distancia_parque < (Q3_DP + 1.5*IQR_DP))

#Eliminamos outliers distancia a área comercial
Q1_DC <- quantile(train$distancia_comercial, .25)
Q3_DC <- quantile(train$distancia_comercial, .75)
IQR_DC <- IQR(train$distancia_comercial)
train <- subset(train, train$distancia_comercial> (Q1_DC - 1.5*IQR_DC) & train$distancia_comercial < (Q3_DC + 1.5*IQR_DC))

#Eliminamos outliers distancia a avenida principal
Q1_AP <- quantile(train$distancia_avenida_principal, .25)
Q3_AP <- quantile(train$distancia_avenida_principal, .75)
IQR_AP <- IQR(train$distancia_avenida_principal)
train <- subset(train, train$distancia_avenida_principal> (Q1_AP - 1.5*IQR_AP) & train$distancia_avenida_principal < (Q3_AP + 1.5*IQR_AP))

#Eliminamos outliers distancia a universidad
Q1_U <- quantile(train$distancia_universidad, .25)
Q3_U <- quantile(train$distancia_universidad, .75)
IQR_U <- IQR(train$distancia_universidad)
train <- subset(train, train$distancia_universidad> (Q1_U - 1.5*IQR_U) & train$distancia_universidad < (Q3_U + 1.5*IQR_U))

#Eliminamos outliers distancia a universidad
train<-train%>%
  filter( surface_covered < 500 )


####DIVIDIMOS LA MUESTRA EN TRAIN Y TEST

set.seed(1011)
inTrain <- createDataPartition(
  y = train$price,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

train_muestra <- train[ inTrain,]
test_muestra  <- train[-inTrain,]


##ARBOLES---------------------------------------------------------------------

set.seed(123)


#Cross validation V=5

cv10<- trainControl(number = 10, method ="cv")

arbol_1<-train(price ~ lat + lon + Superficie+  property_type + distancia_parque+ distancia_comercial+ distancia_avenida_principal +
                         distancia_universidad,
                       data=train,
               method="rpart",
               trControl = cv10)

p_load(rattle)
fancyRpartPlot(arbol_1$finalModel)

pred_train_arbol_muestra<-predict(arbol_1, newdata=train_muestra)
pred_test_arbol_muestra<-predict(arbol_1, newdata=test_muestra)


#Error en porcentaje
MAPE(y_pred=pred_train_arbol_muestra, y_true = train_muestra$price)

#Error promedio
MAE(y_pred=pred_train_arbol_muestra, y_true = train_muestra$price)

## FUERA DE MUESTRA
#Error en porcentaje
MAPE(y_pred=pred_test_arbol_muestra, y_true = test_muestra$price)

#Error promedio
MAE(y_pred=pred_test_arbol_muestra, y_true = test_muestra$price)



##RANDOM FOREST------------------------------------------------------------

set.seed(123)

summary(train_muestra)

#Cross validation V=8

cv10<- trainControl(number = 10, method ="cv")

tunegrid_rf<-expand.grid(mtry=c(2,3,4,5, 8), #Predictores aleatorios
                         splitrule= "variance", ##Cambiar por gini y revisar
                         min.node.size=c(1,2,3,6))


rforest<-train(price ~ bedrooms + lat + lon + bathrooms + property_type + Superficie + distancia_parque+ distancia_comercial+ distancia_avenida_principal +
                 distancia_universidad,
               data=train_muestra, 
               trControl = cv10,
               metric = "RMSE",
               tuneGrid = tunegrid_rf,
               method ="ranger")


plot(rforest)

pred_train_2_muestra<-predict(rforest, newdata=train_muestra)
pred_test_2_muestra<-predict(rforest, newdata=test_muestra)

##EN MUESTRA
#Error en porcentaje
MAPE(y_pred=pred_train_2_muestra, y_true = train_muestra$price)

#Error promedio
MAE(y_pred=pred_train_2_muestra, y_true = train_muestra$price)

## FUERA DE MUESTRA
#Error en porcentaje
MAPE(y_pred=pred_test_2_muestra, y_true = test_muestra$price)

#Error promedio
MAE(y_pred=pred_test_2_muestra, y_true = test_muestra$price)


##PREDICCION TEST KAGGLE RANDOM FOREST 
pred_test_forest<-predict(rforest, newdata=test)
pred_test_forest

pred_test_forest_round<- round(pred_test_forest, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_forest_round)
test_k

write_csv(test_k, file="Bogota_kaggle_R1103F.csv")



#### OLS - ELASTICNET------------------------------------------------------------
##ALPHA Y LAMBDA
##OLS---------------------------------------
p_load("caret")

set.seed(123)
fitControl <- trainControl(## 8-fold CV
  method = "cv",
  number = 10)


fmla<-formula(price ~ bedrooms + bathrooms + property_type + Superficie + distancia_parque+ distancia_comercial+ distancia_avenida_principal +
                distancia_universidad)

linear_reg<-train(fmla,
                  data=train_muestra,
                  method = 'lm', 
                  trControl = fitControl,
                  preProcess = c("center", "scale")
) 


linear_reg
summary(linear_reg)

pred_train_OLS_muestra<-predict(linear_reg, newdata=train_muestra)
pred_test_OLS_muestra<-predict(linear_reg, newdata=test_muestra)

##EN MUESTRA
#Error en porcentaje
MAPE(y_pred=pred_train_OLS_muestra, y_true = train_muestra$price)

#Error promedio
MAE(y_pred=pred_train_OLS_muestra, y_true = train_muestra$price)

## FUERA DE MUESTRA
#Error en porcentaje
MAPE(y_pred=pred_test_OLS_muestra, y_true = test_muestra$price)

#Error promedio
MAE(y_pred=pred_test_OLS_muestra, y_true = test_muestra$price)




