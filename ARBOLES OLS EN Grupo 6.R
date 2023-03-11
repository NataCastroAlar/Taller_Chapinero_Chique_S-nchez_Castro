pre_proc_val <- preProcess(train[,cols], method = c("center", "scale"))
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

colSums(is.na(train))
colSums(is.na(test))

###ESCOGEMOS LA MEDIA PARA IMPUTAR EN ROOMS Y SURFACE COVER------------------------------

# Imputación Manual

train$rooms[is.na(train$rooms)] <- mean(train$rooms, na.rm = T)
train$surface_covered[is.na(train$surface_covered)] <- mean(train$surface_covered, na.rm = T)

test$rooms[is.na(test$rooms)] <- mean(test$rooms, na.rm = T)
test$surface_covered[is.na(test$surface_covered)] <- mean(test$surface_covered, na.rm = T)



#SELECCIONAMOS VARIABLES POTENCIALES PARA EL MODELO----------------------------------------

train<-train%>%
  select(price, bedrooms, surface_covered, property_type, rooms, distancia_parque, distancia_comercial, distancia_avenida_principal, distancia_universidad)
test<-test%>% 
  select(property_id, price, bedrooms, surface_covered, rooms, property_type, distancia_parque, distancia_comercial, distancia_avenida_principal, distancia_universidad)

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

arbol_1<-train(price ~ bedrooms + surface_covered + rooms + tipo + distancia_parque+ distancia_comercial+ distancia_avenida_principal +
                         distancia_universidad,
                       data=train_muestra,
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


rforest<-train(price ~ .,
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

write_csv(test_k, file="Bogota_kaggle_R1003F.csv")


####BOOSTING-------------------------------------------------------------------

#Instalar h2o
if ("package:h2o" %in% search()) {detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) {remove.packages("h2o")}
library(pacman)
p_load("RCurl","jsonlite") 
install.packages("h2o", type = "source", 
                repos = (c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))

library(h2o)

tunegrid_gbm <- expand.grid(learn_rate = c(0.1, 0.01, 0.001), #tasa a la que se actualizan los parámetros entre 0 y 1
                            ntrees= c(30,50),
                            max_depth = 2,
                            col_sample_rate= 1,
                            min_rows = 100)

h2o.init(nthreads = 5)
library(parallel)
detectCores()

modelo_boosting<- train(price ~ bedrooms + property_type + distancia_parque+ distancia_comercial+ distancia_avenida_principal + distancia_universidad,
                     data=train_muestra, 
                     trControl = cv8,
                     metric = "RMSE",
                     tuneGrid = tunegrid_gbm,
                     method ="gbm_h2o")


#### OLS - ELASTICNET------------------------------------------------------------
##ALPHA Y LAMBDA
##OLS---------------------------------------
p_load("caret")

set.seed(123)
fitControl <- trainControl(## 8-fold CV
  method = "cv",
  number = 10)


fmla<-formula(price ~ bedrooms + property_type + distancia_parque+            
                distancia_comercial+ distancia_avenida_principal + distancia_universidad)

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



##ELASTIC NET---------------------------------------------------


EN<-train(fmla,
          data=train_muestra,
          method = 'glmnet', 
          trControl = fitControl,
          tuneGrid = expand.grid(alpha = seq(0,1,by = 0.01), #grilla de alpha
                                 lambda = seq(0.001,0.02,by = 0.001)),
          preProcess = c("center", "scale")
) 

EN$bestTune


coef_EN<-coef(EN$finalModel,EN$bestTune$lambda)
coef_EN

pred_EN_train_muestra<-predict(EN, newdata=train_muestra)
pred_EN_test_muestra<-predict(EN, newdata=test_muestra)

##EN MUESTRA
#Error en porcentaje
MAPE(y_pred=pred_EN_train_muestra, y_true = train_muestra$price)

#Error promedio
MAE(y_pred=pred_EN_train_muestra, y_true = train_muestra$price)

## FUERA DE MUESTRA
#Error en porcentaje
MAPE(y_pred=pred_EN_test_muestra, y_true = test_muestra$price)

#Error promedio
MAE(y_pred=pred_EN_test_muestra, y_true = test_muestra$price)

-------------------------------------------------------------------
coefs_lineal<-cbind(coef(linear_reg$finalModel), as.matrix(coef_EN))
colnames(coefs_lineal)<-c("OLS","ELASTIC_NET")
round(coefs_lineal,4)












###IMPUTACIONES BASE TRAIN--------------------------------------------------

##CON MEDIA--------------------------------
install.packages("mice", dependencies = TRUE)
library(mice)

columns <- c("surface_covered", "rooms")
imputed_data <- mice(train[,names(train) %in% columns],m = 1,
                     maxit = 1, method = "mean",seed = 2018,print=F)
complete.data <- mice::complete(imputed_data)


##ESTOCASTICA------------------------------

imputed_data1 <- mice(train$surface_covered,m = 1,
                      maxit = 20, method = "norm.nob",seed = 2018,print=F)
complete.data1 <- mice::complete(imputed_data1)

xyplot(imputed_data1,rooms ~ surface_covered)

##MEDIANTE REGRESION------------------------

columns <- c("surface_covered", "rooms")
imputed_data <- mice(train[,names(train) %in% columns],m = 1,
                     maxit = 1, method = "norm.predict",seed = 2018,print=F)
train <- train%>%
  mutate(mice::complete(imputed_data))

xyplot(impute_arg1,rooms ~ surface_covered)



rooms_i<-complete.data%>%
  select(rooms)

surface_covered_i<-complete.data%>%
  select(surface_covered)

train<-train%>%
  mutate(surface_imp=surface_covered, newdata=surface_covered_i)

train<-train%>%
  mutate(rooms_imp=rooms, newdata=rooms_i)

train<-data.frame(train)

summary(train)
colnames(train)

train<-train%>%
  rename(rooms_i = newdata.rooms)


##BASE ORIGINAL-------------------------------
xyplot(rooms ~ surface_covered, data=train )



##RENOMBRAMOS LAS VARIABLES EN ESPAÑOL
train<-train%>%
  rename(precio=price,
         mes=month,
         banios=bathrooms,
         anio=year,
  )

#Estadísticas descriptivas

#Gráfica Precios
ggplot(train_final, aes(x = price)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() +
  labs(x = "Precio casas", y = "")

p_load(gridExtra)

#Bedrooms - Precio

ggplot(train_final, aes(x=bed_f)) + 
  geom_bar(aes(x=Pobre), fill="steelblue")+
  labs(title = "Personas en Situación de Pobreza")+
  scale_x_discrete(limit=c(0,1), labels = c("No Pobre", "Pobre"))

ggplot(train_final, aes(x=price, fill=bed_f)) + 
  geom_density()+
  labs(title = "Precio - Cuartos")

#Estadísticas descriptivas

#Gráfica Precios
ggplot(train_final, aes(x = price)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() +
  labs(x = "Precio casas", y = "")

ggplot(train_final, aes(x = bedrooms)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(x = "Cuartos", y = "")+
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.1) # Más de 6 c

##VARIABLES CONTINUAS

##Precios
ggplot(train, aes(x = surface_covered)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() 

##Cuartos
ggplot(train, aes(x=bedrooms)) + 
  geom_bar(aes(x=bedrooms), fill="steelblue")+
  labs(title = "Cuartos",
       x="Número de cuartos")

##Area cubierta
ggplot(train, aes(x = distancia_parque)) +
  geom_histogram(fill = "#f4a261") +
  theme_bw()


##CORRELACION PRECIO Y VARIABLES DE DISTANCIA
library(GGally)
ggpairs(train, columns=c(1,6,7,8,9),
        ggplot2::aes(colour = tipo))

##CORRELACION VARIABLES CARACTERISTICAS DE LA CASA
ggpairs(train, columns=c(1,3,5),
        ggplot2::aes(colour = tipo))


p_load(corrplot)
train_dist<-train%>%
  select(price, distancia_parque, distancia_comercial, distancia_avenida_principal,
         distancia_universidad)
corrplot(cor(train_dist), type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


train %>% 
  ggplot(mapping = aes(price, surface_covered)) +
  geom_point() 

