
################################################
#                                              #
# Taller Nro 3 / MODELOS                       #
#                                              #
################################################

# Limpiar todo

rm(list = ls())

library(readr)

library(pacman, caret) 
p_load(imputeMissings)  # Para imputar missing values
# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rstudioapi, # Get the location of this script
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       stargazer, # Report LM models
       osmdata) # Get OSM's data 

#-------------Importamos datos con informacion espacial-----------------

setwd("C:/Users/User/Desktop/Dulio/Big Data y Machine Learning/taller 3")

Bogota_train <- read_csv("Bogota_train.csv")

Bogota_test <- read_csv("Bogota_test.csv")

submission_template <- read_csv("submission_template.csv")

submission_template_profe <- read_csv("submission_template_profe.csv")

p_load(skimr)
skim(Bogota_train)
skim(Bogota_test)

#------------------Extrayendo area--------------------------------------

# Bogota train:

library(stringr)

Metros <- str_extract(Bogota_train$description, "\\d+\\s*(mts|m2|metros)")

Area_sin_texto <- gsub("m2", "", Metros)
Area_sin_texto <- gsub("[[:alpha:]]", "", Area_sin_texto)
as.numeric(Area_sin_texto)

Bogota_train <- cbind(Bogota_train, area = Area_sin_texto)

sapply(Bogota_train, function(x) sum(is.na(x)))

Bogota_train$Superficie <- ifelse(is.na(Bogota_train$surface_covered), Bogota_train$area, Bogota_train$surface_covered)

Bogota_train$Superficie <- as.numeric(Bogota_train$Superficie)

sapply(Bogota_train, function(x) sum(is.na(x)))

summary(Bogota_train$Superficie)

# Limpiar los valores muy altos

Bogota_train <- Bogota_train %>%
  filter(Superficie <= 500 & Superficie >= 30)

#imputamos con KNN

p_load(VIM)

Bogota_train <-  kNN(Bogota_train, variable = c("Superficie"), k = 6)

Bogota_train$Superficie <- round(Bogota_train$Superficie,0)
summary(Bogota_train$Superficie)

p2 <- ggplot(Bogota_train, aes(x = Superficie)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Area (m2)", y = "Cantidad",
       title = "Distribución del area") +
  theme_bw()
p2

# Bogota test:

Metros <- str_extract(Bogota_test$description, "\\d+\\s*(mts|m2|metros)")

Area_sin_texto <- gsub("m2", "", Metros)
Area_sin_texto <- gsub("[[:alpha:]]", "", Area_sin_texto)
as.numeric(Area_sin_texto)

Bogota_test <- cbind(Bogota_test, area = Area_sin_texto)

sapply(Bogota_test, function(x) sum(is.na(x)))

Bogota_test$Superficie <- ifelse(is.na(Bogota_test$surface_covered), Bogota_test$area, Bogota_test$surface_covered)

Bogota_test$Superficie <- as.numeric(Bogota_test$Superficie)

sapply(Bogota_test, function(x) sum(is.na(x)))

summary(Bogota_test$Superficie)

#imputamos con KNN

#p_load(VIM)
Bogota_test$Superficie <- ifelse(Bogota_test$Superficie >= 500, mean(Bogota_test$Superficie, na.rm = TRUE), Bogota_test$Superficie)
Bogota_test$Superficie <- ifelse(Bogota_test$Superficie <= 30, mean(Bogota_test$Superficie, na.rm = TRUE), Bogota_test$Superficie)

Bogota_test <-  kNN(Bogota_test, variable = c("Superficie"), k = 6)

Bogota_test$Superficie <- round(Bogota_test$Superficie,0)
summary(Bogota_test$Superficie)

# Limpiar los valores muy altos

Bogota_test$Superficie <- ifelse(Bogota_test$Superficie >= 500, mean(Bogota_test$Superficie, na.rm = TRUE), Bogota_test$Superficie)
Bogota_test$Superficie <- ifelse(Bogota_test$Superficie <= 30, mean(Bogota_test$Superficie, na.rm = TRUE), Bogota_test$Superficie)

p3 <- ggplot(Bogota_test, aes(x = Superficie)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Area (m2)", y = "Cantidad",
       title = "Distribución del area") +
  theme_bw()
p3


#------------------- Imputando datos NA con KNN -----------------------

library(ggplot2)

#p_load(VIM)

Bogota_train <-  kNN(Bogota_train, variable = c("bathrooms"), k = 6)
Bogota_train$bathrooms <- round(Bogota_train$bathrooms,0)
summary(Bogota_train$bathrooms)

Bogota_test <-  kNN(Bogota_test, variable = c("bathrooms"), k = 6)
Bogota_test$bathrooms <- round(Bogota_test$bathrooms,0)
summary(Bogota_test$bathrooms)

# Generando variables

Bogota_train$rooms[is.na(Bogota_train$rooms)] <- 0
Bogota_train$rooms_tot <- apply(Bogota_train[, c("rooms", "bedrooms")], 1, max)

Bogota_test$rooms[is.na(Bogota_test$rooms)] <- 0
Bogota_test$rooms_tot <- apply(Bogota_test[, c("rooms", "bedrooms")], 1, max)

sapply(Bogota_train, function(x) sum(is.na(x)))
sapply(Bogota_test, function(x) sum(is.na(x)))

#--------------Analisis de datos----------------------------------------

# Distribución del precio 
summary(Bogota_train$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

# Graficos de distribucion del precio

ggplot(Bogota_train, aes(x = 0, y = price)) + 
  geom_boxplot()+
  theme_bw()


p1 <- ggplot(Bogota_train, aes(x = price)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "precio en pesos ($)", y = "Cantidad",
       title = "Distribución del precio") +
  theme_bw()
ggplotly(p1)

# Quitando outlier

# En train

g_price<-boxplot(Bogota_train$price, col="skyblue", frame.plot=F)
g_price$out
Bogota_train<-Bogota_train[!(Bogota_train$price %in% g_price$out),]

g_room<-boxplot(Bogota_train$rooms_tot, col="skyblue", frame.plot=F)
g_room$out
Bogota_train<-Bogota_train[!(Bogota_train$rooms_tot %in% g_room$out),]

g_bathrooms<-boxplot(Bogota_train$bathrooms, col="skyblue", frame.plot=F)
g_bathrooms$out
Bogota_train<-Bogota_train[!(Bogota_train$bathrooms %in% g_bathrooms$out),]


g_parque<-boxplot(Bogota_train$distancia_parque, col="skyblue", frame.plot=F)
g_parque$out
Bogota_train<-Bogota_train[!(Bogota_train$distancia_parque %in% g_parque$out),]

g_av_prin<-boxplot(Bogota_train$distancia_avenida_principal, col="skyblue", frame.plot=F)
g_av_prin$out
Bogota_train<-Bogota_train[!(Bogota_train$distancia_avenida_principal %in% g_av_prin$out),]

g_comercial<-boxplot(Bogota_train$distancia_comercial, col="skyblue", frame.plot=F)
g_comercial$out
Bogota_train<-Bogota_train[!(Bogota_train$distancia_comercial %in% g_comercial$out),]

g_universidad<-boxplot(Bogota_train$distancia_universidad, col="skyblue", frame.plot=F)
g_universidad$out
Bogota_train<-Bogota_train[!(Bogota_train$distancia_universidad %in% g_universidad$out),]

g_price<-boxplot(Bogota_train$price, col="skyblue", frame.plot=F)
g_price$out
Bogota_train<-Bogota_train[!(Bogota_train$price %in% g_price$out),]

g_room<-boxplot(Bogota_train$rooms_tot, col="skyblue", frame.plot=F)
g_room$out
Bogota_train<-Bogota_train[!(Bogota_train$rooms_tot %in% g_room$out),]

g_bathrooms<-boxplot(Bogota_train$bathrooms, col="skyblue", frame.plot=F)
g_bathrooms$out
Bogota_train<-Bogota_train[!(Bogota_train$bathrooms %in% g_bathrooms$out),]

#------------------- Graficos de dos variables---------------------------

g0 <- ggplot(data = Bogota_train, mapping = aes(x = Superficie , y= price))+
  geom_point(col = "darkblue" , size = 0.9)+
  labs(x = "Area (m2)",
       y = "Precio ($)",
       title = "Relacion Precio - Area")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g0

g1 <- ggplot(data = Bogota_train, mapping = aes(x = distancia_parque , y= price))+
  geom_point(col = "darkblue" , size = 0.9)+
  labs(x = "Distancia la parque (metros)",
       y = "Precio ($)",
       title = "Relacion Precio - Distancia al Parque")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g1

g2 <- ggplot(data = Bogota_train, mapping = aes(x = distancia_avenida_principal , y= price))+
  geom_point(col = "darkblue" , size = 0.9)+
  labs(x = "Distancia a Av. principal (metros)",
       y = "Precio ($)",
       title = "Relacion Precio - Distancia a Av. Principal")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g2

g3 <- ggplot(data = Bogota_train, mapping = aes(x = distancia_comercial , y= price))+
  geom_point(col = "darkblue" , size = 0.9)+
  labs(x = "Distancia a centro comercial (metros)",
       y = "Precio ($)",
       title = "Relacion Precio - Distancia a Centro Comercial")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g3

g4 <- ggplot(data = Bogota_train, mapping = aes(x = distancia_universidad , y= price))+
  geom_point(col = "darkblue" , size = 0.9)+
  labs(x = "Distancia a univeridad (metros)",
       y = "Precio ($)",
       title = "Relacion Precio - Distancia a Universidad")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g4

p_load(grid, gridExtra)

grid.arrange(g1,g2,g3,g4, nrow = 2, widths = c(1,1.5))


#------------------------ Modelos-----------------------------------------

modelo_reg <- lm(price ~ property_type + Superficie + bedrooms + bathrooms + distancia_parque +
                   distancia_comercial + distancia_avenida_principal + distancia_universidad, data = Bogota_train)
summary(modelo_reg)

modelo_reg1 <- lm(price ~ property_type + Superficie + rooms_tot + bathrooms + distancia_parque + 
                   distancia_comercial + distancia_avenida_principal + distancia_universidad, data = Bogota_train)
summary(modelo_reg1)

# Modelos de Regularizacion

## ------- Elejimos los Hiper parametros -----------------

# Minimos Cuadrados Ordinario (OLS)

p_load(caret)

set.seed(123)
fitControl <- trainControl(## 5-fold CV, 10 better
  method = "cv",
  number = 10)

fmla<-formula(price ~ property_type + Superficie + rooms_tot + bathrooms + distancia_parque + 
                distancia_comercial + distancia_avenida_principal + distancia_universidad)

linear_reg<-train(fmla,
                  data=Bogota_train,
                  method = 'lm', 
                  trControl = fitControl,
                  preProcess = c("center", "scale")
) 


linear_reg
summary(linear_reg)

y_hat_reg <- predict(linear_reg, newdata = Bogota_test)

#------ Modelo Ridge --------------------------#

ridge<-train(fmla,
             data=Bogota_train,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 0, #Ridge
                                    lambda = seq(10000000, 20000000,by = 10000)),
             preProcess = c("center", "scale")
) 

plot(ridge$results$lambda,
     ridge$results$RMSE,
     xlab="lambda",
     ylab="Root Mean-Squared Error (RMSE)"
)

ridge$bestTune

coef_ridge<-coef(ridge$finalModel, ridge$bestTune$lambda)
coef_ridge

modelo_ridge<-train(fmla,
                    data=Bogota_train,
                    method = 'glmnet', 
                    trControl = fitControl,
                    tuneGrid = expand.grid(alpha = 0, #Ridge
                                           lambda = 14880000),
                    preProcess = c("center", "scale")
) 

y_hat_ridge <- predict(modelo_ridge, newdata = Bogota_test)

## Modelo Lasso

lasso<-train(fmla,
             data=Bogota_train,
             method = 'glmnet', 
             trControl = fitControl,
             tuneGrid = expand.grid(alpha = 1, #lasso
                                    lambda = seq(10000,1000000,by = 1000)),
             preProcess = c("center", "scale")
) 

plot(lasso$results$lambda,
     lasso$results$RMSE,
     xlab="lambda",
     ylab="Root Mean-Squared Error (RMSE) Lasso"
)

lasso$bestTune

coef_lasso<-coef(lasso$finalModel, lasso$bestTune$lambda)
coef_lasso

modelo_lasso<-train(fmla,
                    data=Bogota_train,
                    method = 'glmnet', 
                    trControl = fitControl,
                    tuneGrid = expand.grid(alpha = 1, #lasso
                                           lambda = 320000),
                    preProcess = c("center", "scale")
) 

y_hat_lasso <- predict(modelo_lasso, newdata = Bogota_test)

## Elastic Net

EN<-train(fmla,
          data=Bogota_train,
          method = 'glmnet', 
          trControl = fitControl,
          tuneGrid = expand.grid(alpha = seq(0,1,by = 0.1), #grilla de alpha
                                 lambda = seq(100000,10000000,by = 10000)),
          preProcess = c("center", "scale")
) 

EN$bestTune

coef_EN<-coef(EN$finalModel,EN$bestTune$lambda)
coef_EN

modelo_EN<-train(fmla,
                 data=Bogota_train,
                 method = 'glmnet', 
                 trControl = fitControl,
                 tuneGrid = expand.grid(alpha = 0.9, #grilla de alpha
                                        lambda = 320000),
                 preProcess = c("center", "scale")
) 

y_hat_EN <- predict(modelo_EN, newdata = Bogota_test)

## Tabla: Coeficientes de los modelos

coefs_df<-cbind(coef(linear_reg$finalModel),as.matrix(coef_ridge),as.matrix(coef_lasso),as.matrix(coef_EN))
colnames(coefs_df)<-c("OLS","RIDGE","LASSO","ELASTIC_NET")
round(coefs_df,4)

RMSE_df<-cbind(linear_reg$results$RMSE,ridge$results$RMSE[which.min(ridge$results$lambda)],lasso$results$RMSE[which.min(lasso$results$lambda)],EN$results$RMSE[which.min(EN$results$lambda)])
colnames(RMSE_df)<-c("OLS","RIDGE","LASSO","EN")
RMSE_df

# Para enviar valores de prediccion

# y_hat_reg
# y_hat_ridge
# y_hat_lasso
# y_hat_EN

price <- y_hat_lasso
submission_template <- select(submission_template, -price)
submission_template <- cbind(submission_template, price)
names(submission_template)[ncol(submission_template)] <- "price"

p4 <- ggplot(submission_template, aes(x = price)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "precio en pesos ($)", y = "Cantidad",
       title = "Distribución del precio") +
  theme_bw()
p4

summary(submission_template$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))


# ------------- Exportando archivo ------------------------

write.csv(submission_template, file="submission_template1.csv",  row.names = F)
submission_template1 <- read_csv("submission_template1.csv")

