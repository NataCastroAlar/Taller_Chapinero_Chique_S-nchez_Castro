rm(list = ls())

require("pacman")
p_load("tidyverse","sf","geojsonio")
p_load("leaflet")
p_load("skimr")
library(readr)

train_sin_outliers <- read_csv("/Data/train_sin_outliers.csv")
write.csv(train_sin_outliers, file="train_sin_outliers.csv", row.names = F)

n <- 500
indices_aleatorios <- sample(1:nrow(train_sin_outliers), n)
df_sub <- train_sin_outliers[indices_aleatorios, ]

##-----------Superlearner--------##

bd_sub<- df_sub %>% mutate(logprice=log(price))

p_load("caret")
p_load("SuperLearner")

set.seed(1011)
inTrain <- createDataPartition(
  y = bd_sub$logprice,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

train_sub <- bd_sub[ inTrain,]
test_sub  <- bd_sub[-inTrain,]

#Modelos disponibles
listWrappers()

ySL<- train_sub$price
XSL<- train_sub  %>% select(area_tot, lat, lon, distancia_universidad, area_tot_imp,
                             distancia_avenida_principal, bathrooms, bathrooms_imp, property_type,  
                             distancia_parque, rooms_tot)

sl.lib <- c("SL.randomForest", "SL.lm") #lista de los algoritmos a correr

# Fit using the SuperLearner package, 
install.packages("randomForest")

fitY <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

fitY

test_sub <- test_sub  %>%  mutate(yhat_Sup=predict(fitY, newdata = data.frame(test_sub), onlySL = T)$pred)
head(test_sub$yhat_Sup)


MAE_S4 <- with(test_sub,mean(abs(price-yhat_Sup))) #MAE
MAE_S4

