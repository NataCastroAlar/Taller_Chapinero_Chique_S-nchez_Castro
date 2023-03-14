rm(list = ls())

require("pacman")
p_load("tidyverse","sf","geojsonio")
#p_load("leaflet")
#p_load("skimr")
#library(readr)

train_sin_outliers <- read_csv("train_sin_outliers.csv")
#write.csv(train_sin_outliers, file="train_sin_outliers.csv", row.names = F)

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
#install.packages("randomForest")

fitY <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

fitY




test_sub <- test_sub  %>%  mutate(yhat_Sup=predict(fitY, newdata = data.frame(test_sub), onlySL = T)$pred)
head(test_sub$yhat_Sup)


MAE_S4 <- with(test_sub,mean(abs(price-yhat_Sup))) #MAE
MAE_S4


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



