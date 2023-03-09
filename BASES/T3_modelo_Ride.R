
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
       stargazer # Report LM model
       )

#-------------Importamos datos con informacion espacial-----------------
setwd("C:/Users/User/Desktop/Dulio/Big Data y Machine Learning/taller 3")

Bogota_train <- read_csv("Bogota_train.csv")

Bogota_test <- read_csv("Bogota_test.csv")

submission_template <- read_csv("submission_template.csv")

submission_template_profe <- read_csv("submission_template_profe.csv")

p_load(skimr)
skim(Bogota_train)
skim(Bogota_test)


# Imputando datos NA con KNN

library(ggplot2)

p_load(VIM)

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
p1

# Quitando outliers

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

# Graficos de dos variables

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

# RIDGE
train <- Bogota_train %>%
  select(price, property_type, rooms_tot, bathrooms, distancia_avenida_principal,
         distancia_comercial, distancia_parque, distancia_universidad)

test <- Bogota_test %>%
  select(property_type, rooms_tot, bathrooms, distancia_avenida_principal,
         distancia_comercial, distancia_parque, distancia_universidad)

x_train <- model.matrix(price~., data = train)[, -1]
y_train <- train$price

x_test <- model.matrix(~., data = test)[, -1]


# Modelo Ridge (alpha=0)
# Si no se especifica el valor de lambda, se selecciona un rango automático.

library(glmnet)

modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  nlambda     = 100,
  standardize = TRUE
)

# Evolución de los coeficientes en función de lambda

regularizacion <- modelo$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

# Evolución del error en función de lambda
# ==============================================================================
set.seed(123)
cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)

# Mejor valor lambda encontrado
# ==============================================================================
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)

# Mejor valor lambda encontrado + 1sd
# ==============================================================================
# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)

# Mejor modelo lambda óptimo + 1sd
# ==============================================================================
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

# Coeficientes del modelo
# ==============================================================================
df_coeficientes <- coef(modelo) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

# Predicciones de entrenamiento
# ==============================================================================
predicciones_train <- predict(modelo, newx = x_train)

# MSE de entrenamiento
# ==============================================================================
training_mse <- mean((predicciones_train - y_train)^2)
paste("Error (mse) de entrenamiento:", training_mse)

# Predicciones de test
# ==============================================================================
y_hat_ridge <- predict(modelo, newx = x_test)

# Sacamos el dato

y_hat_ridge <- as.data.frame(y_hat_ridge)

submission_template <- select(submission_template, -price)

submission_template <- cbind(submission_template, y_hat_ridge)

submission_template <- submission_template %>%
  mutate(price =s0)

submission_template <- select(submission_template, -s0)

p3 <- ggplot(submission_template, aes(x = price)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "precio en pesos ($)", y = "Cantidad",
       title = "Distribución del precio") +
  theme_bw()
p3

summary(submission_template$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

# Datos del profesor

p2 <- ggplot(submission_template_profe, aes(x = price)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "precio en pesos ($)", y = "Cantidad",
       title = "Distribución del precio") +
  theme_bw()
p2

summary(submission_template_profe$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

# ----------

write.csv(submission_template, file="submission_template2.csv",  row.names = F)


