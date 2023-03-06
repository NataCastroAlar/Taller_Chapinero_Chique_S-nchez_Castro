################################
###GRUPO 6 CHIQUE-SANCHEZ-CASTRO
#################################
#OBTENCION DE GRAFICAS Y DATOS ESPACIALES
#####################################

rm(list = ls())

# Librerias

library(pacman)

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

train <- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/DATOS BOGOTA/train.csv")

glimpse(train)


# Eliminamos las observaciones que no tienen información de latitud o longitud
filtro <- is.na(train$lat) | is.na(train$lon)
train <- train[!filtro, ]

# Observaciones sólo en Bogotá
limites <- getbb("Bogota Colombia")

##PUNTOS DE LA BASE DE DATOS EN EL MAPA--------------------------------------

leaflet()%>%
  addTiles()%>%
  addCircles(lng=train$lon,
             lat=train$lat)


##Limitamos sólo para Bogotá
filtro1 <- between(train$lon, limites[1, "min"], limites[1, "max"])
filtro2 <- between(train$lat, limites[2, "min"], limites[2, "max"])

train <- train[(filtro1 & filtro2),] 

#Mapa con número de cuartos resaltados

table(train$rooms)
train$rooms[!(train$rooms %in% as.character(1:10))] = NA

color<-rep(NA, nrow(train))
color[train$rooms == "1"] <- "#7400b8"
  color[train$rooms == "2"] <- "#6930c3"
    color[train$rooms == "3"] <- "#5e60ce"
      color[train$rooms == "4"] <- "#5390d9"
        color[train$rooms == "5"] <- "#4ea8de"
          color[train$rooms == "6"] <- "#48bfe3"
            color[train$rooms == "7"] <- "#56cfe1"
              color[train$rooms == "8"] <- "#64dfdf"
                color[train$rooms == "9"] <- "#72efdd"
                  color[train$rooms == "10"] <- "#80ffdb"
                    
leaflet()%>%
  addTiles()%>%
  addCircles(lng=train$lon,
             lat=train$lat,
             col = color)

##Añadiendo precio en html

leaflet()%>%
  addTiles()%>%
  addCircles(lng=train$lon,
             lat=train$lat,
             col = train$price)

## DISTANCIA A PARQUES-------------------------------------------------------------

available_tags("leisure")
parques <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "leisure", value = "park")

#librería sf:
parques_sf <- osmdata_sf(parques)
parques_geometria <- parques_sf$osm_polygons%>%
  select (osm_id, name)
                  
#En el mapa parques y apartamentos por número de cuartos

leaflet()%>%
  addTiles()%>%
  addPolygons(data = parques_geometria, col = "#007200",
              opacity = 0.8, popup= parques_geometria)%>%
  addCircles(lng=train$lon,
             lat=train$lat,
             col = color)

#Distancia a parques utilziando centroides

centroides<- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T)

class(train)

#Convertimos la base de datos en datos espaciales

train_sf <- st_as_sf(train, coords=c("lon", "lat"))
st_crs(train_sf) <- 4326
class (train_sf)

#Centroides en matriz
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))
                          
dist_matrix <-st_distance(x=train_sf, centroides_sf)

dis_min_parque<-apply(dist_matrix, 1, min)
train$distancia_parque <- dis_min_parque


##DISTANCIA A AVENIDAS MAS CERCANAS---------------------------------

train<-st_as_sf(train,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object

avenidas <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "highway", value = "secondary")

avenidas_sf <- osmdata_sf(avenidas)

avenidas_geometria <- avenidas_sf$osm_lines%>%
  select (osm_id, name)

leaflet()%>%
  addTiles()%>%
  addPolygons(data = avenidas_geometria, col = "#F72585",
              opacity = 0.8, popup= avenidas_geometria)%>%
  addCircles(data=train)

#Busco la geometría más cercana
cercano <- st_nearest_feature(train,avenidas_geometria)
#calculo la distancia
dist <-st_distance(train, avenidas_geometria[cercano,], by_element=TRUE)
dist
train$distancia_avenida_principal<-dist


##DISTANCIA A CENTRO COMERCIAL O AREA COMERCIAL----------------------------------------------


available_tags("building")
comercial <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "building", value = "commercial")

#librería sf:
comercial_sf <- osmdata_sf(comercial)
comercial_geometria <- comercial_sf$osm_points%>%
  select (osm_id)

#En el mapa supermercados y apartamentos por número de cuartos

leaflet()%>%
  addTiles()%>%
  addCircles(data = comercial_geometria, col = "#c9184a",
              opacity = 0.8, popup= comercial_geometria)%>%
  addCircles(lng=train$lon,
             lat=train$lat,
             col = "#0a9396")


#Convierto base train en objeto sf:
train<-st_as_sf(train,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object

#Busco la geometría comercial más cercana:
cercano_com <- st_nearest_feature(train,comercial_geometria)
#calculo la distancia
dist_com <-st_distance(train, comercial_geometria[cercano_com,], by_element=TRUE)
dist_com
train$distancia_comercial<-dist_com

###DISTANCIA A UNIVERSIDADES-------------------------------------


available_tags("building")
universidad <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "building", value = "university")

#librería sf:
universidad_sf <- osmdata_sf(universidad)
universidad_geometria <- universidad_sf$osm_polygons%>%
  select (osm_id, name)

#Mapa Universidades

leaflet()%>%
  addTiles()%>%
  addPolygons(data = universidad_geometria, col = "#ffff3f",
             opacity = 0.8, popup= universidad_geometria)%>%
  addCircles(lng=train$lon,
             lat=train$lat,
             col = "#0a9396")


#Convierto base train en objeto sf:
train<-st_as_sf(train,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object

#Busco la geometría comercial más cercana:
cercano_uni <- st_nearest_feature(train,universidad_geometria)
#calculo la distancia
dist_uni <-st_distance(train, universidad_geometria[cercano_uni,], by_element=TRUE)
dist_uni
train$distancia_universidad<-dist_uni


##-------------------------------------------------------------------------
##-------------------------------------------------------------------------
##PARA LA BASE DE TESTEO
##-------------------------------------------------------------------------
##-------------------------------------------------------------------------

test<- read_csv("Documents/UNIANDES BIG DATA MACHINE LEARNING/DATOS BOGOTA/test.csv")

glimpse(test)


# Eliminamos las observaciones que no tienen información de latitud o longitud
filtro <- is.na(test$lat) | is.na(test$lon)
test <- test[!filtro, ]

# Observaciones sólo en Bogotá
limites <- getbb("Bogota Colombia")

##Limitamos sólo para Bogotá
filtro1 <- between(test$lon, limites[1, "min"], limites[1, "max"])
filtro2 <- between(test$lat, limites[2, "min"], limites[2, "max"])

test <- test[(filtro1 & filtro2),] 


## TEST - DISTANCIA A PARQUES-------------------------------------------------------------

parques <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "leisure", value = "park")

#librería sf:
parques_sf <- osmdata_sf(parques)
parques_geometria <- parques_sf$osm_polygons%>%
  select (osm_id, name)

#En el mapa parques y apartamentos por número de cuartos

leaflet()%>%
  addTiles()%>%
  addPolygons(data = parques_geometria, col = "#007200",
              opacity = 0.8, popup= parques_geometria)%>%
  addCircles(lng=train$lon,
             lat=train$lat,
             col = color)

#Distancia a parques utilziando centroides

centroides<- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T)

class(test)

#Convertimos la base de datos en datos espaciales

test_sf <- st_as_sf(test, coords=c("lon", "lat"))
st_crs(test_sf) <- 4326
class (test_sf)

#Centroides en matriz
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))

dist_matrix <-st_distance(x=test_sf, centroides_sf)

dis_min_parque<-apply(dist_matrix, 1, min)
test$distancia_parque <- dis_min_parque


##TEST - DISTANCIA A AVENIDAS MAS CERCANAS---------------------------------

test<-st_as_sf(test,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object

avenidas <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "highway", value = "secondary")

avenidas_sf <- osmdata_sf(avenidas)

avenidas_geometria <- avenidas_sf$osm_lines%>%
  select (osm_id, name)

leaflet()%>%
  addTiles()%>%
  addPolygons(data = avenidas_geometria, col = "#F72585",
              opacity = 0.8, popup= avenidas_geometria)%>%
  addCircles(data=test)

#Busco la geometría más cercana
cercano_test <- st_nearest_feature(test,avenidas_geometria)
#calculo la distancia
dist_test <-st_distance(test, avenidas_geometria[cercano_test,], by_element=TRUE)
dist_test
test$distancia_avenida_principal<-dist_test


##TEST - DISTANCIA A CENTRO COMERCIAL O AREA COMERCIAL----------------------------------------------

available_tags("building")
comercial_test <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "building", value = "commercial")

#librería sf:
comercial_sf <- osmdata_sf(comercial_test)
comercial_geometria <- comercial_sf$osm_points%>%
  select (osm_id)

#En el mapa supermercados y apartamentos por número de cuartos

leaflet()%>%
  addTiles()%>%
  addCircles(data = comercial_geometria, col = "#c9184a",
             opacity = 0.8, popup= comercial_geometria)%>%
  addCircles(lng=test$lon,
             lat=test$lat,
             col = "#0a9396")


#Convierto base train en objeto sf:
test<-st_as_sf(test,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object

#Busco la geometría comercial más cercana:
cercano_com <- st_nearest_feature(test,comercial_geometria)
#calculo la distancia
dist_com <-st_distance(test, comercial_geometria[cercano_com,], by_element=TRUE)
dist_com
test$distancia_comercial<-dist_com

###TEST- DISTANCIA A UNIVERSIDADES-------------------------------------
                  

available_tags("building")
universidad <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "building", value = "university")

#librería sf:
universidad_sf <- osmdata_sf(universidad)
universidad_geometria <- universidad_sf$osm_polygons%>%
  select (osm_id, name)

#Mapa Universidades

leaflet()%>%
  addTiles()%>%
  addPolygons(data = universidad_geometria, col = "#ffff3f",
              opacity = 0.8, popup= universidad_geometria)%>%
  addCircles(lng=test$lon,
             lat=test$lat,
             col = "#0a9396")


#Convierto base train en objeto sf:
test<-st_as_sf(test,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object

#Busco la geometría comercial más cercana:
cercano_uni <- st_nearest_feature(test,universidad_geometria)
#calculo la distancia
dist_uni <-st_distance(test, universidad_geometria[cercano_uni,], by_element=TRUE)
dist_uni
test$distancia_universidad<-dist_uni


