#Librerias que usaremos 
library(lubridate)
library(jsonlite)
library(tidyverse)
library(httr)

# Indicamos el directorio de trabajo (debes indicarlo tú, está vacío por defecto)
wd <- ""

setwd(wd)

#Descargamos los datos 
url <- GET("https://www.valencia.es/web/guest/valenciaalminut/calidadaireNO2.cors")

#Los seleccionamos
datos <- content(url, "text", encoding = "UTF-8")
sensores <- fromJSON(datos)


#Nombres de los sensores
nombres_sensores <- names(sensores[[1]])

#Variables que usaremos
dias <- 90 #Dias de los que queremos datos

#Fechas de los sensores 
fechas <- tail(sensores$fechas, dias)

#Creamos un dataframe donde guardaremos los datos
sensor_global <- data.frame(Fechas = fechas)

#Guardamos los datos para cada sensor
for (i in 1:length(sensores[[1]])) {
  
  #Datos cada sensor
  sensor <- data.frame(sensores[[1]][nombres_sensores[i]])
  
  #Nos quedamos con los de los ultimos 90 dias
  sensor <- tail(sensor,dias)
  
  sensor_global <- cbind(sensor_global, sensor)
  
  rm(sensor) #Eliminamos los sensores
  
}

#Asignamos los nombres
colnames(sensor_global) <- c("Fechas", nombres_sensores)

#Ponemos el tipo de dato correcto
sensor_global$Fechas <- dmy(sensor_global$Fechas)