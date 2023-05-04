#Cargar las librerias
library(lubridate)
library(jsonlite)
library(tidyverse)
library(readr)


# Descargar los datos y convertirlos en una cadena de texto
url <- "https://valencia.opendatasoft.com/api/explore/v2.1/catalog/datasets/rvvcca/exports/csv?lang=es&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"

#Guardarnos la variable
datos_diarios <- read.csv2(url, sep = ";")

# Cargamos los datos acumulados 
path <- "./data/datos_diarios.RData"

# Guardamos los datos 
save(datos_diarios, file = path)

datos_diarios_clean <- datos_diarios%>% select(-c("Fecha.baja", "NH3","Ruido", "Humidad.relativa", "Radiacion.solar", 
                                                   "Direccion.del.viento", "Velocidad.del.viento", "Velocidad.maxima.del.viento",
                                                   "Precipitacion", "Temperatura"))

# En el data frame resultante unifica las denominaciones del fabricante `AIRBUS` bajo la denominación `"AIRBUS COMP"`.
 
columnas <- names(datos_diarios_clean)

for (i in columnas){
  datos_diarios_clean[[i]] <- replace(datos_diarios_clean[[i]], datos_diarios_clean[[i]] == "", NA)
}

# Seleccionamos las columnas que son numericas
parametros_numericos <- names(datos_diarios_clean %>% select(-c("Id", "Fecha", "Dia.de.la.semana", "Estacion", "Fecha.creacion")))

#Las convertimos todas a numericas
for (i in parametros_numericos) {
  datos_diarios_clean[[i]] <- as.integer(datos_diarios_clean[[i]])
}


#Selccionamos las variables que nos interesan 
datos_diarios_interesantes <- datos_diarios_clean %>% select(c(PM2.5, PM10,NO2, O3, SO2, Id, Fecha, Dia.de.la.semana, Estacion))


#Ahora queremos los datos de una manera sencilla para representarlo en los graficos
datos_bonitos <- pivot_longer(datos_diarios_interesantes, names_to = "Parametros", values_to = "Valores", cols = c( PM2.5, PM10,NO2, O3, SO2))














