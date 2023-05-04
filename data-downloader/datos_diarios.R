#Cargar las librerias
library(lubridate)
library(jsonlite)
library(tidyverse)
library(readr)
library(dplyr)

# Descargar los datos y convertirlos en una cadena de texto
url <- "https://valencia.opendatasoft.com/api/explore/v2.1/catalog/datasets/rvvcca/exports/csv?lang=es&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"

#Guardarnos la variable
datos_diarios <- read.csv2(url, sep = ";")

# Cargamos los datos acumulados 
path <- "./data/datos_diarios.RData"

# Guardamos los datos 
#save(datos_diarios, file = path)

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
datos_diarios_clean <- datos_diarios_clean %>% select(c(PM2.5, PM10,NO2, O3, SO2, Id, Fecha, Dia.de.la.semana, Estacion))


#Ahora queremos los datos de una manera sencilla para representarlo en los graficos
datos_diarios_clean <- pivot_longer(datos_diarios_clean, names_to = "Parametros", values_to = "Valores", cols = c( PM2.5, PM10,NO2, O3, SO2))



datos_diarios_clean <- datos_diarios_clean %>%
  mutate(
    clasificacion = case_when(
      Parametros == "NO2" & Valores >= 0 & Valores < 40 ~ "Buena",
      Parametros == "NO2" & Valores >= 40 & Valores < 90 ~ "Razonablemente Buena",
      Parametros == "NO2" & Valores >= 90 & Valores < 120 ~ "Regular",
      Parametros == "NO2" & Valores >= 120 & Valores < 230 ~ "Desfavorable",
      Parametros == "NO2" & Valores >= 230 & Valores < 340 ~ "Muy desfavorable",
      Parametros == "NO2" & Valores >= 340 & Valores <= 1000 ~ "Extremadamente Desfavorable",
      
      Parametros == "O3" & Valores >= 0 & Valores < 50 ~ "Buena",
      Parametros == "O3" & Valores >= 50 & Valores < 100 ~ "Razonablemente Buena",
      Parametros == "O3" & Valores >= 100 & Valores < 130 ~ "Regular",
      Parametros == "O3" & Valores >= 130 & Valores < 240 ~ "Desfavorable",
      Parametros == "O3" & Valores >= 240 & Valores < 380 ~ "Muy desfavorable",
      Parametros == "O3" & Valores >= 380 & Valores <= 3800 ~ "Extremadamente Desfavorable",
      
      Parametros == "PM2.5" & Valores >= 0 & Valores < 10 ~ "Buena",
      Parametros == "PM2.5" & Valores >= 10 & Valores < 20 ~ "Razonablemente Buena",
      Parametros == "PM2.5" & Valores >= 20 & Valores < 25 ~ "Regular",
      Parametros == "PM2.5" & Valores >= 25 & Valores < 50 ~ "Desfavorable",
      Parametros == "PM2.5" & Valores >= 50 & Valores < 75 ~ "Muy desfavorable",
      Parametros == "PM2.5" & Valores >= 75 & Valores <= 800 ~ "Extremadamente Desfavorable",
      
      Parametros == "PM10" & Valores >= 0 & Valores < 20 ~ "Buena",
      Parametros == "PM10" & Valores >= 20 & Valores < 40 ~ "Razonablemente Buena",
      Parametros == "PM10" & Valores >= 40 & Valores < 50 ~ "Regular",
      Parametros == "PM10" & Valores >= 50 & Valores < 100 ~ "Desfavorable",
      Parametros == "PM10" & Valores >= 100 & Valores < 150 ~ "Muy desfavorable",
      Parametros == "PM10" & Valores >= 150 & Valores <= 1200 ~ "Extremadamente Desfavorable",
      
      Parametros == "SO2" & Valores >= 0 & Valores < 100 ~ "Buena",
      Parametros == "SO2" & Valores >= 100 & Valores < 200 ~ "Razonablemente Buena",
      Parametros == "SO2" & Valores >= 200 & Valores < 350 ~ "Regular",
      Parametros == "SO2" & Valores >= 350 & Valores < 500 ~ "Desfavorable",
      Parametros == "SO2" & Valores >= 500 & Valores < 750 ~ "Muy desfavorable",
      Parametros == "SO2" & Valores >= 750 & Valores <= 1250 ~ "Extremadamente Desfavorable",
      TRUE ~ NA_character_
    )
  )

# Guardamos los datos 
save(datos_diarios, file = path)
