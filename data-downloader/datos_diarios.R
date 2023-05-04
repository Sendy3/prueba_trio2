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



AQ_index_lvls <- c("Sin Datos", "Buena", "Razonablemente Buena", "Regular",
                   "Desfavorable", "Muy Desfavorable",
                   "Extremadamente Desfavorable")

AQ_data <- read.csv("./data/AQ_EU_data.csv") %>% 
  mutate(
    AQ_index = case_when(
      AirPollutant == "PM2.5" ~ cut(Concentration, c(0, 10, 20, 25, 50, 75, 800), labels = F),
      AirPollutant == "PM10" ~ cut(Concentration, c(0, 20, 40, 50, 100, 150, 1200), labels = F),
      AirPollutant == "NO2" ~ cut(Concentration, c(0, 40, 90, 120, 230, 340, 1000), labels = F),
      AirPollutant == "O3" ~ cut(Concentration, c(0, 50, 100, 130, 240, 380, 800), labels = F),
      AirPollutant == "SO2" ~ cut(Concentration, c(0, 100, 200, 350, 500, 750, 1250), labels = F),
      TRUE ~ 0)
  )


AQ_index_all_hourly <- AQ_data %>% 
  group_by(AirQualityStationEoICode, DatetimeBegin, DatetimeEnd, objectid, fecha_carga) %>% 
  summarise(cause = paste(AirPollutant[which(AQ_index == max(AQ_index))], collapse = ", "),
            AQ_index = max(AQ_index),
            AirPollutant = "AQ_index_all"
  ) %>% 
  ungroup()

AQ_data_clean <- bind_rows(AQ_data, AQ_index_all_hourly) %>% 
  mutate(AQ_index = factor(AQ_index, labels = AQ_index_lvls, levels = 0:6))







