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


# creamos una columna con el valor Clasificacion inicialmente con todo NA
datos_diarios_clean <- datos_diarios_clean %>% mutate(Clasificacion = NA)

# creamos una lista con las diferentes clasificaciones
lista <- list(PM2.5 = c(0, 10, 20, 25, 50, 75, 800),
              PM10 = c(0, 20, 40, 50, 100, 150, 1200),
              NO2 = c(0, 40, 90, 120, 230, 340, 1000),
              O3 = c(0, 50, 100, 130, 240, 380, 800),
              SO2 = c(0, 100, 200, 350, 500, 750, 1250))
# creamos un vector con lo que indica cada intervalo de las clasificaciones
vector2 <- c("Buena", "Razonablemente Buena", "Regular", "Desfavorable", "Muy Desfavorable", "Extremadamente Desfavorable")

# para cada parametro de los nombres de la lista (PM2.5, PM10...)
for (parametro in names(lista)) {
  # miramos el vector de valores de ese parametro
  valor <- lista[[parametro]] 
  # con mutate editamos el valor de la clasificacion
  datos_diarios_clean <- datos_diarios_clean %>% 
    # si el valor de la columna Parametros es igual al parametro que estamos observando en esta iteración
    # y el valor de la columna Valores es mayor que el valor[1] del vector de valores y menor que el 
    # valor[2] del vector de valores entonces ponemos la primera clasificación del vector2 (Buena) sino 
    # lo dejamos igual
    # seguimos el mismo procedimiento con los diferentes mutate hasta que en el último miramos
    # si los valores están a NA y en ese caso ponemos la clasificación como "Sin datos"
    mutate(Clasificacion = ifelse(Parametros == parametro & (Valores >= valor[1] & Valores <= valor[2]), vector2[1], Clasificacion)) %>%
    mutate(Clasificacion = ifelse(Parametros == parametro & (Valores > valor[2] & Valores <= valor[3]), vector2[2], Clasificacion)) %>%
    mutate(Clasificacion = ifelse(Parametros == parametro & (Valores > valor[3] & Valores <= valor[4]), vector2[3], Clasificacion)) %>%
    mutate(Clasificacion = ifelse(Parametros == parametro & (Valores > valor[4] & Valores <= valor[5]), vector2[4], Clasificacion)) %>%
    mutate(Clasificacion = ifelse(Parametros == parametro & (Valores > valor[5] & Valores <= valor[6]), vector2[5], Clasificacion)) %>%
    mutate(Clasificacion = ifelse(Parametros == parametro & (Valores > valor[6] & Valores <= valor[7]), vector2[6], Clasificacion)) %>%
    mutate(Clasificacion = ifelse(Parametros == parametro & is.na(Valores), "Sin datos", Clasificacion))
  
}

# assigning the third column name to a new name
colnames(datos_diarios_clean)[3] <- "dia_sem"

path <- "./data/datos_diarios_clean.RData"
# Guardamos los datos 
save(datos_diarios_clean, file = path)
