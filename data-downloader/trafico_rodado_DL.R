# Cargar la librería necesaria
library(lubridate)
library(jsonlite)
library(tidyverse)

# Indicamos el directorio de trabajo
wd <- ""

setwd(wd)

# URL del archivo que deseamos descargar
url <- url("https://valencia.opendatasoft.com/api/explore/v2.1/catalog/datasets/estat-transit-temps-real-estado-trafico-tiempo-real/exports/json?lang=es")

# Leemos los datos
trafico_rodado <- stream_in(url)

labels_estado <- c("Fluido", "Denso", "Congestionado", "Cortado", "Sin datos",
                   str_c("Paso inferior ",
                        c("fluido", "denso", "congestionado", "cortado")
                   ),
                   "Sin datos (paso inferior)")

hora_actual <- Sys.time()

# Guardamos datos de los tramos
# tramos_trafico <- st_read("https://valencia.opendatasoft.com/api/explore/v2.1/catalog/datasets/estat-transit-temps-real-estado-trafico-tiempo-real/exports/geojson?lang=es") %>% 
#   select(gid, denominacion, idtramo)
# save(tramos_trafico, file = "./data/tramos_trafico.RData")

# Limpiamos los datos
trafico_rodado_clean <- trafico_rodado
trafico_rodado_clean$estado <- factor(trafico_rodado$estado, levels = 0:9, labels = labels_estado)
trafico_rodado_clean$fecha_carga <- ymd_hms(hora_actual)
trafico_rodado_clean <- trafico_rodado_clean %>% select(gid, estado, fecha_carga)

# Cargamos los datos acumulados para insertar los nuevos
accum_path <- "./data/accum_trafico_rodado.RData"

# Si había datos, los cargamos y añadimos los nuevos. Si no, lo creamos.
if (file.exists(accum_path)){
  load(accum_path)
  accum_trafico_rodado <- rbind(trafico_rodado_clean, accum_trafico_rodado)
  
} else {
  accum_trafico_rodado <- trafico_rodado_clean
}

# Guardamos los datos 
save(accum_trafico_rodado, file = accum_path)

# Cerramos la sesión de R
q()