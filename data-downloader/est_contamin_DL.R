# Cargar la librería necesaria
library(lubridate)
library(jsonlite)
library(tidyverse)

# Indicamos el directorio de trabajo (debes indicarlo tú, está vacío por defecto)
wd <- ""

setwd(wd)

# URL del archivo que deseamos descargar
url <- url("https://valencia.opendatasoft.com/api/explore/v2.1/catalog/datasets/estacions-contaminacio-atmosferiques-estaciones-contaminacion-atmosfericas/exports/json?lang=es")

# Leemos los datos
est_contamin <- stream_in(url)

# Guardamos datos de las estaciones

# estaciones <- st_read("https://valencia.opendatasoft.com/api/explore/v2.1/catalog/datasets/estacions-contaminacio-atmosferiques-estaciones-contaminacion-atmosfericas/exports/geojson?lang=es") %>%
#   select(-so2:-calidad_ambiental) %>%
#   mutate(tipozona = factor(tipozona),
#          tipoemision = factor(tipoemision))
# save(estaciones, file = "./data/estaciones.RData")

calidad_aire <- c("Buena", "Razonablemente Buena", "Regular",
                  "Desfavorable", "Muy Desfavorable",
                  "Extremadamente Desfavorable", "Sin datos")
hora_actual <- trunc(Sys.time(), units = "hours") + 60*20

# Limpiamos los datos
est_contamin_clean <- est_contamin %>% 
  mutate(calidad_ambiental = factor(calidad_ambiental, levels = calidad_aire),
         fecha_carga = trunc(ymd_hms(fecha_carga), units = "hours") + 60*20) %>% # Añadimos los 20 minutos manualmente para asegurar que tienen todos el mismo tiempo
  select(objectid, so2:calidad_ambiental)

# Cargamos los datos acumulados para insertar los nuevos
accum_path <- "../data/accum_est_contamin.RData"

# Si había datos, los cargamos y añadimos los nuevos. Si no, lo creamos.
if (file.exists(accum_path)){
  load(accum_path)
  repetidos <- intersect(est_contamin_clean, accum_est_contamin)
  repetidos_clean <- repetidos %>% 
    mutate(so2 = NA, no2 = NA, o3 = NA, co = NA, pm10 = NA, pm25 = NA,
           fecha_carga = hora_actual,
           calidad_ambiental = "Sin datos")
  est_contamin_clean_uq <- est_contamin_clean %>% 
    filter(fecha_carga == hora_actual) %>% 
    rbind(repetidos_clean)
  accum_est_contamin <- rbind(est_contamin_clean_uq, accum_est_contamin)
} else {
  accum_est_contamin <- est_contamin_clean
}

# Guardamos los datos 
save(accum_est_contamin, file = accum_path)

# Cerramos la sesión de R
q()