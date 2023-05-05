# Instalar y cargar las librerías necesarias
library(shiny)
library(leaflet)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(rgdal)
library(ggplot2)

# Cargar los datos de calidad del aire en Valencia
# air_data <- read.csv("air_data_valencia.csv", header = TRUE)


load("./data/accum_est_contamin.RData")
load("./data/estaciones2.RData")
load("./data/datos_diarios.RData")
load("./data/datos_diarios_clean.RData")

est_contamin <- left_join(accum_est_contamin, estaciones2, by = "objectid") %>% 
  nest(data = -fecha_carga) %>% 
  arrange(fecha_carga) %>% 
  mutate(id_hora = row_number()) %>% 
  unnest(cols = c(fecha_carga, data))

pal <- colorFactor(c("green", "blue", "orange", "red", "darkred", "purple", "black"),
                   levels = c("Buena", "Razonablemente Buena", "Regular",
                              "Desfavorable", "Muy Desfavorable",
                              "Extremadamente Desfavorable", "Sin datos"))
# trafico_rodado <- left_join(accum_trafico_rodado, tramos_trafico, by = "gid")


#Añadir la info de los buffers

shinyApp(ui, server)
