# Crear el servidor
server <- function(input, output) {
  output$slider <- renderUI({
    sliderInput("time","Time",
                min = min(est_contamin$fecha_carga),
                max =  max(est_contamin$fecha_carga),
                value = min(est_contamin$fecha_carga),step = 3600,
                timezone = "+0000", animate = T)
  })
  
  air_data <- reactive({
    est_contamin
  })
  
  # Creamos el grafico
  
  # Función para filtrar los datos
  datos_filtrados <- reactive({
    datos_diarios %>% 
      filter(Fecha >= "2019-02-06" & Fecha <= "2019-02-09",
             Estacion == input$ID_Estacion)  
  })
  
  # Función para hacer el gráfico
  output$grafico1 <- renderPlot({
    ggplot(datos_filtrados(), aes(x = Fecha, group = 1)) +
      geom_line(aes(y = datos_filtrados()[[input$ID_Calidad]]))
  })
  
  

  
  # Función para crear el mapa con Leaflet
  output$map <- renderLeaflet({
    
    # Crear el mapa con Leaflet
    map <- leaflet(air_data()) %>%
      addTiles() %>%
      addPolygons(data = buffer, color = "red", opacity = 0.2) %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, color = ~pal(calidad_ambiental),
                       popup = ~paste0("<strong>Estación:</strong> ", nombre, "<br>",
                                       "NO2: ", no2, " µg/m3<br>",
                                       "PM10: ", pm10, " µg/m3<br>",
                                       "O3: ", o3, " µg/m3<br>",
                                       "SO2: ", so2, " µg/m3<br>",
                                       "Calidad: ", calidad_ambiental)
      )
    
    # Retornar el mapa
    return(map)
    
  })
  
  observeEvent(input$time,{ 
    # adding day and night section --------------------------------------------
    
    if (hour(input$time) >= 7 & hour(input$time) < 17 ) {
      
      leafletProxy("map", data= air_data()  %>% 
                     filter(fecha_carga == input$time)) %>% 
        clearMarkers() %>% 
        addTiles() %>%
        addCircleMarkers(lng = ~lon, lat = ~lat, color = ~pal(calidad_ambiental),
                         popup = ~paste0("<strong>Estación:</strong> ", nombre, "<br>",
                                         "NO2: ", no2, " µg/m3<br>",
                                         "PM10: ", pm10, " µg/m3<br>",
                                         "O3: ", o3, " µg/m3<br>",
                                         "SO2: ", so2, " µg/m3<br>",
                                         "Calidad: ", calidad_ambiental)
        )
    } else {
      leafletProxy("map", data= air_data()  %>% 
                     filter(fecha_carga == input$time) ) %>% 
        clearMarkers() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addCircleMarkers(lng = ~lon, lat = ~lat, color = ~pal(calidad_ambiental),
                         popup = ~paste0("<strong>Estación:</strong> ", nombre, "<br>",
                                         "NO2: ", no2, " µg/m3<br>",
                                         "PM10: ", pm10, " µg/m3<br>",
                                         "O3: ", o3, " µg/m3<br>",
                                         "SO2: ", so2, " µg/m3<br>",
                                         "Calidad: ", calidad_ambiental)
        )}
  })
  
}

