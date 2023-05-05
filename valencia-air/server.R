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
      filter(Fecha >= input$ID_Fecha2[1] & Fecha <= input$ID_Fecha2[2],
             Estacion %in% input$ID_Estacion2)
  })

  #Metemos la info de los plots
  output$grafico1 <- renderPlotly({
    shiny::validate(
      need(input$ID_Estacion2, "Elige una o varias estaciones"),
      need(input$ID_Calidad2, "Elige una o varios parametros de calidad del aire")
    )
    gra <- ggplot(datos_filtrados(), aes(x = Fecha, colour = Estacion, group = Estacion)) +
      ylim(0, 100) +
      theme(legend.position = "none") + 
      theme_minimal()
    for (calidad in input$ID_Calidad2) {
      gra <- gra + geom_line(aes_string(y = calidad))
    }
    gra
  })
  
  #AÑADIMOS LA TABLA
  estaciones <- reactive({
    input$ID_Estacion3 
  })
  
  #Datos para varias estaciones y todos los parametros
  datos_filtrados1 <- reactive({
    datos_diarios_clean %>% 
      filter(Fecha >= input$ID_Fecha2[1] & Fecha <= input$ID_Fecha2[2],
             Estacion == input$ID_Estacion2)  
  })

  
  #Funcion para crear el boxplot
  output$boxplot <- renderPlotly({
    validate(need(input$ID_Estacion2, "Elige una o varias estaciones"))
    gra <- ggplot(datos_filtrados1(), aes(x = Parametros, y = Valores, fill = "blue")) + 
      geom_boxplot(alpha = 0.5) + 
      labs(x = "Parametros", y = "Valores") + 
      theme(legend.position = "none") + 
      theme_minimal() + 
      scale_fill_manual(values = alpha("blue", 0.5))
    
    # Convertir ggplot en plotly
    ggplotly(gra, tooltip = "text", dynamicTicks = TRUE) %>% 
      layout(showlegend = FALSE) # Ocultar la leyenda de color en plotly
  })
  
  # Datos normalizados
  datos_normalizados <- reactive({
    datos_diarios %>% 
    pivot_longer(names_to = "Parametros", values_to = "Valores", 
                 cols = c("PM1", "PM2.5", "PM10", "NO", "NO2", "NOx", "O3", "SO2", "CO", "NH3", "C7H8", "C6H6", "C8H10"))%>%
    group_by(Parametros) %>%
    mutate(Valores_norm = (Valores - min(Valores, na.rm = TRUE)) / (max(Valores, na.rm = TRUE) - min(Valores, na.rm = TRUE))) %>%
    filter(Fecha >= "2019-02-06" & Fecha <= "2019-02-09",
           Estacion %in% "Viveros")
  })
  
  
  #Funcion para crear el histograma con los parametros normalizados
  output$histograma <- renderPlot({
    validate(need(input$ID_Estacion2, "Elige una o varias estaciones"))
    ggplot(datos_diarios %>% 
             pivot_longer(names_to = "Parametros", values_to = "Valores", 
                          cols = c("PM1", "PM2.5", "PM10", "NO", "NO2", "NOx", "O3", "SO2", "CO", "NH3", "C7H8", "C6H6", "C8H10"))%>%
             group_by(Parametros) %>%
             mutate(Valores_norm = (Valores - min(Valores, na.rm = TRUE)) / (max(Valores, na.rm = TRUE) - min(Valores, na.rm = TRUE))) %>%
             filter(Fecha >= "2019-02-06" & Fecha <= "2019-02-09",
                    Estacion %in% "Viveros"), aes(x = Parametros, y = Valores_norm)) +
      geom_bar(stat = "identity") +
      labs(x = "Parametros", y = "Valores normalizados") + 
      theme(legend.position = "none") + 
      theme_minimal() 
  })
  
  
  # Funcion para crear el grafico de tarta para varias estaciones y todos los parametros
  output$tartageneral <- renderPlot({
  validate(need(input$ID_Estacion2, "Elige una o varias estaciones"))
    #pie(x = datos_filtrados1()[[Valores]], labels = datos_filtrados1()[[Parametros]],  main = "Gráfico de tarta para todos los parametros")
  # Basic piechart
  ggplot(datos_diarios_clean %>%
           filter(Estacion %in% input$ID_Estacion2) %>%
           filter(Fecha >= input$ID_Fecha2[1] & Fecha <= input$ID_Fecha2[2]) %>%
           group_by(Clasificacion) %>% 
           summarise(con=n()) %>% 
           ungroup()
           , aes(x="", y=con, fill=Clasificacion)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
      
    theme_void() # remove background, grid, numeric labels
    })

  # Función para crear el gráfico semanal 
  output$semanal <- renderPlot({
    validate(need(input$ID_Estacion2, "Elige una o varias estaciones"))
    ggplot(datos_diarios_clean %>% 
             filter(Fecha >= input$ID_Fecha2[1] & Fecha <= input$ID_Fecha2[2]) %>%
             filter(Clasificacion == "Muy Desfavorable"|Clasificacion=="Extremadamente Desfavorable") %>% 
             mutate(dia_sem = factor(dia_sem, levels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"))) %>% 
             group_by(dia_sem) %>% 
             summarise(con=n()) %>% 
             ungroup(),
           aes(dia_sem, con, fill=dia_sem))+
      geom_col()+
      coord_polar()+
      theme(legend.position = "none",
            axis.text.y = element_blank(),
            axis.text.x = element_text(size=6),
            axis.ticks = element_blank())+
      labs(title="Valores desfavorables para todas las estaciones", x="", y="")
  })
  
  # Función para crear el gráfico semanal 2 para cada estacion seleccionada
  output$semanal2 <- renderPlot({
    validate(need(input$ID_Estacion2, "Elige una o varias estaciones"))
    ggplot(datos_diarios_clean %>% 
             filter(Fecha >= input$ID_Fecha2[1] & Fecha <= input$ID_Fecha2[2]) %>%
             filter(Clasificacion == "Muy Desfavorable"|Clasificacion=="Extremadamente Desfavorable") %>% 
             mutate(dia_sem = factor(dia_sem, levels = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"))) %>% 
             filter(Estacion %in% input$ID_Estacion2) %>%
             group_by(dia_sem) %>% 
             summarise(con=n()) %>% 
             ungroup(),
           aes(dia_sem, con, fill=dia_sem))+
      geom_col()+
      coord_polar()+
      theme(legend.position = "none",
            axis.text.y = element_blank(),
            axis.text.x = element_text(size=6),
            axis.ticks = element_blank())+
      labs(title="Valores desfavorables para cada estacion seleccionada", x="", y="")
  })
  # #Funcion para crear el grafico de tarta para varias estaciones y 1 parametro
  # output$tarta1parametro <- renderPlot({
  # validate(need(input$ID_Estacion2, "Elige una o varias estaciones"))
  #   datos <- datos_filtrados1() %>% filter(Parametros == input$ID_Calidad2)
  #   pie(x =  datos[["Valores"]], labels = datos[["clasificacion"]], main = "Gráfico de tarta para el parametro seleccionado")
  # })

  
  #Datos filtrados para la pestaña de tabla
  datos_filtrados3 <- reactive({
    datos_diarios_clean %>% 
      filter(Fecha >= input$ID_Fecha2[1] & Fecha <= input$ID_Fecha2[2], #Fecha >= input$ID_Fecha3[1] & Fecha <= input$ID_Fecha3[2]
             Estacion %in% input$ID_Estacion3, Parametros %in% input$ID_Calidad3)  
  })
  
  #AÑADIMOS LA TABLA
  estaciones <- reactive({
    input$ID_Estacion3 
  })
  
  #Muestro las 10 primeras variables en Table2
  output$tabla <- DT::renderDataTable({
    validate(
      need(input$ID_Estacion3, "Selecciona la estación que quieras ver"),
      need(input$ID_Calidad3, "Selecciona el parametro de calidad de aire")
    )
    DT::datatable(
      datos_filtrados3(), # Dataframe a mostrar
      options = list(scrollX = TRUE), # Opciones de visualización
      class = "display nowrap compact"
    )
  })
  
  
  #Mostrar las estadisticas para los datos seleccionados
  
  output$stats<- renderPrint({
    validate(
      need(input$ID_Estacion3, "Elige una o varias estaciones"),
      need(input$ID_Calidad3, "Elige uno o varios parametros"),
      need(input$ID_Fecha3, "Elige una o varias estaciones")
    )
    summary(datos_filtrados3())
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
