ui <- dashboardPage(
  
  dashboardHeader(title = "Valencia AQ"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Graficos", tabName = "graficos"),
      menuItem("Stats", tabName = "stats")
      )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 4,
                       box(title = "Visualización temporal",
                           width = NULL, solidHeader = TRUE,
                           actionButton("play", "Reproducir animación", icon = icon("play-circle")),
                       ),
                       box(title = "Variable a mostrar",
                           width = NULL, solidHeader = TRUE,
                           selectInput("var", NULL, choices = c("Índice de Calidad de Aire"))
                       ),
                       conditionalPanel(condition = "input.map_marker_click != null",
                                        box(title = "Estadísticas de la estación seleccionada",
                                            width = NULL, solidHeader = TRUE,
                                            uiOutput("estadisticas_ui"),
                                            plotOutput("grafico", height = 300)))
                ),
                column(width = 8, leafletOutput("map", height = 900))
                
              )),
      
      # Second tab content
      tabItem(tabName = "graficos",
              h2("Distribución del NO2"),
              fluidRow(
                column(width = 4,
                       box(title = "Visualización temporal",
                           width = NULL, solidHeader = TRUE,
                           actionButton("play", "Reproducir animación", icon = icon("play-circle")))))),
      # Third tab content
      tabItem(tabName = "stats",
              h2("Distribución del NO2"),
              fluidPage(
                  # Show a plot of the generated distribution
                  tabsetPanel(
                    tabPanel("Graficos ", 
                        sidebarLayout(
                        sidebarPanel(
                          h4("Elementos de entrada"), #header de tama?o 3 x eso es m?s grande
                          
                          hr(), # crea una linea horizontal, horizontal rule
                          
                          selectInput("ID_Estacion1",
                                      "Selecciona la estación",
                                      unique(datos_diarios$Estacion)),
                          dateRangeInput("ID_Fecha1",
                                         "Selecciona las fechas",
                                         start = min(datos_diarios$Fecha), end = max(datos_diarios$Fecha), min = min(datos_diarios$Fecha),
                                         max = max(datos_diarios$Fecha), format = "yyyy-mm-dd", weekstart = 1,
                                         language = "es", separator = "a"),
                          selectInput("ID_Calidad1",
                                      "Selecciona los parámetros",
                                      names(datos_diarios[6:19]))
                             ),
                        mainPanel())),
                    tabPanel("Graficos varias estaciones", 
                             sidebarLayout(
                               sidebarPanel(
                                 h4("Elementos de entrada"), #header de tama?o 3 x eso es m?s grande
                                 
                                 hr(), # crea una linea horizontal, horizontal rule
                                 
                                 selectInput("ID_Estacion2",
                                             "Selecciona la estación",
                                             unique(datos_diarios$Estacion),multiple = TRUE),
                                 dateRangeInput("ID_Fecha2",
                                                "Selecciona las fechas",
                                                start = min(datos_diarios$Fecha), end = max(datos_diarios$Fecha), min = min(datos_diarios$Fecha),
                                                max = max(datos_diarios$Fecha), format = "yyyy-mm-dd", weekstart = 1,
                                                language = "es", separator = "a"),
                                 selectInput("ID_Calidad2",
                                             "Selecciona los parámetros",
                                             names(datos_diarios[6:19]))
                               ),
                               mainPanel(
                                 h4("Grafico boxplot"),
                                 plotOutput("boxplot"), 
                                 h4("Graficos de tarta"),
                                 plotOutput("tartageneral"),
                                 plotOutput("tarta1parametro")
                               ))),
                    tabPanel("Tabla",  sidebarLayout(
                      sidebarPanel(
                        h4("Elementos de entrada"), #header de tama?o 3 x eso es m?s grande
                        
                        hr(), # crea una linea horizontal, horizontal rule
                        
                        selectInput("ID_Estacion3",
                                    "Selecciona la estación",
                                    unique(datos_diarios$Estacion),multiple = TRUE),
                        dateRangeInput("ID_Fecha3",
                                       "Selecciona las fechas",
                                       start = min(datos_diarios$Fecha), end = max(datos_diarios$Fecha), min = min(datos_diarios$Fecha),
                                       max = max(datos_diarios$Fecha), format = "yyyy-mm-dd", weekstart = 1,
                                       language = "es", separator = "a"),
                        selectInput("ID_Calidad3",
                                    "Selecciona los parámetros",
                                    names(datos_diarios[6:19]),multiple = TRUE)
                      ),
                      mainPanel()))    
                    
                    
                  )
                )
              )
      )
    )
)

