library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(e1071) 
library(shinyjs)
library(shiny)
library(shinydashboard)
library(ggmosaic)

# Cargar la base de datos de salud del sueño y estilo de vida
base_datos <- read.csv("Sleep_health_and_lifestyle_dataset.csv", stringsAsFactors = FALSE)
colnames(base_datos)

ui <- fluidPage(
  useShinyjs(),  # Habilita shinyjs
  
  tags$head(
    tags$style(
      HTML(
        "
        /* Estilo para las pestañas de tabsetPanel */
        .nav-tabs > li > a {
          background-color: #aac3e4 ; /* Color de fondo de las pestañas */
          color: black; /* Color del texto en las pestañas */
        }
        .nav-tabs > li > a:hover {
          background-color: #789ac7 ; /* Color de fondo cuando se pasa el ratón por encima */
        }
        .nav-tabs > li.active > a {
          background-color: #789ac7 ; /* Color de fondo de la pestaña activa */
          color: black; /* Color del texto en la pestaña activa */
        }

        /* Estilo para el sidebarPanel */
        .well {
          background-color: #aac3e4; /* Cambia a celeste claro el panel lateral */
          color: black; /* Cambia el color del texto a negro */
          border-color: #789ac7 ; /* Cambia el color del borde del panel */
        }

        /* Estilos para la portada */
        #portada h1 {
          font-size: 3em;
        }
        #portada h3 {
          font-size: 2em;
        }
        "
      )
    )
  ),
  
  fluidRow(
    id = "portada",
    style = "background-color: #29395e ; padding: 50px; text-align: center; font-family: 'Arial';",
    column(12,
           h1("Estudio del Sueño y Estilo de Vida", 
              style = "color: #e2eff4; font-weight: bold; margin-bottom: 40px;"),
           img(src = "https://cdn-icons-png.flaticon.com/512/4310/4310163.png", 
               alt = "Imagen de portada", width = "30%", style = "margin-bottom: 20px;"),
           h3("Proyecto de Análisis Estadístico", 
              style = "color: #e2eff4; margin-bottom: 20px;"),
           h3("Grupo 2", 
              style = "color: #e2eff4; margin-bottom: 40px;"),
           
           # Botón Descripción de la Base de Datos
           actionButton("toggle_db_desc", "Descripción de la Base de Datos", 
                        style = "background-color: #3e4e8f; color: white; font-size: 18px; padding: 10px 25px; border-radius: 8px; margin-bottom: 10px;"),
           br(),
           
           # Texto Descripción de la Base de Datos (oculto inicialmente)
           hidden(
             div(id = "db_description",
                 div(
                   h3("Sleep_health_and_lifestyle_dataset",
                      style = "color: #f0f8ff ; font-size: 20px; margin-top: 10px;"),
                   p("La base de datos 'Sleep Health and Lifestyle' comprende 400 filas y 6 columnas, abarcando una amplia gama de variables relacionadas con el sueño y hábitos diarios.",
                     style = "color: #f0f8ff ; font-size: 16px;"),
                   p("Incluye detalles como la edad, la duración del sueño, el nivel de actividad física, el género, su ocupación (profesión) y su Índice de Masa Corporal.",
                     style = "color: #f0f8ff ; font-size: 16px; margin-bottom: 30px;"),
                   style = "border: 1px solid #f0f8ff ; padding: 10px; border-radius: 10px; background-color: #3e4e8f; margin-bottom: 20px; width: 60%; margin: 0 auto;"
                 )
             )
           ),
           
           # Botón Conceptos Clave (con separación adicional)
           actionButton("toggle_concepts", "Conceptos Clave", 
                        style = "background-color: #3e4e8f; color: white; font-size: 18px; padding: 10px 25px; border-radius: 8px; margin-top: 20px; margin-bottom: 10px;"),  # Aquí se añade margen superior
           br(),
           
           # Texto Conceptos Clave (oculto inicialmente)
           hidden(
             div(id = "concepts_info",
                 div(
                  
                   p("Variables Numéricas: Son aquellas que pueden medirse y expresarse en valores numéricos. Se dividen en: ",
                     style = "color: #f0f8ff ; font-size: 16px;"),
                   tags$ul(
                     tags$li("Continuas: Pueden tomar cualquier valor dentro de un rango, como la duración del sueño.",
                             style = "color: #c6dee9 ; font-size: 16px;"),  # Color más claro
                     tags$li("Discretas: Solo pueden tomar valores enteros, como el número de hijos.",
                             style = "color: #c6dee9 ; font-size: 16px;")   # Color más claro
                   ),
                   p("Variables Categóricas: Son aquellas que representan categorías o grupos. Se dividen en: ",
                     style = "color: #f0f8ff ; font-size: 16px;"),
                   tags$ul(
                     tags$li("Nominales: No tienen un orden natural, como el género.",
                             style = "color: #c6dee9 ; font-size: 16px;"),  # Color más claro
                     tags$li("Ordinales: Tienen un orden o jerarquía, como las categorías de IMC (Bajo, Normal, Sobrepeso).",
                             style = "color: #c6dee9 ; font-size: 16px;")   # Color más claro
                   ),
                   style = "border: 1px solid #f0f8ff ; padding: 10px; border-radius: 10px; background-color: #3e4e8f; margin-bottom: 20px; width: 60%; margin: 0 auto;"
                 )
             )
           ),
           
           # Botón Autores
           actionButton("toggle_authors", "Autores", 
                        style = "background-color: #3e4e8f; color: white; font-size: 18px; padding: 10px 25px; border-radius: 8px; margin-top: 10px;"),
           br(),
           
           hidden(
             div(id = "authors_info",
                 div(
                   p("El proyecto fue realizado por estudiantes de la Universidad de las Fuerzas Armadas ESPE, de la carrera de Ingenieria de Software, para la asignatura de Estadística:",  
                     style = "color: #f0f8ff ; font-size: 17px;"),
                   p("1. Luis Sagnay",
                     style = "color: #f0f8ff ; font-size: 16px;"),
                   p("2. Mateo Iza",
                     style = "color: #f0f8ff ; font-size: 16px;"),
                   p("3. Eduardo García",
                     style = "color: #f0f8ff ; font-size: 16px;"),
                   style = "border: 1px solid #e2eff4; padding: 10px; border-radius: 10px; background-color: #3e4e8f; margin-bottom: 20px; width: 60%; margin: 20px auto;"
                 )
             )
           ),
           
           # Botón Iniciar Análisis
           actionButton("go_analysis", "Iniciar Análisis", 
                        style = "background-color: #3e4e8f; color: white; font-size: 20px; padding: 12px 30px; border-radius: 8px; margin-top: 20px;")
    )
  ),
  
  # Contenido principal oculto inicialmente
  hidden(
    div(id = "main_content",
        sidebarLayout(
          sidebarPanel(
            # Aquí es donde se controlará qué elementos mostrar en función de la pestaña seleccionada
            uiOutput("sidebar_content")
          ),
          
          mainPanel(
            tabsetPanel(
              id = "tabs",  # ID del tabsetPanel para usar en las condiciones
              
              tabPanel("Análisis Numérico",
                       conditionalPanel(
                         condition = "input.show_age",
                         h3("Edad"),
                         plotOutput("histogram_age"),
                         plotOutput("ojiva_age"),
                         plotOutput("boxplot_age"),
                         uiOutput("stats_age")
                       ),
                       conditionalPanel(
                         condition = "input.show_sleep",
                         h3("Duración del Sueño"),
                         plotOutput("histogram_sleep"),
                         plotOutput("ojiva_sleep"),
                         plotOutput("boxplot_sleep"),
                         uiOutput("stats_sleep")
                       ),
                       conditionalPanel(
                         condition = "input.show_activity",
                         h3("Nivel de Actividad Física"),
                         plotOutput("histogram_activity"),
                         plotOutput("ojiva_activity"),
                         plotOutput("boxplot_activity"),
                         uiOutput("stats_activity")
                       )
              ),
              tabPanel("Análisis Categórico",
                       uiOutput("categorical_analysis")
              ),
              tabPanel("Análisis Descriptivo Conjunto",
                       plotOutput("boxplot_continuous_categorical"),
                       plotOutput("barplot_continuous_categorical")
              ),
              tabPanel("Análisis Conjunto Categórico",
                       plotOutput("mosaicplot_categorical_categorical"),
                       tableOutput("contingency_table"),
                       uiOutput("conclusion_categorical_categorical")
              ),
              tabPanel("Análisis entre Variables Numéricas",
                       plotOutput("scatterplot_numeric_numeric"),
                       tableOutput("correlation_table"),
                       uiOutput("conclusion_numeric_numeric")
              ), 
              tabPanel("Base de Datos", 
                       DTOutput("data_table")
              )
            )
          )
        )
    )
  )
)

server <- function(input, output) {
  # Manejadores para mostrar/ocultar secciones de la portada
  observeEvent(input$toggle_db_desc, {
    toggle("db_description")
  })
  
  observeEvent(input$toggle_concepts, {
    toggle("concepts_info")
  })
  
  observeEvent(input$toggle_authors, {
    toggle("authors_info")
  })
  
  observeEvent(input$go_analysis, {
    show("main_content")
    hide("portada")
  })
  
  # Aquí puedes agregar el código para generar gráficos y resultados según las pestañas seleccionadas
  # Por ejemplo:
  output$histogram_age <- renderPlot({
    # Código para el histograma de edad
  })
  
  output$ojiva_age <- renderPlot({
    # Código para la ojiva de edad
  })
  
  output$boxplot_age <- renderPlot({
    # Código para el boxplot de edad
  })
  
  output$stats_age <- renderUI({
    # Código para mostrar estadísticas descriptivas
  })
  
  output$sidebar_content <- renderUI({
    if (input$tabs == "Análisis Numérico") {
      tagList(
        h4("Opciones de visualización"),
        checkboxInput("show_age", "Mostrar Edad", TRUE),
        checkboxInput("show_sleep", "Mostrar Duración del Sueño", TRUE),
        checkboxInput("show_activity", "Mostrar Nivel de Actividad Física", TRUE),
        sliderInput("confidence_level", "Nivel de Confianza:", min = 0.80, max = 0.99, value = 0.95, step = 0.01),
        
        h4("Modificar Datos"),
        sliderInput("age_filter", "Filtrar por Edad:", min = 0, max = 100, value = c(0, 100)),
        sliderInput("sleep_filter", "Filtrar por Horas de Sueño:", min = 0, max = 24, value = c(0, 24)),
        sliderInput("activity_filter", "Filtrar por Nivel de Actividad Física (min/día):", min = 0, max = 300, value = c(0, 300))
      )
    } else if (input$tabs == "Análisis Categórico") {
      tagList(
        h4("Visualización de Variables Categóricas"),
        checkboxGroupInput("categorical_vars", "Seleccione Variables Categóricas:",
                           choices = c("Género" = "Gender", "Ocupación" = "Occupation", "Categoría IMC" = "BMI_Category"), 
                           selected = c("Gender"))
      )
    } else if (input$tabs == "Análisis Descriptivo Conjunto") {
      tagList(
        h4("Análisis Descriptivo Conjunto"),
        selectInput("continuous_var", "Seleccionar Variable Continua:", 
                    choices = c("Edad" = "Age", "Duración del Sueño" = "Sleep_Duration", "Nivel de Actividad Física" = "Physical_Activity_Level")),
        selectInput("categorical_var", "Seleccionar Variable Categórica:", 
                    choices = c("Género" = "Gender", "Ocupación" = "Occupation", "Categoría IMC" = "BMI_Category"))
      )
    } else if (input$tabs == "Análisis Conjunto Categórico") {
      tagList(
        h4("Análisis Conjunto Categórico"),
        selectInput("categorical_var1", "Seleccionar Primera Variable Categórica:", 
                    choices = c("Género" = "Gender", "Ocupación" = "Occupation", "Categoría IMC" = "BMI_Category")),
        selectInput("categorical_var2", "Seleccionar Segunda Variable Categórica:", 
                    choices = c("Género" = "Gender", "Ocupación" = "Occupation", "Categoría IMC" = "BMI_Category"))
      )
    } else if (input$tabs == "Análisis entre Variables Numéricas") {
      tagList(
        h4("Análisis entre Variables Numéricas"),
        selectInput("numeric_var1", "Seleccionar Primera Variable Numérica:", 
                    choices = c("Edad" = "Age", "Duración del Sueño" = "Sleep_Duration", "Nivel de Actividad Física" = "Physical_Activity_Level")),
        selectInput("numeric_var2", "Seleccionar Segunda Variable Numérica:", 
                    choices = c("Edad" = "Age", "Duración del Sueño" = "Sleep_Duration", "Nivel de Actividad Física" = "Physical_Activity_Level"))
      )
    } else {
      NULL
    }
  })
  
  # Datos filtrados según selección de usuario
  datos_filtrados <- reactive({
    datos <- base_datos %>%
      filter(Age >= input$age_filter[1], Age <= input$age_filter[2]) %>%
      filter(Sleep_Duration >= input$sleep_filter[1], Sleep_Duration <= input$sleep_filter[2]) %>%
      filter(Physical_Activity_Level >= input$activity_filter[1], Physical_Activity_Level <= input$activity_filter[2])
    
    return(datos)
  })
  
  
  intervalo_confianza_media <- function(x, conf_level) {
    n <- length(x)
    media <- mean(x)
    s <- sd(x)
    error <- qt((1 + conf_level) / 2, df = n - 1) * (s / sqrt(n))
    c(media - error, media + error)
  }
  
  intervalo_confianza_varianza <- function(x, conf_level) {
    n <- length(x)
    s2 <- var(x)
    df <- n - 1
    alpha <- 1 - conf_level
    chi2_lower <- qchisq(alpha / 2, df)
    chi2_upper <- qchisq(1 - alpha / 2, df)
    c((df * s2) / chi2_upper, (df * s2) / chi2_lower)
  }
  
  
  # Renderizar tabla de datos
  output$data_table <- renderDT({
    datatable(datos_filtrados(), 
              options = list(pageLength = 10,
                             lengthMenu = c(5, 10, 20),
                             searching = FALSE,
                             ordering = TRUE),
              rownames = FALSE,
              class = 'cell-border stripe',
              style = "bootstrap")
  })
  
  # Renderizar histogramas
  output$histogram_age <- renderPlot({
    req(input$show_age)
    ggplot(datos_filtrados(), aes(x = Age)) +
      geom_histogram(binwidth = 1, fill = "pink", color = "black") +
      labs(title = "Histograma de Edad", x = "Edad", y = "Frecuencia")
  })
  
  output$histogram_sleep <- renderPlot({
    req(input$show_sleep)
    ggplot(datos_filtrados(), aes(x = Sleep_Duration)) +
      geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
      labs(title = "Histograma de Duración del Sueño", x = "Duración del Sueño (horas)", y = "Frecuencia") 
  })
  
  output$histogram_activity <- renderPlot({
    req(input$show_activity)
    ggplot(datos_filtrados(), aes(x = Physical_Activity_Level)) +
      geom_histogram(binwidth = 10, fill = "orange", color = "black") +
      labs(title = "Histograma de Nivel de Actividad Física", x = "Nivel de Actividad Física", y = "Frecuencia")
  })
  
  # Renderizar ojivas
  output$ojiva_age <- renderPlot({
    req(input$show_age)
    histo <- hist(datos_filtrados()$Age, plot = FALSE)
    facum <- cumsum(histo$counts)
    plot(histo$mids, facum, type = "l", main = "Ojiva de Edad", xlab = "Edad", ylab = "Frecuencia Acumulada", col = "purple")
  })
  
  output$ojiva_sleep <- renderPlot({
    req(input$show_sleep)
    histo <- hist(datos_filtrados()$Sleep_Duration, plot = FALSE)
    facum <- cumsum(histo$counts)
    plot(histo$mids, facum, type = "l", main = "Ojiva de Duración del Sueño", xlab = "Duración del Sueño", ylab = "Frecuencia Acumulada", col = "blue")
  })
  
  output$ojiva_activity <- renderPlot({
    req(input$show_activity)
    histo <- hist(datos_filtrados()$Physical_Activity_Level, plot = FALSE)
    facum <- cumsum(histo$counts)
    plot(histo$mids, facum, type = "l", main = "Ojiva de Nivel de Actividad Física", xlab = "Nivel de Actividad Física", ylab = "Frecuencia Acumulada", col = "red")
  })
  
  # Renderizar boxplots
  output$boxplot_age <- renderPlot({
    req(input$show_age)
    ggplot(datos_filtrados(), aes(y = Age)) +
      geom_boxplot(fill = "pink") +
      labs(title = "BoxPlot de Edad", y = "Edad")
  })
  
  output$boxplot_sleep <- renderPlot({
    req(input$show_sleep)
    ggplot(datos_filtrados(), aes(y = Sleep_Duration)) +
      geom_boxplot(fill = "skyblue") +
      labs(title = "BoxPlot de Duración del Sueño", y = "Duración del Sueño")
  })
  
  output$boxplot_activity <- renderPlot({
    req(input$show_activity)
    ggplot(datos_filtrados(), aes(y = Physical_Activity_Level)) +
      geom_boxplot(fill = "orange") +
      labs(title = "BoxPlot de Nivel de Actividad Física", y = "Nivel de Actividad Física")
  })
  
  output$stats_age <- renderUI({
    req(input$show_age)
    var <- datos_filtrados()$Age
    media <- mean(var)
    mediana <- median(var)
    moda <- as.numeric(names(sort(table(var), decreasing = TRUE)[1]))
    rango <- range(var)
    varianza <- var(var)
    desviacion <- sd(var)
    cuartiles <- quantile(var, probs = c(0.25, 0.5, 0.75))
    deciles <- quantile(var, probs = seq(0, 1, by = 0.1))
    percentiles <- quantile(var, probs = seq(0, 1, by = 0.01))
    asimetria <- skewness(var)
    
    conf_level <- input$confidence_level
    
    conf_media <- intervalo_confianza_media(var, conf_level)
    conf_varianza <- intervalo_confianza_varianza(var, conf_level)
    
    tagList(
      h4("Estadísticas Descriptivas de Edad"),  # Tamaño de letra para el título
      tags$div(style = "display: flex; flex-wrap: wrap;",
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Media:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Media"
                        tags$p(round(media, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Media"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Mediana:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Mediana"
                        tags$p(round(mediana, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Mediana"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Moda:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Moda"
                        tags$p(round(moda, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Moda"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Rango:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Rango"
                        tags$p(paste(rango, collapse = " - "), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Rango"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Varianza:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Varianza"
                        tags$p(round(varianza, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Varianza"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Desviación Estándar:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Desviación Estándar"
                        tags$p(round(desviacion, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Desviación Estándar"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Cuartil 1:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Cuartil 1"
                        tags$p(paste(round(cuartiles[1], 2)), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Cuartil 1"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Cuartil 2:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Cuartil 2"
                        tags$p(paste(round(cuartiles[2], 2)), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Cuartil 2"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Cuartil 3:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Cuartil 3"
                        tags$p(paste(round(cuartiles[3], 2)), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Cuartil 3"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Asimetría:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Asimetría"
                        tags$p(round(asimetria, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Asimetría"
               )
      ),
      h4("Intervalos de Confianza"),  # Tamaño de letra para el título de Intervalos de Confianza
      tags$div(
        style = "display: flex; justify-content: space-between;", 
        tags$div(
          style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
          tags$p(tags$strong("Intervalo de Confianza para la Media:", style = "font-size: 17px;"), style = "font-size: 17px;"),  # Tamaño de letra para el título del intervalo de confianza de la media
          tags$p(paste0("IC: [", round(conf_media[1], 2), ", ", round(conf_media[2], 2), "]"), style = "font-size: 15px;")  # Tamaño de letra para el valor del intervalo de confianza de la media
        ),
        tags$div(
          style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
          tags$p(tags$strong("Intervalo de Confianza para la Varianza:", style = "font-size: 17px;"), style = "font-size: 17px;"),  # Tamaño de letra para el título del intervalo de confianza de la varianza
          tags$p(paste0("IC: [", round(conf_varianza[1], 2), ", ", round(conf_varianza[2], 2), "]"), style = "font-size: 15px;")  # Tamaño de letra para el valor del intervalo de confianza de la varianza
        )
      )
    )
    

  })
  
  output$stats_sleep <- renderUI({
    req(input$show_sleep)
    var <- datos_filtrados()$Sleep_Duration
    media <- mean(var)
    mediana <- median(var)
    moda <- as.numeric(names(sort(table(var), decreasing = TRUE)[1]))
    rango <- range(var)
    varianza <- var(var)
    desviacion <- sd(var)
    cuartiles <- quantile(var, probs = c(0.25, 0.5, 0.75))
    deciles <- quantile(var, probs = seq(0, 1, by = 0.1))
    percentiles <- quantile(var, probs = seq(0, 1, by = 0.01))
    asimetria <- skewness(var)
    conf_level <- input$confidence_level
    
    conf_media <- intervalo_confianza_media(var, conf_level)
    conf_varianza <- intervalo_confianza_varianza(var, conf_level)
    
    tagList(
      h4("Estadísticas Descriptivas de Duración del Sueño"),
      tags$div(style = "display: flex; flex-wrap: wrap;",
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
                        tags$b("Media:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Media"
                        tags$p(round(media, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Media"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
                        tags$b("Mediana:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Mediana"
                        tags$p(round(mediana, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Mediana"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
                        tags$b("Moda:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Moda"
                        tags$p(round(moda, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Moda"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
                        tags$b("Rango:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Rango"
                        tags$p(paste(rango, collapse = " - "), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Rango"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
                        tags$b("Varianza:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Varianza"
                        tags$p(round(varianza, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Varianza"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
                        tags$b("Desviación Estándar:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Desviación Estándar"
                        tags$p(round(desviacion, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Desviación Estándar"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
                        tags$b("Cuartil 1:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Cuartil 1"
                        tags$p(paste(round(cuartiles[1], 2)), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Cuartil 1"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
                        tags$b("Cuartil 2:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Cuartil 2"
                        tags$p(paste(round(cuartiles[2], 2)), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Cuartil 2"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
                        tags$b("Cuartil 3:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Cuartil 3"
                        tags$p(paste(round(cuartiles[3], 2)), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Cuartil 3"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
                        tags$b("Asimetría:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Asimetría"
                        tags$p(round(asimetria, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Asimetría"
               )
      ),
      h4("Intervalos de Confianza"),  # Tamaño de letra para el título de Intervalos de Confianza
      tags$div(
        style = "display: flex; justify-content: space-between;", 
        tags$div(
          style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
          tags$p(tags$strong("Intervalo de Confianza para la Media:", style = "font-size: 17px;"), style = "font-size: 17px;"),  # Tamaño de letra para el título del intervalo de confianza de la media
          tags$p(paste0("IC: [", round(conf_media[1], 2), ", ", round(conf_media[2], 2), "]"), style = "font-size: 15px;")  # Tamaño de letra para el valor del intervalo de confianza de la media
        ),
        tags$div(
          style = "flex: 1; padding: 10px; border: 1px solid #00FFFFE6; border-radius: 5px; margin: 5px; background-color: rgba(0, 255, 255, 0.3); width: 200px; height: 100px; color: #333;", 
          tags$p(tags$strong("Intervalo de Confianza para la Varianza:", style = "font-size: 17px;"), style = "font-size: 17px;"),  # Tamaño de letra para el título del intervalo de confianza de la varianza
          tags$p(paste0("IC: [", round(conf_varianza[1], 2), ", ", round(conf_varianza[2], 2), "]"), style = "font-size: 15px;")  # Tamaño de letra para el valor del intervalo de confianza de la varianza
        )
      )  
    )
  })
  
  output$stats_activity <- renderUI({
    req(input$show_activity)
    var <- datos_filtrados()$Physical_Activity_Level
    media <- mean(var)
    mediana <- median(var)
    moda <- as.numeric(names(sort(table(var), decreasing = TRUE)[1]))
    rango <- range(var)
    varianza <- var(var)
    desviacion <- sd(var)
    cuartiles <- quantile(var, probs = c(0.25, 0.5, 0.75))
    deciles <- quantile(var, probs = seq(0, 1, by = 0.1))
    percentiles <- quantile(var, probs = seq(0, 1, by = 0.01))
    asimetria <- skewness(var)
    conf_level <- input$confidence_level
    
    conf_media <- intervalo_confianza_media(var, conf_level)
    conf_varianza <- intervalo_confianza_varianza(var, conf_level)
    
    tagList(
      h4("Estadísticas Descriptivas de Nivel de Actividad Física"),
      tags$div(style = "display: flex; flex-wrap: wrap;",
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Media:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Media"
                        tags$p(round(media, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Media"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Mediana:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Mediana"
                        tags$p(round(mediana, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Mediana"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Moda:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Moda"
                        tags$p(round(moda, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Moda"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Rango:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Rango"
                        tags$p(paste(rango, collapse = " - "), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Rango"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Varianza:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Varianza"
                        tags$p(round(varianza, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Varianza"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Desviación Estándar:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Desviación Estándar"
                        tags$p(round(desviacion, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Desviación Estándar"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Cuartil 1:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Cuartil 1"
                        tags$p(paste(round(cuartiles[1], 2)), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Cuartil 1"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Cuartil 2:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Cuartil 2"
                        tags$p(paste(round(cuartiles[2], 2)), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Cuartil 2"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Cuartil 3:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Cuartil 3"
                        tags$p(paste(round(cuartiles[3], 2)), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Cuartil 3"
               ),
               tags$div(style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
                        tags$b("Asimetría:", style = "font-size: 17px;"),  # Tamaño de letra para la etiqueta "Asimetría"
                        tags$p(round(asimetria, 2), style = "font-size: 15px;")  # Tamaño de letra para el valor de "Asimetría"
               )
      ),
      h4("Intervalos de Confianza"),  # Tamaño de letra para el título de Intervalos de Confianza
      tags$div(
        style = "display: flex; justify-content: space-between;", 
        tags$div(
          style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
          tags$p(tags$strong("Intervalo de Confianza para la Media:", style = "font-size: 17px;"), style = "font-size: 17px;"),  # Tamaño de letra para el título del intervalo de confianza de la media
          tags$p(paste0("IC: [", round(conf_media[1], 2), ", ", round(conf_media[2], 2), "]"), style = "font-size: 15px;")  # Tamaño de letra para el valor del intervalo de confianza de la media
        ),
        tags$div(
          style = "flex: 1; padding: 10px; border: 1px solid #5a9bd6; border-radius: 5px; margin: 5px; background-color: rgba(90, 155, 214, 0.4); width: 200px; height: 100px; color: #333;", 
          tags$p(tags$strong("Intervalo de Confianza para la Varianza:", style = "font-size: 17px;"), style = "font-size: 17px;"),  # Tamaño de letra para el título del intervalo de confianza de la varianza
          tags$p(paste0("IC: [", round(conf_varianza[1], 2), ", ", round(conf_varianza[2], 2), "]"), style = "font-size: 15px;")  # Tamaño de letra para el valor del intervalo de confianza de la varianza
        )
      )
    )
  })
  
  # Renderizar análisis categórico dinámico
  output$categorical_analysis <- renderUI({
    req(input$categorical_vars)
    lapply(input$categorical_vars, function(var) {
      list(
        h3(paste("Análisis de la variable:", var)),
        plotOutput(paste0("barplot_", var)),
        uiOutput(paste0("conclusion_barplot_", var)),
        plotOutput(paste0("piechart_", var)),
        uiOutput(paste0("conclusion_piechart_", var)),
        tableOutput(paste0("table_", var)),
        uiOutput(paste0("conclusion_table_", var))
      )
    })
  })
  
  # Renderizar gráficos y tablas para cada variable categórica seleccionada
  observe({
    lapply(input$categorical_vars, function(var) {
      
      output[[paste0("barplot_", var)]] <- renderPlot({
        datos <- datos_filtrados() %>% count(!!sym(var))
        ggplot(datos, aes(x = !!sym(var), y = n, fill = !!sym(var))) +
          geom_bar(stat = "identity") +
          labs(title = paste("Distribución de", var), x = var, y = "Frecuencia") +
          theme_minimal() +
          theme(legend.title = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      output[[paste0("conclusion_barplot_", var)]] <- renderUI({
        datos <- datos_filtrados() %>% count(!!sym(var))
        if (nrow(datos) > 0) {
          conclusion <- paste("El gráfico de barras muestra que la categoría con mayor frecuencia para la variable", var, 
                              "es", datos[[var]][which.max(datos$n)], "con", max(datos$n), "casos.")
          tags$div(
            style = "padding: 10px; margin: 5px; background-color: rgba(0, 255, 255, 0.2); border-radius: 5px; color: #333;",
            tags$b("Conclusión del gráfico de barras:", style = "font-size: 16px;"),
            tags$p(conclusion, style = "font-size: 14px;")
          )
        }
      })
      
      output[[paste0("piechart_", var)]] <- renderPlot({
        datos <- datos_filtrados() %>% count(!!sym(var))
        ggplot(datos, aes(x = "", y = n, fill = !!sym(var))) +
          geom_bar(width = 1, stat = "identity") +
          coord_polar(theta = "y") +
          labs(title = paste("Distribución de", var)) +
          theme_void()
      })
      
      output[[paste0("conclusion_piechart_", var)]] <- renderUI({
        datos <- datos_filtrados() %>% count(!!sym(var))
        if (nrow(datos) > 0) {
          conclusion <- paste("El gráfico circular indica que la categoría predominante para la variable", var, 
                              "es", datos[[var]][which.max(datos$n)], "con un porcentaje del", round(100 * max(datos$n) / sum(datos$n), 2), "%.")
          tags$div(
            style = "padding: 10px; margin: 5px; background-color: rgba(0, 255, 255, 0.2); border-radius: 5px; color: #333;",
            tags$b("Conclusión del gráfico circular:", style = "font-size: 16px;"),
            tags$p(conclusion, style = "font-size: 14px;")
          )
        }
      })
      
      # Tabla con frecuencias absoluta, relativa, y acumulada
      output[[paste0("table_", var)]] <- renderTable({
        datos <- datos_filtrados() %>% 
          count(!!sym(var)) %>% 
          mutate(Frecuencia_Relativa = n / sum(n),
                 Frecuencia_Acumulada = cumsum(n),
                 Frecuencia_Relativa_Acumulada = cumsum(Frecuencia_Relativa))
        datos
      })
      
      output[[paste0("conclusion_table_", var)]] <- renderUI({
        datos <- datos_filtrados() %>% count(!!sym(var))
        if (nrow(datos) > 0) {
          conclusion <- paste("La tabla de frecuencias muestra que la categoría más frecuente para la variable", var, 
                              "es", datos[[var]][which.max(datos$n)], "con una frecuencia absoluta de", max(datos$n),
                              "y una frecuencia relativa de", round(max(datos$n) / sum(datos$n), 4), ".")
          tags$div(
            style = "padding: 10px; margin: 5px; background-color: rgba(0, 255, 255, 0.2); border-radius: 5px; color: #333;",
            tags$b("Conclusión del gráfico circular:", style = "font-size: 16px;"),
            tags$p(conclusion, style = "font-size: 14px;")
          )
        }
      })
      
    })
  })
  
  # Renderizar análisis descriptivo conjunto
  output$boxplot_continuous_categorical <- renderPlot({
    req(input$continuous_var, input$categorical_var)
    ggplot(datos_filtrados(), aes_string(x = input$categorical_var, y = input$continuous_var, fill = input$categorical_var)) +
      geom_boxplot() +
      labs(title = paste("Boxplot de", input$continuous_var, "por", input$categorical_var), 
           x = input$categorical_var, 
           y = input$continuous_var) +
      theme_minimal()
  })
  
  output$barplot_continuous_categorical <- renderPlot({
    req(input$continuous_var, input$categorical_var)
    datos <- datos_filtrados() %>%
      group_by(across(all_of(input$categorical_var))) %>%
      summarise(Media = mean(get(input$continuous_var), na.rm = TRUE))
    ggplot(datos, aes_string(x = input$categorical_var, y = "Media", fill = input$categorical_var)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Promedio de", input$continuous_var, "por", input$categorical_var), 
           x = input$categorical_var, 
           y = paste("Promedio de", input$continuous_var)) +
      theme_minimal()
  })
  # Gráfico de barras apiladas entre dos variables categóricas
  output$mosaicplot_categorical_categorical <- renderPlot({
    req(input$categorical_var1, input$categorical_var2)
    ggplot(datos_filtrados(), aes_string(x = input$categorical_var1, fill = input$categorical_var2)) +
      geom_bar(position = "fill") +
      labs(title = paste("Relación entre", input$categorical_var1, "y", input$categorical_var2), 
           x = input$categorical_var1, 
           y = "Proporción",
           fill = input$categorical_var2) +
      theme_minimal()
  })
  
  
  # Tabla de contingencia entre dos variables categóricas
  output$contingency_table <- renderTable({
    req(input$categorical_var1, input$categorical_var2)
    tabla <- table(datos_filtrados()[[input$categorical_var1]], datos_filtrados()[[input$categorical_var2]])
    return(tabla)
  })
  # Renderizar conclusión
  output$conclusion_categorical_categorical <- renderUI({
    req(input$categorical_var1, input$categorical_var2)
    tabla <- table(datos_filtrados()[[input$categorical_var1]], datos_filtrados()[[input$categorical_var2]])
    
    # Análisis de la tabla de contingencia
    max_combination <- names(which.max(tabla))
    max_value <- max(tabla)
    
    conclusion <- paste(
      "El gráfico de mosaico muestra la relación entre", input$categorical_var1, "y", input$categorical_var2, ".",
      "Se observa que la combinación más frecuente es", max_combination, "con un total de", max_value, "casos.",
      "Esto indica que en la población estudiada, la mayoría de las personas con la categoría", max_combination,
      "tienen una distribución significativa en estas dos variables.",
      "Por otro lado, la tabla de contingencia nos proporciona una vista detallada de las frecuencias absolutas para cada combinación.",
      "Este análisis es útil para comprender cómo interactúan estas variables categóricas entre sí y cómo se distribuyen las categorías en la muestra."
    )
    
    tags$div(
      style = "flex: 1; padding: 10px; border: 1px solid #aac3e4; border-radius: 5px; margin: 5px; background-color: rgba(160, 185, 239, 0.6); width: 800px; height: auto; color: #333;", 
      tags$p(tags$strong("Conclusión del Análisis Categórico:", style = "font-size: 17px;"), style = "font-size: 17px;"),  # Título de la conclusión
      tags$p(conclusion, style = "font-size: 16px; margin-top: 10px;")  # Texto de la conclusión
    )
  })
  
  # Gráfico de dispersión entre dos variables numéricas
  output$scatterplot_numeric_numeric <- renderPlot({
    req(input$numeric_var1, input$numeric_var2)
    ggplot(datos_filtrados(), aes_string(x = input$numeric_var1, y = input$numeric_var2)) +
      geom_point() +
      geom_smooth(method = "lm", col = "red") +
      labs(title = paste("Relación entre", input$numeric_var1, "y", input$numeric_var2), 
           x = input$numeric_var1, 
           y = input$numeric_var2) +
      theme_minimal()
  })
  
  # Tabla de correlación entre dos variables numéricas
  output$correlation_table <- renderTable({
    req(input$numeric_var1, input$numeric_var2)
    correlacion <- cor(datos_filtrados()[[input$numeric_var1]], datos_filtrados()[[input$numeric_var2]], use = "complete.obs")
    tabla_correlacion <- data.frame(
      Variable1 = input$numeric_var1,
      Variable2 = input$numeric_var2,
      Correlación = round(correlacion, 2)
    )
    return(tabla_correlacion)
  })
  
  # Renderizar conclusión
  output$conclusion_numeric_numeric <- renderUI({
    req(input$numeric_var1, input$numeric_var2)
    correlacion <- cor(datos_filtrados()[[input$numeric_var1]], datos_filtrados()[[input$numeric_var2]], use = "complete.obs")
    
    # Interpretación de la correlación
    interpretacion <- if (abs(correlacion) > 0.7) {
      "una fuerte relación"
    } else if (abs(correlacion) > 0.4) {
      "una relación moderada"
    } else {
      "una relación débil o casi nula"
    }
    
    direccion <- if (correlacion > 0) {
      "positiva"
    } else if (correlacion < 0) {
      "negativa"
    } else {
      "nula"
    }
    
    conclusion <- paste(
      "El gráfico de dispersión muestra la relación entre", input$numeric_var1, "y", input$numeric_var2, ".",
      "La línea roja indica la tendencia lineal estimada entre estas dos variables.",
      "El coeficiente de correlación calculado es de", round(correlacion, 2), ", lo que sugiere", interpretacion, "de tipo", direccion, "entre las variables.",
      "Este resultado nos permite entender cómo", input$numeric_var1, "y", input$numeric_var2, "están relacionadas en la muestra analizada.",
      "Una correlación", interpretacion, direccion, "implica que al aumentar", input$numeric_var1, 
      if (direccion == "positiva") {"también aumenta"} else if (direccion == "negativa") {"disminuye"} else {""},
      input$numeric_var2, "."
    )
    
    tags$div(
      style = "flex: 1; padding: 10px; border: 1px solid #aac3e4; border-radius: 5px; margin: 5px; background-color: rgba(160, 185, 239, 0.6); width: 800px; height: auto; color: #333;", 
      tags$p(tags$strong("Conclusión del Análisis Categórico:", style = "font-size: 17px;"), style = "font-size: 17px;"),  # Título de la conclusión
      tags$p(conclusion, style = "font-size: 16px; margin-top: 10px;")  # Texto de la conclusión
    )
  })
  
}


shinyApp(ui = ui, server = server)
