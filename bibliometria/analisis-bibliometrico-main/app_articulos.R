library(shiny)
library(bslib)
library(pdftools)
library(httr)
library(jsonlite)
library(DT)
library(plotly)
library(writexl)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(rsconnect)

openai_api_key <- "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"

# Función mejorada para extraer autores con GPT
extraer_autores_gpt <- function(texto_pdf) {
  # Preprocesamiento del texto
  texto_limpio <- texto_pdf %>%
    str_replace_all("\n", " ") %>%       # Eliminar saltos de línea
    str_replace_all("\\s+", " ") %>%     # Normalizar espacios
    str_sub(1, 15000) %>%               # Limitar tamaño por restricciones de API
    str_trim()

  if (nchar(texto_limpio) < 100) {
    return(data.frame(Autor = "Texto demasiado corto para análisis", Frecuencia = NA))
  }

  # Intento de conexión con manejo robusto de errores
  respuesta <- tryCatch({
    POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(
        "Authorization" = paste("Bearer", openai_api_key),
        "Content-Type" = "application/json"
      ),
      body = toJSON(list(
        model = "gpt-3.5-turbo",  # Más económico que gpt-4 para este caso
        messages = list(
          list(
            role = "system",
            content = paste(
              "Eres un experto en bibliometría. Extrae SOLO los nombres completos de autores",
              "del siguiente texto académico. Devuelve ÚNICAMENTE una lista de nombres",
              "separados por saltos de línea, sin números, viñetas o texto adicional."
            )
          ),
          list(
            role = "user",
            content = paste("Texto a analizar:", texto_limpio)
          )
        ),
        temperature = 0.3,  # Menor aleatoriedad
        max_tokens = 1000
      ), auto_unbox = TRUE),
      timeout(30)
    )
  }, error = function(e) {
    warning("Error al conectar con OpenAI: ", e$message)
    return(NULL)
  })

  # Verificación de respuesta
  if (is.null(respuesta) || status_code(respuesta) != 200) {
    error_msg <- if (!is.null(respuesta)) {
      paste("Error API:", status_code(respuesta), "-", content(respuesta, "text"))
    } else {
      "Error de conexión con la API"
    }
    return(data.frame(Autor = error_msg, Frecuencia = NA))
  }

  # Procesamiento de respuesta
  contenido <- content(respuesta, as = "parsed")
  texto_respuesta <- contenido$choices[[1]]$message$content

  autores <- texto_respuesta %>%
    str_split("\\n") %>%
    unlist() %>%
    str_trim() %>%
    discard(~ .x == "" | str_detect(.x, "^(Autor|Nombre|Lista|$)"))

  # Validación de resultados
  if (length(autores) == 0) {
    return(data.frame(Autor = "No se detectaron patrones de autores", Frecuencia = NA))
  }

  # Creación de dataframe con frecuencias
  autores_df <- data.frame(Autor = autores) %>%
    count(Autor, name = "Frecuencia") %>%
    arrange(desc(Frecuencia)) %>%
    mutate(Autor = as.character(Autor))

  return(autores_df)
}

extraer_metodologias_gpt <- function(texto_pdf) {
  texto_limpio <- texto_pdf %>%
    str_replace_all("\n", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_sub(1, 15000) %>%
    str_trim()

  if (nchar(texto_limpio) < 100) {
    return(data.frame(Metodologia = "Texto demasiado corto", Frecuencia = NA))
  }

  respuesta <- tryCatch({
    POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(
        "Authorization" = paste("Bearer", openai_api_key),
        "Content-Type" = "application/json"
      ),
      body = toJSON(list(
        model = "gpt-3.5-turbo",
        messages = list(
          list(
            role = "system",
            content = paste(
              "Eres un experto en metodología científica.",
              "Del siguiente texto académico, extrae SOLO las metodologías estadísticas utilizadas.",
              "Devuelve una lista simple de nombres de metodologías como ANOVA, chi-cuadrado, regresión lineal, etc.",
              "No agregues explicaciones ni texto adicional, solo los nombres en una lista con salto de línea."
            )
          ),
          list(
            role = "user",
            content = texto_limpio
          )
        ),
        temperature = 0.2,
        max_tokens = 1000
      ), auto_unbox = TRUE),
      timeout(30)
    )
  }, error = function(e) {
    warning("Error al conectar con OpenAI: ", e$message)
    return(NULL)
  })

  if (is.null(respuesta) || status_code(respuesta) != 200) {
    return(data.frame(Metodologia = "Error en API", Frecuencia = NA))
  }

  contenido <- content(respuesta, as = "parsed")
  texto_respuesta <- contenido$choices[[1]]$message$content

  metodologias <- texto_respuesta %>%
    str_split("\\n") %>%
    unlist() %>%
    str_trim() %>%
    discard(~ .x == "")

  if (length(metodologias) == 0) {
    return(data.frame(Metodologia = "No se encontraron metodologías", Frecuencia = NA))
  }

  data.frame(Metodologia = metodologias) %>%
    count(Metodologia, name = "Frecuencia") %>%
    arrange(desc(Frecuencia))
}

# UI mejorada con más funcionalidades
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2c3e50",
    secondary = "#18bc9c",
    success = "#18bc9c",
    base_font = font_google("Roboto")
  ),

  titlePanel(div(
    img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Flat_tick_icon.svg/1200px-Flat_tick_icon.svg.png",
        height = 50, style = "margin-right:15px;"),
    "Analizador Bibliométrico Profesional"
  ), windowTitle = "Bibliometría"),

  navbarPage(
    NULL,
    tabPanel(
      "Análisis",
      icon = icon("file-import"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fileInput(
            "archivo",
            "Subir documentos PDF",
            accept = ".pdf",
            multiple = TRUE,
            buttonLabel = "Examinar...",
            placeholder = "Seleccione archivos"
          ),
          tags$hr(),
          sliderInput(
            "limite_autores",
            "Número de autores a mostrar:",
            min = 5,
            max = 50,
            value = 15
          ),
          actionButton(
            "procesar",
            "Analizar Autores",
            icon = icon("search"),
            class = "btn-primary"
          ),
          tags$hr(),
          downloadButton(
            "descargar",
            "Exportar Resultados",
            class = "btn-success"
          ),
          tags$hr(),
          helpText(
            "Nota: El análisis utiliza IA para extraer autores.",
            "Archivos grandes pueden tardar varios segundos."
          )
        ),

        mainPanel(
          width = 9,
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Resultados",
              icon = icon("table"),
              h3("Autores identificados"),
              DTOutput("tabla_autores"),
              tags$br(),
              h3("Distribución de autores por frecuencia"),
              plotlyOutput("grafico_autores")
            ),
            tabPanel(
              "Resumen",
              icon = icon("chart-bar"),
              fluidRow(
                column(
                  6,
                  h4("Estadísticas básicas"),
                  verbatimTextOutput("resumen_estadistico")
                ),
                column(
                  6,
                  h4("Top instituciones"),
                  plotlyOutput("grafico_instituciones", height = "300px")
                )
              )
            ),
            tabPanel(
              "Metodologías Estadísticas",
              icon = icon("flask"),
              h3("Metodologías encontradas"),
              DTOutput("tabla_metodologias"),
              tags$br(),
              plotlyOutput("grafico_metodologias")
            )
          )
        )
      )
    ),
    tabPanel(
      "Instrucciones",
      icon = icon("info-circle"),
      includeMarkdown("instrucciones.md")  # Crear este archivo
    )
  )
)

# Server con mejor manejo de errores y funcionalidades adicionales
server <- function(input, output, session) {
  # Reactive para almacenar datos
  datos <- reactiveValues(
    autores = NULL,
    texto_original = NULL,
    instituciones = NULL,
    metodologias = NULL
  )

  # Procesamiento de PDFs
  texto_pdf <- reactive({
    req(input$archivo)

    tryCatch({
      withProgress(
        message = "Extrayendo texto de PDFs...",
        value = 0,
        {
          textos <- lapply(input$archivo$datapath, function(file) {
            incProgress(1/length(input$archivo$datapath))
            pdf_text(file) %>% paste(collapse = " ")
          })

          texto_completo <- paste(unlist(textos), collapse = " ")
          datos$texto_original <- texto_completo
          return(texto_completo)
        }
      )
    }, error = function(e) {
      showNotification(
        paste("Error al leer PDF:", e$message),
        type = "error",
        duration = NULL
      )
      return(NULL)
    })
  })

  # Procesamiento con GPT
  observeEvent(input$procesar, {
    req(texto_pdf())

    showModal(modalDialog(
      title = "Procesando con IA",
      "Analizando contenido con OpenAI... Esto puede tomar unos segundos.",
      footer = NULL,
      easyClose = FALSE
    ))

    tryCatch({
      resultado <- extraer_autores_gpt(texto_pdf())
      # Extraer metodologías estadísticas
      datos$metodologias <- extraer_metodologias_gpt(texto_pdf())

      if (!is.null(resultado) && nrow(resultado) > 0) {
        datos$autores <- resultado

        # Extracción simple de instituciones (ejemplo básico)
        instituciones <- texto_pdf() %>%
          str_extract_all("[A-Z][a-zA-Z ]+University|[A-Z][a-zA-Z ]+Institute") %>%
          unlist()

        if (length(instituciones) > 0) {
          datos$instituciones <- data.frame(Institucion = instituciones) %>%
            count(Institucion, name = "Frecuencia") %>%
            arrange(desc(Frecuencia))
        }
      }
    }, error = function(e) {
      showNotification(
        paste("Error en el análisis:", e$message),
        type = "error",
        duration = NULL
      )
    }, finally = {
      removeModal()
    })
  })

  # Renderizado de tabla
  output$tabla_autores <- renderDT({
    req(datos$autores)

    datatable(
      datos$autores,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 20, 50),
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel'),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      ),
      rownames = FALSE,
      class = "hover stripe nowrap"
    ) %>%
      formatStyle("Frecuencia", fontWeight = "bold")
  })

  # Gráfico de autores
  output$grafico_autores <- renderPlotly({
    req(datos$autores)

    top_autores <- head(datos$autores, input$limite_autores)

    plot_ly(
      top_autores,
      x = ~Frecuencia,
      y = ~reorder(Autor, Frecuencia),
      type = "bar",
      orientation = "h",
      marker = list(color = "#18bc9c"),
      hoverinfo = "text",
      text = ~paste("Autor:", Autor, "<br>Frecuencia:", Frecuencia)
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "Frecuencia de aparición"),
        yaxis = list(title = ""),
        margin = list(l = 150),
        hoverlabel = list(bgcolor = "white")
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Gráfico de instituciones
  output$grafico_instituciones <- renderPlotly({
    req(datos$instituciones)

    plot_ly(
      head(datos$instituciones, 10),
      labels = ~Institucion,
      values = ~Frecuencia,
      type = "pie",
      hole = 0.4,
      textinfo = "label+percent",
      hoverinfo = "label+value+percent"
    ) %>%
      layout(showlegend = FALSE)
  })

  # Resumen estadístico
  output$resumen_estadistico <- renderPrint({
    req(datos$autores)

    cat("Total de autores únicos:", nrow(datos$autores), "\n")
    cat("Total de apariciones:", sum(datos$autores$Frecuencia), "\n")
    cat("\nTop 5 autores:\n")
    print(head(datos$autores, 5))

    if (!is.null(datos$instituciones)) {
      cat("\nTotal de instituciones:", nrow(datos$instituciones), "\n")
      cat("Institución más frecuente:", datos$instituciones$Institucion[1], "\n")
    }
  })
  output$tabla_metodologias <- renderDT({
    req(datos$metodologias)

    datatable(
      datos$metodologias,
      options = list(
        pageLength = 10,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      ),
      rownames = FALSE
    )
  })

  output$grafico_metodologias <- renderPlotly({
    req(datos$metodologias)

    plot_ly(
      datos$metodologias,
      x = ~Frecuencia,
      y = ~reorder(Metodologia, Frecuencia),
      type = "bar",
      orientation = "h",
      marker = list(color = "#2c3e50")
    ) %>%
      layout(
        xaxis = list(title = "Frecuencia"),
        yaxis = list(title = ""),
        margin = list(l = 120)
      )
  })

  # Descarga de resultados
  output$descargar <- downloadHandler(
    filename = function() {
      paste("resultados_bibliometria_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(datos$autores)

      lista_export <- list(
        Autores = datos$autores,
        Resumen = data.frame(
          Metricas = c("Autores únicos", "Total apariciones"),
          Valor = c(nrow(datos$autores), sum(datos$autores$Frecuencia))
        )
      )

      if (!is.null(datos$instituciones)) {
        lista_export$Instituciones <- datos$instituciones
      }
      if (!is.null(datos$metodologias)) {
        lista_export$Metodologias <- datos$metodologias
      }

      write_xlsx(lista_export, path = file)
    }
  )
}

shinyApp(ui = ui, server = server)
