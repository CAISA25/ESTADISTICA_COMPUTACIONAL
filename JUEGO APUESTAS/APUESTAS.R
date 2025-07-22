library(shiny)
library(shinythemes)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(DescTools)
library(tidyr)
library(stats)
library(coin)
library(knitr)
library(rmarkdown)
library(plotly)
library(gridExtra)
library(scales)

# Función segura para calcular la moda
moda_segura <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  freq <- table(x)
  max_freq <- max(freq)
  if (max_freq == 1) {
    return("No hay moda")
  }
  moda <- names(freq[freq == max_freq])
  if (length(moda) == 1) {
    return(moda)
  } else {
    return(paste(moda, collapse = ", "))
  }
}

# Función para crear gráfico de pastel
crear_grafico_pastel <- function(datos, variable) {
  freq_table <- table(datos)
  df_pie <- data.frame(
    categoria = names(freq_table),
    frecuencia = as.numeric(freq_table),
    porcentaje = round(as.numeric(freq_table) / sum(freq_table) * 100, 1)
  )

  ggplot(df_pie, aes(x = "", y = frecuencia, fill = categoria)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(porcentaje, "%")),
              position = position_stack(vjust = 0.5)) +
    theme_void() +
    labs(title = paste("Gráfico de Pastel:", variable),
         fill = "Categorías") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

# Función para crear ojiva
crear_ojiva <- function(datos, variable) {
  if (is.numeric(datos)) {
    # Crear intervalos
    breaks <- pretty(datos, n = 10)
    hist_data <- hist(datos, breaks = breaks, plot = FALSE)

    # Calcular frecuencias acumuladas
    freq_cum <- cumsum(hist_data$counts)

    # Crear dataframe para ojiva
    df_ojiva <- data.frame(
      limite_superior = hist_data$breaks[-1],
      freq_acumulada = freq_cum
    )

    ggplot(df_ojiva, aes(x = limite_superior, y = freq_acumulada)) +
      geom_line(color = "#E74C3C", size = 1.2) +
      geom_point(color = "#C0392B", size = 3) +
      theme_minimal() +
      labs(title = paste("Ojiva de", variable),
           x = variable,
           y = "Frecuencia Acumulada") +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  } else {
    # Para variables categóricas
    freq_table <- table(datos)
    freq_cum <- cumsum(freq_table)

    df_ojiva <- data.frame(
      categoria = names(freq_cum),
      freq_acumulada = as.numeric(freq_cum)
    )

    ggplot(df_ojiva, aes(x = factor(categoria, levels = categoria), y = freq_acumulada)) +
      geom_line(aes(group = 1), color = "#E74C3C", size = 1.2) +
      geom_point(color = "#C0392B", size = 3) +
      theme_minimal() +
      labs(title = paste("Ojiva de", variable),
           x = variable,
           y = "Frecuencia Acumulada") +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  }
}

ui <- dashboardPage(
  dashboardHeader(title = "Análisis Estadístico Avanzado"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Importar Datos", tabName = "importar", icon = icon("file-import")),
      menuItem("Estadísticas Descriptivas", tabName = "descriptivas", icon = icon("chart-bar")),
      menuItem("Gráficos por Variable", tabName = "graficos_individual", icon = icon("chart-pie")),
      menuItem("Pruebas e Interpretación", tabName = "pruebas", icon = icon("flask")),
      menuItem("Extras", tabName = "extras", icon = icon("plus-circle")),
      menuItem("Nuevas Pruebas", tabName = "nuevas_pruebas", icon = icon("plus")),
      menuItem("Exportar Reporte", tabName = "exportar", icon = icon("file-pdf"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("importar",
              fluidRow(
                box(title = "Carga de archivo", width = 4, solidHeader = TRUE, status = "primary",
                    fileInput("archivo", "Sube archivo .csv o .xlsx", accept = c(".csv", ".xlsx")),
                    uiOutput("seleccion_vars"),
                    actionButton("analizar", "Iniciar análisis", icon = icon("play"), class = "btn btn-primary")
                ),
                box(title = "Vista previa de datos", width = 8, solidHeader = TRUE, status = "info",
                    tableOutput("vista_datos"))
              )
      ),
      tabItem("descriptivas",
              uiOutput("estadisticas_ui"),
              uiOutput("graficos_ui"),
              downloadButton("descargar_estadisticas", "📥 Descargar estadísticas")
      ),
      tabItem("graficos_individual",
              fluidRow(
                box(title = "Seleccionar Variable", width = 12, solidHeader = TRUE, status = "primary",
                    selectInput("var_individual", "Selecciona una variable para análisis detallado:",
                                choices = NULL),
                    hr(),
                    h4("Gráficos Estadísticos"),
                    tabsetPanel(
                      tabPanel("Histograma/Barras", plotOutput("plot_principal", height = "400px")),
                      tabPanel("Gráfico de Pastel", plotOutput("plot_pastel", height = "400px")),
                      tabPanel("Boxplot", plotOutput("plot_boxplot", height = "400px")),
                      tabPanel("Ojiva", plotOutput("plot_ojiva", height = "400px")),
                      tabPanel("Estadísticas", tableOutput("stats_individual"))
                    )
                )
              )
      ),
      tabItem("pruebas",
              box(title = "Resultado de prueba", width = 12, solidHeader = TRUE, status = "success",
                  verbatimTextOutput("resultado_prueba"),
                  h4("📌 Interpretación:"),
                  verbatimTextOutput("texto_interpretacion"))
      ),
      tabItem("extras",
              fluidRow(
                box(title = "Análisis de normalidad (Shapiro-Wilk)", width = 6, solidHeader = TRUE, status = "warning",
                    verbatimTextOutput("normalidad")),
                box(title = "Valores atípicos detectados", width = 6, solidHeader = TRUE, status = "danger",
                    verbatimTextOutput("outliers"))
              ),
              fluidRow(
                box(title = "Boxplot comparativo", width = 12, solidHeader = TRUE, status = "info",
                    plotOutput("boxplot_grupos"))
              )
      ),
      tabItem("nuevas_pruebas",
              fluidRow(
                box(title = "Prueba Chi-cuadrada", width = 6, solidHeader = TRUE, status = "warning",
                    verbatimTextOutput("chi_cuadrada")),
                box(title = "Teorema del Límite Central", width = 6, solidHeader = TRUE, status = "info",
                    plotOutput("tlc_plot"),
                    verbatimTextOutput("tlc"))
              ),
              fluidRow(
                box(title = "Prueba de Wilcoxon", width = 6, solidHeader = TRUE, status = "danger",
                    verbatimTextOutput("wilcoxon")),
                box(title = "Correlación de Spearman y Pearson", width = 6, solidHeader = TRUE, status = "success",
                    verbatimTextOutput("correlacion"))
              )
      ),
      tabItem("exportar",
              fluidRow(
                box(title = "Exportar Reporte Completo", width = 12, solidHeader = TRUE, status = "success",
                    h4("📄 Generar Reporte PDF"),
                    p("Este reporte incluirá:"),
                    tags$ul(
                      tags$li("Resumen de datos"),
                      tags$li("Estadísticas descriptivas de todas las variables"),
                      tags$li("Gráficos estadísticos"),
                      tags$li("Pruebas estadísticas realizadas"),
                      tags$li("Análisis de normalidad y outliers")
                    ),
                    br(),
                    downloadButton("descargar_pdf", "📥 Descargar Reporte PDF",
                                   class = "btn btn-success btn-lg"),
                    br(), br(),
                    verbatimTextOutput("status_pdf")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  datos <- reactive({
    req(input$archivo)
    tryCatch({
      if (grepl(".csv$", input$archivo$name, ignore.case = TRUE)) {
        read.csv(input$archivo$datapath)
      } else {
        read_excel(input$archivo$datapath)
      }
    }, error = function(e) {
      showNotification("Error al leer el archivo", type = "error")
      NULL
    })
  })

  output$vista_datos <- renderTable({
    req(datos())
    head(datos())
  })

  output$seleccion_vars <- renderUI({
    req(datos())
    checkboxGroupInput("variables", "Selecciona variables para analizar:",
                       choices = names(datos()), selected = names(datos())[1:2])
  })

  # Actualizar choices para variable individual
  observe({
    req(datos())
    updateSelectInput(session, "var_individual",
                      choices = names(datos()),
                      selected = names(datos())[1])
  })

  output$estadisticas_ui <- renderUI({
    req(input$variables)
    tablas <- lapply(input$variables, function(var) {
      datos_col <- datos()[[var]]
      tipo <- if (is.numeric(datos_col)) "numérica" else "categórica"
      tagList(
        h4(paste("Variable:", var, "-", tipo)),
        tableOutput(paste0("tabla_", var))
      )
    })
    do.call(tagList, tablas)
  })

  observe({
    req(input$variables)
    for (var in input$variables) {
      local({
        v <- var
        output[[paste0("tabla_", v)]] <- renderTable({
          datos_col <- datos()[[v]]
          if (is.numeric(datos_col)) {
            data.frame(
              Estadístico = c("Media", "Mediana", "Moda", "Mínimo", "Máximo",
                              "Rango", "Desv. Estandar", "Coef. Variación"),
              Valor = c(
                mean(datos_col, na.rm = TRUE),
                median(datos_col, na.rm = TRUE),
                moda_segura(datos_col),
                min(datos_col, na.rm = TRUE),
                max(datos_col, na.rm = TRUE),
                max(datos_col, na.rm = TRUE) - min(datos_col, na.rm = TRUE),
                sd(datos_col, na.rm = TRUE),
                round(sd(datos_col, na.rm = TRUE) / mean(datos_col, na.rm = TRUE), 3)
              )
            )
          } else {
            as.data.frame(table(datos_col)) %>%
              rename(Categoría = 1, Frecuencia = 2)
          }
        }, rownames = FALSE)
      })
    }
  })

  output$graficos_ui <- renderUI({
    req(input$variables)
    plots <- lapply(input$variables, function(var) {
      tagList(
        h4(paste("Gráfico:", var)),
        plotOutput(paste0("plot_", var)),
        br()
      )
    })
    do.call(tagList, plots)
  })

  observe({
    req(input$variables)
    for (var in input$variables) {
      local({
        v <- var
        output[[paste0("plot_", v)]] <- renderPlot({
          datos_col <- datos()[[v]]
          if (is.numeric(datos_col)) {
            ggplot(data.frame(x = datos_col), aes(x = x)) +
              geom_histogram(bins = 15, fill = "#2E86C1", color = "white") +
              geom_density(aes(y = ..count..), color = "#1F618D", size = 1) +
              theme_minimal() +
              labs(title = paste("Histograma de", v), x = v, y = "Frecuencia")
          } else {
            ggplot(data.frame(x = datos_col), aes(x = x)) +
              geom_bar(fill = "#E67E22") +
              theme_minimal() +
              labs(title = paste("Gráfico de barras de", v), x = v) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          }
        })
      })
    }
  })

  # Gráficos individuales detallados
  output$plot_principal <- renderPlot({
    req(input$var_individual)
    datos_col <- datos()[[input$var_individual]]

    if (is.numeric(datos_col)) {
      ggplot(data.frame(x = datos_col), aes(x = x)) +
        geom_histogram(bins = 15, fill = "#3498DB", color = "white", alpha = 0.8) +
        geom_density(aes(y = ..count..), color = "#E74C3C", size = 1.2) +
        theme_minimal() +
        labs(title = paste("Histograma de", input$var_individual),
             x = input$var_individual, y = "Frecuencia") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    } else {
      ggplot(data.frame(x = datos_col), aes(x = x)) +
        geom_bar(fill = "#2ECC71", alpha = 0.8) +
        theme_minimal() +
        labs(title = paste("Gráfico de barras de", input$var_individual),
             x = input$var_individual, y = "Frecuencia") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    }
  })

  output$plot_pastel <- renderPlot({
    req(input$var_individual)
    datos_col <- datos()[[input$var_individual]]

    if (!is.numeric(datos_col)) {
      crear_grafico_pastel(datos_col, input$var_individual)
    } else {
      # Para variables numéricas, crear categorías
      datos_cat <- cut(datos_col, breaks = 5, labels = c("Muy Bajo", "Bajo", "Medio", "Alto", "Muy Alto"))
      crear_grafico_pastel(datos_cat, input$var_individual)
    }
  })

  output$plot_boxplot <- renderPlot({
    req(input$var_individual)
    datos_col <- datos()[[input$var_individual]]

    if (is.numeric(datos_col)) {
      ggplot(data.frame(x = datos_col), aes(y = x)) +
        geom_boxplot(fill = "#9B59B6", alpha = 0.8, outlier.color = "#E74C3C") +
        theme_minimal() +
        labs(title = paste("Boxplot de", input$var_individual),
             y = input$var_individual) +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
    } else {
      # Para variables categóricas, mostrar gráfico de barras horizontal
      freq_table <- table(datos_col)
      df_bar <- data.frame(
        categoria = names(freq_table),
        frecuencia = as.numeric(freq_table)
      )

      ggplot(df_bar, aes(x = reorder(categoria, frecuencia), y = frecuencia)) +
        geom_col(fill = "#9B59B6", alpha = 0.8) +
        coord_flip() +
        theme_minimal() +
        labs(title = paste("Gráfico horizontal de", input$var_individual),
             x = input$var_individual, y = "Frecuencia") +
        theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    }
  })

  output$plot_ojiva <- renderPlot({
    req(input$var_individual)
    datos_col <- datos()[[input$var_individual]]
    crear_ojiva(datos_col, input$var_individual)
  })

  output$stats_individual <- renderTable({
    req(input$var_individual)
    datos_col <- datos()[[input$var_individual]]

    if (is.numeric(datos_col)) {
      data.frame(
        Estadístico = c("Media", "Mediana", "Moda", "Mínimo", "Máximo", "Q1", "Q3",
                        "Rango", "Desv. Estandar", "Varianza", "Coef. Variación", "Asimetría"),
        Valor = c(
          round(mean(datos_col, na.rm = TRUE), 3),
          round(median(datos_col, na.rm = TRUE), 3),
          moda_segura(datos_col),
          round(min(datos_col, na.rm = TRUE), 3),
          round(max(datos_col, na.rm = TRUE), 3),
          round(quantile(datos_col, 0.25, na.rm = TRUE), 3),
          round(quantile(datos_col, 0.75, na.rm = TRUE), 3),
          round(max(datos_col, na.rm = TRUE) - min(datos_col, na.rm = TRUE), 3),
          round(sd(datos_col, na.rm = TRUE), 3),
          round(var(datos_col, na.rm = TRUE), 3),
          round(sd(datos_col, na.rm = TRUE) / mean(datos_col, na.rm = TRUE), 3),
          round(mean((datos_col - mean(datos_col, na.rm = TRUE))^3, na.rm = TRUE) /
                  sd(datos_col, na.rm = TRUE)^3, 3)
        )
      )
    } else {
      freq_table <- table(datos_col)
      prop_table <- prop.table(freq_table)

      data.frame(
        Categoría = names(freq_table),
        Frecuencia = as.numeric(freq_table),
        Proporción = round(as.numeric(prop_table), 3),
        Porcentaje = paste0(round(as.numeric(prop_table) * 100, 1), "%")
      )
    }
  }, rownames = FALSE)

  # Resto del código original para pruebas estadísticas...
  prueba_resultado <- reactive({
    req(input$variables)
    df <- datos()
    vars <- input$variables

    if (length(vars) != 2) return(NULL)

    var1 <- df[[vars[1]]]
    var2 <- df[[vars[2]]]

    if (is.numeric(var1) && is.numeric(var2)) {
      return(cor.test(var1, var2, method = "pearson"))
    } else if (is.factor(var1) && is.factor(var2) && length(levels(var1)) == 2 && length(levels(var2)) == 2) {
      tabla <- table(var1, var2)
      if (all(dim(tabla) == c(2, 2))) {
        return(mcnemar.test(tabla))
      }
    } else if (all(sapply(df[vars], function(x) all(x %in% c(0, 1))))) {
      return(CochranQTest(as.matrix(df[vars])))
    } else if (is.numeric(var1) && (is.factor(var2) || is.character(var2))) {
      var2 <- as.factor(var2)
      if (length(levels(var2)) == 2) {
        return(t.test(var1 ~ var2))
      } else if (length(levels(var2)) > 2) {
        aov_model <- aov(var1 ~ var2)
        tukey <- TukeyHSD(aov_model)
        return(list(aov_model = aov_model, tukey = tukey))
      }
    } else if (is.numeric(var2) && (is.factor(var1) || is.character(var1))) {
      var1 <- as.factor(var1)
      if (length(levels(var1)) == 2) {
        return(t.test(var2 ~ var1))
      } else if (length(levels(var1)) > 2) {
        aov_model <- aov(var2 ~ var1)
        tukey <- TukeyHSD(aov_model)
        return(list(aov_model = aov_model, tukey = tukey))
      }
    }
    return(NULL)
  })

  output$resultado_prueba <- renderPrint({
    res <- prueba_resultado()
    if (is.null(res)) {
      cat("❌ No se pudo aplicar la prueba. Selecciona variables adecuadas.")
    } else {
      print(res)
    }
  })

  output$texto_interpretacion <- renderPrint({
    res <- prueba_resultado()
    if (is.null(res)) {
      cat("No se puede interpretar. Asegúrate de seleccionar variables válidas.")
    } else if (inherits(res, "htest")) {
      p <- res$p.value
      if (p < 0.05) {
        cat("✅ Diferencia o relación significativa encontrada. (p =", round(p, 4), ")")
      } else {
        cat("ℹ️ No hay diferencia o relación significativa. (p =", round(p, 4), ")")
      }
    } else if (is.list(res)) {
      p <- summary(res$aov_model)[[1]][["Pr(>F)"]][1]
      if (p < 0.05) {
        cat("✅ ANOVA muestra diferencias significativas entre grupos. (p =", round(p, 4), ")\n✔️ Revisa Tukey para saber qué grupos difieren.")
      } else {
        cat("ℹ️ ANOVA no muestra diferencias significativas entre grupos. (p =", round(p, 4), ")")
      }
    }
  })

  # Análisis de normalidad (Shapiro-Wilk)
  output$normalidad <- renderPrint({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.numeric(datos_col) && length(datos_col) >= 3 && length(datos_col) <= 5000) {
      normalidad_test <- shapiro.test(datos_col)
      cat("Resultado de la prueba de normalidad (Shapiro-Wilk):\n")
      cat("Valor p: ", round(normalidad_test$p.value, 4), "\n")
      if (normalidad_test$p.value < 0.05) {
        cat("❌ La variable NO sigue una distribución normal.\n")
      } else {
        cat("✅ La variable sigue una distribución normal.\n")
      }
    } else {
      cat("Selecciona una variable numérica con entre 3 y 5000 observaciones para el análisis de normalidad.")
    }
  })

  # Detección de valores atípicos
  output$outliers <- renderPrint({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.numeric(datos_col)) {
      q1 <- quantile(datos_col, 0.25, na.rm = TRUE)
      q3 <- quantile(datos_col, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      outliers <- datos_col[datos_col < (q1 - 1.5 * iqr) | datos_col > (q3 + 1.5 * iqr)]

      cat("Detección de valores atípicos:\n")
      cat("Q1:", q1, "| Q3:", q3, "| IQR:", iqr, "\n")
      if (length(outliers) > 0) {
        cat("Se encontraron", length(outliers), "valores atípicos:\n")
        print(sort(outliers))
      } else {
        cat("No se detectaron valores atípicos.\n")
      }
    } else {
      cat("Selecciona una variable numérica para detectar valores atípicos.")
    }
  })

  # Boxplot comparativo
  output$boxplot_grupos <- renderPlot({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.numeric(datos_col)) {
      ggplot(datos(), aes(y = datos_col)) +
        geom_boxplot(fill = "#2E86C1", color = "#1F618D") +
        theme_minimal() +
        labs(title = paste("Boxplot de", var), y = var) +
        theme(axis.text.x = element_blank())
    }
  })

  # Prueba Chi-cuadrada
  output$chi_cuadrada <- renderPrint({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.factor(datos_col) || is.character(datos_col)) {
      tabla <- table(datos_col)
      if (length(tabla) > 1) {
        chisq_result <- chisq.test(tabla)
        cat("Resultado de la prueba Chi-cuadrada:\n")
        cat("Estadístico Chi-cuadrada: ", round(chisq_result$statistic, 3), "\n")
        cat("Grados de libertad: ", chisq_result$parameter, "\n")
        cat("Valor p: ", round(chisq_result$p.value, 4), "\n")
        cat("\nFrecuencias esperadas:\n")
        print(chisq_result$expected)
        cat("\n")
        if (chisq_result$p.value < 0.05) {
          cat("✅ La distribución NO es uniforme (p < 0.05).\n")
        } else {
          cat("ℹ️ No hay evidencia para rechazar la uniformidad (p >= 0.05).\n")
        }
      } else {
        cat("Se necesitan al menos 2 categorías para la prueba Chi-cuadrada.")
      }
    } else {
      cat("Selecciona una variable categórica para la prueba Chi-cuadrada.")
    }
  })

  # Teorema del Límite Central
  output$tlc_plot <- renderPlot({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.numeric(datos_col)) {
      set.seed(123)
      muestras <- replicate(1000, mean(sample(datos_col, size = min(30, length(datos_col)), replace = TRUE)))

      ggplot(data.frame(x = muestras), aes(x = x)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "#3498DB", color = "white") +
        geom_density(color = "#2874A6", size = 1) +
        theme_minimal() +
        labs(title = "Distribución de medias muestrales",
             subtitle = "Teorema del Límite Central",
             x = "Media muestral", y = "Densidad")
    }
  })

  output$tlc <- renderPrint({
    req(input$variables)
    var <- input$variables[1]
    datos_col <- datos()[[var]]
    if (is.numeric(datos_col)) {
      set.seed(123)
      muestras <- replicate(1000, mean(sample(datos_col, length(datos_col), replace = TRUE)))
      media_muestral <- mean(muestras)
      sd_muestral <- sd(muestras)

      cat("Teorema del Límite Central:\n")
      cat("Media poblacional: ", round(mean(datos_col, na.rm = TRUE), 3), "\n")
      cat("Media de las medias muestrales: ", round(media_muestral, 3), "\n")
      cat("Desviación estándar poblacional: ", round(sd(datos_col, na.rm = TRUE), 3), "\n")
      cat("Desviación estándar de las medias muestrales: ", round(sd_muestral, 3), "\n")
      cat("La distribución de las medias muestrales debería aproximarse a una normal.\n")
    } else {
      cat("Selecciona una variable numérica para realizar el TLC.")
    }
  })

  # Prueba de Wilcoxon
  output$wilcoxon <- renderPrint({
    req(input$variables)
    if (length(input$variables) == 2) {
      var1 <- datos()[[input$variables[1]]]
      var2 <- datos()[[input$variables[2]]]
      if (is.numeric(var1) && is.numeric(var2)) {
        wilcox_test <- wilcox.test(var1, var2)
        cat("Prueba de Wilcoxon (Mann-Whitney):\n")
        cat("Estadístico W: ", wilcox_test$statistic, "\n")
        cat("Valor p: ", round(wilcox_test$p.value, 4), "\n")
        if (wilcox_test$p.value < 0.05) {
          cat("✅ Existe diferencia significativa entre las distribuciones.\n")
        } else {
          cat("ℹ️ No hay diferencia significativa entre las distribuciones.\n")
        }
      } else {
        cat("Selecciona dos variables numéricas.")
      }
    } else {
      cat("Selecciona exactamente dos variables.")
    }
  })

  # Correlación de Spearman y Pearson
  output$correlacion <- renderPrint({
    req(input$variables)
    if (length(input$variables) == 2) {
      var1 <- datos()[[input$variables[1]]]
      var2 <- datos()[[input$variables[2]]]
      if (is.numeric(var1) && is.numeric(var2)) {
        pearson <- cor.test(var1, var2, method = "pearson")
        spearman <- cor.test(var1, var2, method = "spearman")

        cat("Correlación de Pearson:\n")
        cat("Coeficiente: ", round(pearson$estimate, 3), "\n")
        cat("Valor p: ", round(pearson$p.value, 4), "\n")
        cat("Intervalo de confianza (95%): [",
            round(pearson$conf.int[1], 3), ", ",
            round(pearson$conf.int[2], 3), "]\n")

        cat("\nCorrelación de Spearman:\n")
        cat("Coeficiente: ", round(spearman$estimate, 3), "\n")
        cat("Valor p: ", round(spearman$p.value, 4), "\n")

        cat("\nInterpretación:\n")
        if (pearson$p.value < 0.05) {
          cat("✅ Pearson: Correlación significativa.\n")
        } else {
          cat("ℹ️ Pearson: Correlación no significativa.\n")
        }

        if (spearman$p.value < 0.05) {
          cat("✅ Spearman: Correlación significativa.\n")
        } else {
          cat("ℹ️ Spearman: Correlación no significativa.\n")
        }
      } else {
        cat("Selecciona dos variables numéricas.")
      }
    } else {
      cat("Selecciona exactamente dos variables.")
    }
  })

  # Descarga de estadísticas CSV
  output$descargar_estadisticas <- downloadHandler(
    filename = function() { "estadisticas_descriptivas.csv" },
    content = function(file) {
      req(input$variables)
      df <- datos()
      resultados <- lapply(input$variables, function(v) {
        col <- df[[v]]
        if (is.numeric(col)) {
          data.frame(
            Variable = v,
            Tipo = "Numérica",
            Media = mean(col, na.rm = TRUE),
            Mediana = median(col, na.rm = TRUE),
            Moda = moda_segura(col),
            Min = min(col, na.rm = TRUE),
            Max = max(col, na.rm = TRUE),
            Rango = max(col, na.rm = TRUE) - min(col, na.rm = TRUE),
            Desv_Estandar = sd(col, na.rm = TRUE),
            Coef_Variacion = round(sd(col, na.rm = TRUE) / mean(col, na.rm = TRUE), 3)
          )
        } else {
          data.frame(
            Variable = v,
            Tipo = "Categórica",
            Categorías = length(unique(col)),
            Moda = moda_segura(col)
          )
        }
      })
      write.csv(do.call(rbind, resultados), file, row.names = FALSE)
    }
  )

  # Función para generar reporte PDF
  output$descargar_pdf <- downloadHandler(
    filename = function() {
      paste("reporte_estadistico_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Crear archivo temporal Rmd
      temp_rmd <- tempfile(fileext = ".Rmd")

      # Contenido del archivo Rmd
      rmd_content <- '
---
title: "Reporte de Análisis Estadístico"
date: "`r Sys.Date()`"
output:
  pdf_document:
    latex_engine: xelatex
    fig_caption: true
    number_sections: true
geometry: margin=1in
header-includes:
  - \\usepackage{float}
  - \\usepackage{booktabs}
  - \\usepackage{longtable}
  - \\usepackage{array}
  - \\usepackage{multirow}
  - \\usepackage{wrapfig}
  - \\usepackage{colortbl}
  - \\usepackage{pdflscape}
  - \\usepackage{tabu}
  - \\usepackage{threeparttable}
  - \\usepackage{threeparttablex}
  - \\usepackage[normalem]{ulem}
  - \\usepackage{makecell}
  - \\usepackage{xcolor}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.pos = "H")
library(ggplot2)
library(dplyr)
library(knitr)
library(gridExtra)

# Datos
datos_reporte <- readRDS("datos_temp.rds")
variables_reporte <- readRDS("variables_temp.rds")
```

# Resumen Ejecutivo

Este reporte presenta un análisis estadístico completo de las variables seleccionadas del conjunto de datos cargado. Se incluyen estadísticas descriptivas, gráficos y pruebas estadísticas para cada variable.

## Información General del Dataset

- **Número total de observaciones:** `r nrow(datos_reporte)`
- **Número total de variables:** `r ncol(datos_reporte)`
- **Variables analizadas:** `r length(variables_reporte)`

# Análisis por Variable

```{r analisis_variables, results="asis", fig.height=6, fig.width=8}
for (var in variables_reporte) {
  cat("\\n## Variable:", var, "\\n\\n")

  datos_col <- datos_reporte[[var]]

  if (is.numeric(datos_col)) {
    cat("**Tipo:** Numérica\\n\\n")

    # Estadísticas descriptivas
    stats <- data.frame(
      Estadístico = c("Media", "Mediana", "Mínimo", "Máximo",
                      "Desviación Estándar", "Coeficiente de Variación"),
      Valor = c(
        round(mean(datos_col, na.rm = TRUE), 3),
        round(median(datos_col, na.rm = TRUE), 3),
        round(min(datos_col, na.rm = TRUE), 3),
        round(max(datos_col, na.rm = TRUE), 3),
        round(sd(datos_col, na.rm = TRUE), 3),
        round(sd(datos_col, na.rm = TRUE) / mean(datos_col, na.rm = TRUE), 3)
      )
    )

    print(kable(stats, caption = paste("Estadísticas Descriptivas de", var)))
    cat("\\n\\n")

    # Gráficos
    p1 <- ggplot(data.frame(x = datos_col), aes(x = x)) +
      geom_histogram(bins = 15, fill = "#3498DB", color = "white", alpha = 0.8) +
      geom_density(aes(y = ..count..), color = "#E74C3C", size = 1) +
      theme_minimal() +
      labs(title = paste("Histograma de", var), x = var, y = "Frecuencia")

    p2 <- ggplot(data.frame(x = datos_col), aes(y = x)) +
      geom_boxplot(fill = "#9B59B6", alpha = 0.8) +
      theme_minimal() +
      labs(title = paste("Boxplot de", var), y = var) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

    print(grid.arrange(p1, p2, ncol = 2))
    cat("\\n\\n")

    # Análisis de normalidad
    if (length(datos_col) >= 3 && length(datos_col) <= 5000) {
      normalidad_test <- shapiro.test(datos_col)
      cat("**Prueba de Normalidad (Shapiro-Wilk):**\\n")
      cat("- Valor p:", round(normalidad_test$p.value, 4), "\\n")
      if (normalidad_test$p.value < 0.05) {
        cat("- Interpretación: La variable NO sigue una distribución normal.\\n\\n")
      } else {
        cat("- Interpretación: La variable sigue una distribución normal.\\n\\n")
      }
    }

    # Detección de outliers
    q1 <- quantile(datos_col, 0.25, na.rm = TRUE)
    q3 <- quantile(datos_col, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    outliers <- datos_col[datos_col < (q1 - 1.5 * iqr) | datos_col > (q3 + 1.5 * iqr)]

    cat("**Detección de Valores Atípicos:**\\n")
    cat("- Q1:", round(q1, 3), "| Q3:", round(q3, 3), "| IQR:", round(iqr, 3), "\\n")
    if (length(outliers) > 0) {
      cat("- Se encontraron", length(outliers), "valores atípicos\\n\\n")
    } else {
      cat("- No se detectaron valores atípicos\\n\\n")
    }

  } else {
    cat("**Tipo:** Categórica\\n\\n")

    # Tabla de frecuencias
    freq_table <- table(datos_col)
    prop_table <- prop.table(freq_table)

    freq_df <- data.frame(
      Categoría = names(freq_table),
      Frecuencia = as.numeric(freq_table),
      Proporción = round(as.numeric(prop_table), 3),
      Porcentaje = paste0(round(as.numeric(prop_table) * 100, 1), "%")
    )

    print(kable(freq_df, caption = paste("Distribución de Frecuencias de", var)))
    cat("\\n\\n")

    # Gráfico de barras
    p <- ggplot(data.frame(x = datos_col), aes(x = x)) +
      geom_bar(fill = "#2ECC71", alpha = 0.8) +
      theme_minimal() +
      labs(title = paste("Distribución de", var), x = var, y = "Frecuencia") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    print(p)
    cat("\\n\\n")
  }

  cat("\\newpage\\n\\n")
}
```

# Análisis de Correlaciones

```{r correlaciones, results="asis", fig.height=6, fig.width=8}
vars_numericas <- variables_reporte[sapply(variables_reporte, function(v) is.numeric(datos_reporte[[v]]))]

if (length(vars_numericas) >= 2) {
  cat("## Matriz de Correlaciones\\n\\n")

  # Crear matriz de correlaciones
  cor_matrix <- cor(datos_reporte[vars_numericas], use = "complete.obs")

  print(kable(round(cor_matrix, 3), caption = "Matriz de Correlaciones (Pearson)"))
  cat("\\n\\n")

  # Gráfico de correlaciones si hay suficientes variables
  if (length(vars_numericas) >= 2) {
    library(corrplot)
    corrplot(cor_matrix, method = "color", type = "upper",
             order = "hclust", tl.cex = 0.8, tl.col = "black")
  }
} else {
  cat("Se necesitan al menos 2 variables numéricas para análisis de correlaciones.\\n\\n")
}
```

# Conclusiones

Este reporte presenta un análisis exhaustivo de las variables seleccionadas. Se recomienda:

1. **Variables numéricas:** Revisar la distribución y presencia de valores atípicos
2. **Variables categóricas:** Analizar la distribución de frecuencias
3. **Correlaciones:** Considerar las relaciones entre variables numéricas para análisis posteriores

---

*Reporte generado automáticamente el `r Sys.Date()`*
'

# Escribir el archivo Rmd
writeLines(rmd_content, temp_rmd)

# Guardar datos temporalmente
saveRDS(datos(), "datos_temp.rds")
saveRDS(input$variables, "variables_temp.rds")
output$descargar_pdf <- downloadHandler(
  filename = function() {
    paste("reporte_estadistico_", Sys.Date(), ".pdf", sep = "")
  },
  content = function(file) {
    # Guardar datos necesarios para el Rmd
    saveRDS(datos(), file = "datos_temp.rds")
    saveRDS(input$variables, file = "variables_temp.rds")

    # Crear archivo temporal Rmd
    temp_rmd <- tempfile(fileext = ".Rmd")

    # Escribir contenido del Rmd
    writeLines(rmd_content, con = temp_rmd)

    # Renderizar el PDF
    rmarkdown::render(temp_rmd, output_file = file, envir = new.env(parent = globalenv()))
  }
)


# Renderizar PDF
tryCatch({
  rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)

  # Limpiar archivos temporales
  unlink("datos_temp.rds")
  unlink("variables_temp.rds")

  output$status_pdf <- renderText({
    "✅ Reporte PDF generado exitosamente"
  })
}, error = function(e) {
  output$status_pdf <- renderText({
    paste("❌ Error al generar PDF:", e$message)
  })
})
    }
  )
}

shinyApp(ui, server)
