# Pregunta 9: Simulación de agentes móviles en cuadrícula 10x10
# 100 agentes, 10 pasos de tiempo, movimiento aleatorio

# Establecer semilla para reproducibilidad
set.seed(123)

# Parámetros de la simulación
n_agentes <- 100
tamano_cuadricula <- 10
n_pasos <- 10

# Función para asegurar que las coordenadas estén dentro de la cuadrícula
limitar_coordenadas <- function(x, limite_min = 1, limite_max = tamano_cuadricula) {
  pmax(limite_min, pmin(limite_max, x))
}

# Inicializar posiciones aleatorias de los agentes
posiciones_iniciales <- data.frame(
  agente = 1:n_agentes,
  x = sample(1:tamano_cuadricula, n_agentes, replace = TRUE),
  y = sample(1:tamano_cuadricula, n_agentes, replace = TRUE)
)

# Copiar posiciones iniciales para la simulación
posiciones_actuales <- posiciones_iniciales

# Definir movimientos posibles: arriba, abajo, izquierda, derecha
movimientos <- data.frame(
  dx = c(0, 0, -1, 1),  # cambio en x
  dy = c(1, -1, 0, 0),  # cambio en y
  direccion = c("Arriba", "Abajo", "Izquierda", "Derecha")
)

# Almacenar historial de posiciones para análisis
historial_posiciones <- array(NA, dim = c(n_agentes, n_pasos + 1, 2))
historial_posiciones[, 1, 1] <- posiciones_iniciales$x
historial_posiciones[, 1, 2] <- posiciones_iniciales$y

# Simulación de movimientos
cat("=== SIMULACIÓN DE MOVIMIENTOS ===\n")
cat("Simulando", n_agentes, "agentes en cuadrícula", tamano_cuadricula, "x", tamano_cuadricula, "\n")
cat("Número de pasos:", n_pasos, "\n\n")

for (paso in 1:n_pasos) {
  cat("Paso", paso, "de", n_pasos, "\n")

  # Para cada agente, elegir un movimiento aleatorio
  for (i in 1:n_agentes) {
    # Seleccionar movimiento aleatorio
    movimiento_elegido <- sample(1:4, 1)

    # Aplicar movimiento
    nueva_x <- posiciones_actuales$x[i] + movimientos$dx[movimiento_elegido]
    nueva_y <- posiciones_actuales$y[i] + movimientos$dy[movimiento_elegido]

    # Limitar coordenadas dentro de la cuadrícula
    posiciones_actuales$x[i] <- limitar_coordenadas(nueva_x)
    posiciones_actuales$y[i] <- limitar_coordenadas(nueva_y)
  }

  # Guardar posiciones en el historial
  historial_posiciones[, paso + 1, 1] <- posiciones_actuales$x
  historial_posiciones[, paso + 1, 2] <- posiciones_actuales$y
}

# Preparar datos para visualización
posiciones_finales <- data.frame(
  agente = 1:n_agentes,
  x = posiciones_actuales$x,
  y = posiciones_actuales$y
)

# Crear visualización comparativa
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# 1. Posiciones iniciales
plot(posiciones_iniciales$x, posiciones_iniciales$y,
     xlim = c(0.5, tamano_cuadricula + 0.5),
     ylim = c(0.5, tamano_cuadricula + 0.5),
     pch = 20,
     col = "blue",
     cex = 1.2,
     main = "Posiciones Iniciales",
     xlab = "Coordenada X",
     ylab = "Coordenada Y")

# Agregar cuadrícula
for (i in 1:tamano_cuadricula) {
  abline(v = i - 0.5, col = "lightgray", lty = 2)
  abline(h = i - 0.5, col = "lightgray", lty = 2)
}

# Agregar números de agentes (muestra algunos)
text(posiciones_iniciales$x[1:20], posiciones_iniciales$y[1:20],
     labels = 1:20, pos = 3, cex = 0.6, col = "darkblue")

# 2. Posiciones finales
plot(posiciones_finales$x, posiciones_finales$y,
     xlim = c(0.5, tamano_cuadricula + 0.5),
     ylim = c(0.5, tamano_cuadricula + 0.5),
     pch = 20,
     col = "red",
     cex = 1.2,
     main = "Posiciones Finales",
     xlab = "Coordenada X",
     ylab = "Coordenada Y")

# Agregar cuadrícula
for (i in 1:tamano_cuadricula) {
  abline(v = i - 0.5, col = "lightgray", lty = 2)
  abline(h = i - 0.5, col = "lightgray", lty = 2)
}

# Agregar números de agentes (muestra algunos)
text(posiciones_finales$x[1:20], posiciones_finales$y[1:20],
     labels = 1:20, pos = 3, cex = 0.6, col = "darkred")

# 3. Comparación superpuesta
plot(posiciones_iniciales$x, posiciones_iniciales$y,
     xlim = c(0.5, tamano_cuadricula + 0.5),
     ylim = c(0.5, tamano_cuadricula + 0.5),
     pch = 20,
     col = "blue",
     cex = 1,
     main = "Comparación: Inicial vs Final",
     xlab = "Coordenada X",
     ylab = "Coordenada Y")

# Superponer posiciones finales
points(posiciones_finales$x, posiciones_finales$y,
       pch = 20, col = "red", cex = 1)

# Agregar líneas conectando posiciones iniciales y finales
for (i in 1:n_agentes) {
  lines(c(posiciones_iniciales$x[i], posiciones_finales$x[i]),
        c(posiciones_iniciales$y[i], posiciones_finales$y[i]),
        col = "gray", lty = 1, lwd = 0.5)
}

# Agregar cuadrícula
for (i in 1:tamano_cuadricula) {
  abline(v = i - 0.5, col = "lightgray", lty = 2)
  abline(h = i - 0.5, col = "lightgray", lty = 2)
}

legend("topright",
       legend = c("Inicial", "Final"),
       col = c("blue", "red"),
       pch = 20,
       cex = 0.8)

# 4. Densidad de ocupación final
densidad_final <- table(posiciones_finales$x, posiciones_finales$y)
image(1:tamano_cuadricula, 1:tamano_cuadricula, as.matrix(densidad_final),
      col = heat.colors(10),
      main = "Densidad de Ocupación Final",
      xlab = "Coordenada X",
      ylab = "Coordenada Y")

# Agregar números en cada celda
for (i in 1:tamano_cuadricula) {
  for (j in 1:tamano_cuadricula) {
    text(i, j, densidad_final[i, j], cex = 0.8, col = "black")
  }
}

# Restaurar parámetros gráficos
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

# Análisis estadístico del movimiento
cat("\n=== ANÁLISIS ESTADÍSTICO ===\n")

# Calcular distancias recorridas
distancias <- sqrt((posiciones_finales$x - posiciones_iniciales$x)^2 +
                     (posiciones_finales$y - posiciones_iniciales$y)^2)

cat("Distancia promedio recorrida:", round(mean(distancias), 3), "\n")
cat("Distancia mínima:", round(min(distancias), 3), "\n")
cat("Distancia máxima:", round(max(distancias), 3), "\n")
cat("Desviación estándar de distancias:", round(sd(distancias), 3), "\n")

# Calcular desplazamientos netos
desplazamiento_x <- posiciones_finales$x - posiciones_iniciales$x
desplazamiento_y <- posiciones_finales$y - posiciones_iniciales$y

cat("\nDesplazamiento neto promedio en X:", round(mean(desplazamiento_x), 3), "\n")
cat("Desplazamiento neto promedio en Y:", round(mean(desplazamiento_y), 3), "\n")

# Análisis de distribución final
cat("\n=== DISTRIBUCIÓN FINAL ===\n")
cat("Posición X - Media:", round(mean(posiciones_finales$x), 3),
    "SD:", round(sd(posiciones_finales$x), 3), "\n")
cat("Posición Y - Media:", round(mean(posiciones_finales$y), 3),
    "SD:", round(sd(posiciones_finales$y), 3), "\n")

# Ocupación por celda
ocupacion_por_celda <- as.vector(densidad_final)
cat("Ocupación por celda - Media:", round(mean(ocupacion_por_celda), 2),
    "SD:", round(sd(ocupacion_por_celda), 2), "\n")
cat("Celda más ocupada:", max(ocupacion_por_celda), "agentes\n")
cat("Celda menos ocupada:", min(ocupacion_por_celda), "agentes\n")

# Mostrar algunas trayectorias individuales
cat("\n=== TRAYECTORIAS DE ALGUNOS AGENTES ===\n")
for (i in 1:5) {
  cat("Agente", i, ":\n")
  cat("  Inicial: (", posiciones_iniciales$x[i], ",", posiciones_iniciales$y[i], ")\n")
  cat("  Final: (", posiciones_finales$x[i], ",", posiciones_finales$y[i], ")\n")
  cat("  Distancia:", round(distancias[i], 3), "\n")
}

# Crear gráfico de evolución temporal de la dispersión
dispersiones <- numeric(n_pasos + 1)
for (paso in 1:(n_pasos + 1)) {
  x_paso <- historial_posiciones[, paso, 1]
  y_paso <- historial_posiciones[, paso, 2]
  # Calcular dispersión como desviación estándar de las distancias al centro
  centro_x <- mean(x_paso)
  centro_y <- mean(y_paso)
  distancias_centro <- sqrt((x_paso - centro_x)^2 + (y_paso - centro_y)^2)
  dispersiones[paso] <- sd(distancias_centro)
}

plot(0:n_pasos, dispersiones,
     type = "b",
     pch = 20,
     col = "darkgreen",
     lwd = 2,
     main = "Evolución de la Dispersión de Agentes",
     xlab = "Paso de Tiempo",
     ylab = "Dispersión (SD de distancias al centro)")

grid(col = "lightgray", lty = 2)

# Resumen final
cat("\n=== RESUMEN DE LA SIMULACIÓN ===\n")
cat("• Agentes simulados:", n_agentes, "\n")
cat("• Pasos de tiempo:", n_pasos, "\n")
cat("• Tamaño de cuadrícula:", tamano_cuadricula, "x", tamano_cuadricula, "\n")
cat("• Distancia promedio recorrida:", round(mean(distancias), 2), "unidades\n")
cat("• Dispersión final:", round(dispersiones[n_pasos + 1], 2), "\n")
cat("• Cambio en dispersión:", round(dispersiones[n_pasos + 1] - dispersiones[1], 2), "\n")

# Guardar datos finales
cat("\n=== DATOS GUARDADOS ===\n")
cat("• posiciones_iniciales: Coordenadas iniciales de todos los agentes\n")
cat("• posiciones_finales: Coordenadas finales de todos los agentes\n")
cat("• historial_posiciones: Trayectoria completa de todos los agentes\n")
cat("• distancias: Distancia recorrida por cada agente\n")
