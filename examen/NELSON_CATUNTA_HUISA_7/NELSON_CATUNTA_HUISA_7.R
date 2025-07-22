# Pregunta 7: Análisis de patrones en distribución uniforme
# Simular 500 valores de una distribución uniforme y evaluar patrones

# Establecer semilla para reproducibilidad
set.seed(123)

# Generar 500 valores de una distribución uniforme
n <- 500
valores_uniformes <- runif(n, min = 0, max = 1)

# Crear vectores para el gráfico de dispersión
# x[i] vs x[i+1] requiere n-1 pares de puntos
x_i <- valores_uniformes[1:(n-1)]      # x[i]: valores del 1 al 499
x_i_plus_1 <- valores_uniformes[2:n]   # x[i+1]: valores del 2 al 500

# Crear el gráfico de dispersión
plot(x_i, x_i_plus_1,
     main = "Gráfico de Dispersión: x[i] vs x[i+1]\nDistribución Uniforme (n=500)",
     xlab = "x[i] (Valor actual)",
     ylab = "x[i+1] (Valor siguiente)",
     pch = 20,                          # Puntos sólidos
     col = "steelblue",                 # Color azul
     cex = 0.8,                         # Tamaño de puntos
     xlim = c(0, 1),                    # Límites del eje x
     ylim = c(0, 1))                    # Límites del eje y

# Agregar líneas de referencia
abline(h = 0.5, v = 0.5, col = "gray", lty = 2, lwd = 1)

# Agregar grid para mejor visualización
grid(col = "lightgray", lty = 3)

# Estadísticas descriptivas
cat("=== ESTADÍSTICAS DESCRIPTIVAS ===\n")
cat("Número de valores simulados:", n, "\n")
cat("Número de pares (x[i], x[i+1]):", length(x_i), "\n")
cat("Media de x[i]:", round(mean(x_i), 4), "\n")
cat("Media de x[i+1]:", round(mean(x_i_plus_1), 4), "\n")
cat("Desviación estándar de x[i]:", round(sd(x_i), 4), "\n")
cat("Desviación estándar de x[i+1]:", round(sd(x_i_plus_1), 4), "\n")

# Calcular correlación entre valores consecutivos
correlacion <- cor(x_i, x_i_plus_1)
cat("Correlación entre x[i] y x[i+1]:", round(correlacion, 4), "\n")

# Prueba de correlación
cor_test <- cor.test(x_i, x_i_plus_1)
cat("p-valor de la prueba de correlación:", round(cor_test$p.value, 4), "\n")

# Crear un segundo gráfico con colores por cuadrantes para mejor análisis
par(mfrow = c(1, 2))

# Gráfico original
plot(x_i, x_i_plus_1,
     main = "Dispersión Original",
     xlab = "x[i]",
     ylab = "x[i+1]",
     pch = 20,
     col = "steelblue",
     cex = 0.8)
abline(h = 0.5, v = 0.5, col = "red", lty = 2)
grid(col = "lightgray", lty = 3)

# Gráfico con colores por cuadrantes
colores <- ifelse(x_i < 0.5 & x_i_plus_1 < 0.5, "red",
                  ifelse(x_i < 0.5 & x_i_plus_1 >= 0.5, "blue",
                         ifelse(x_i >= 0.5 & x_i_plus_1 < 0.5, "green", "purple")))

plot(x_i, x_i_plus_1,
     main = "Por Cuadrantes",
     xlab = "x[i]",
     ylab = "x[i+1]",
     pch = 20,
     col = colores,
     cex = 0.8)
abline(h = 0.5, v = 0.5, col = "black", lty = 2, lwd = 2)
grid(col = "lightgray", lty = 3)

# Leyenda para cuadrantes
legend("topright",
       legend = c("Q1: (<0.5, <0.5)", "Q2: (<0.5, ≥0.5)",
                  "Q3: (≥0.5, <0.5)", "Q4: (≥0.5, ≥0.5)"),
       col = c("red", "blue", "green", "purple"),
       pch = 20,
       cex = 0.8)

# Restaurar parámetros gráficos
par(mfrow = c(1, 1))

# Análisis de cuadrantes
cat("\n=== ANÁLISIS POR CUADRANTES ===\n")
cuadrante1 <- sum(x_i < 0.5 & x_i_plus_1 < 0.5)
cuadrante2 <- sum(x_i < 0.5 & x_i_plus_1 >= 0.5)
cuadrante3 <- sum(x_i >= 0.5 & x_i_plus_1 < 0.5)
cuadrante4 <- sum(x_i >= 0.5 & x_i_plus_1 >= 0.5)

cat("Cuadrante 1 (x<0.5, x+1<0.5):", cuadrante1,
    "(", round(100*cuadrante1/length(x_i), 1), "%)\n")
cat("Cuadrante 2 (x<0.5, x+1≥0.5):", cuadrante2,
    "(", round(100*cuadrante2/length(x_i), 1), "%)\n")
cat("Cuadrante 3 (x≥0.5, x+1<0.5):", cuadrante3,
    "(", round(100*cuadrante3/length(x_i), 1), "%)\n")
cat("Cuadrante 4 (x≥0.5, x+1≥0.5):", cuadrante4,
    "(", round(100*cuadrante4/length(x_i), 1), "%)\n")

# Interpretación de resultados
cat("\n=== INTERPRETACIÓN ===\n")
cat("En una distribución uniforme verdaderamente aleatoria:\n")
cat("- La correlación entre valores consecutivos debería ser ≈ 0\n")
cat("- Los puntos deberían distribuirse uniformemente en el cuadrado [0,1]×[0,1]\n")
cat("- Cada cuadrante debería contener ≈ 25% de los puntos\n")
cat("- No deberían observarse patrones sistemáticos\n")

if (abs(correlacion) < 0.1) {
  cat("\n✓ RESULTADO: No se observa correlación significativa entre valores consecutivos\n")
  cat("  La distribución parece ser genuinamente aleatoria\n")
} else {
  cat("\n⚠ RESULTADO: Se observa correlación entre valores consecutivos\n")
  cat("  Esto podría indicar un patrón en el generador de números aleatorios\n")
}
