# Establecer semilla para reproducibilidad
set.seed(123)

# Generar muestra de 10,000 números con distribución normal estándar
muestra <- rnorm(10000, mean = 0, sd = 1)

# Calcular media y desviación estándar muestral
media_muestral <- mean(muestra)
desviacion_muestral <- sd(muestra)

# Mostrar resultados
cat("Media muestral:", media_muestral, "\n")
cat("Desviación estándar muestral:", desviacion_muestral, "\n")

# Crear histograma de la muestra con densidad
hist(muestra, breaks = 50, probability = TRUE,
     main = "Distribución empírica vs teórica (N(0,1))",
     xlab = "Valor", col = "lightblue", border = "gray")

# Agregar curva de densidad teórica (normal estándar)
curve(dnorm(x, mean = 0, sd = 1),
      col = "red", lwd = 2, add = TRUE)

# Agregar curva de densidad empírica
lines(density(muestra), col = "darkgreen", lwd = 2, lty = 2)

# Leyenda
legend("topright",
       legend = c("Densidad teórica N(0,1)", "Densidad empírica"),
       col = c("red", "darkgreen"),
       lwd = 2, lty = c(1, 2))
