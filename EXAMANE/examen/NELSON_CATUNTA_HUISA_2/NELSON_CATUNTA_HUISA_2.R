# Pregunta 2: Análisis de coeficiente de variación en distribución normal
# Simular muestra normal y evaluar dispersión relativa

# Establecer semilla para reproducibilidad
set.seed(123)

# Parámetros de la distribución normal
media_teorica <- 20
desv_teorica <- 4
n <- 500

# Simular muestra de distribución normal
muestra_normal <- rnorm(n, mean = media_teorica, sd = desv_teorica)

# Calcular estadísticas muestrales
media_muestral <- mean(muestra_normal)
desv_muestral <- sd(muestra_normal)

# Calcular coeficiente de variación
cv <- (desv_muestral / media_muestral) * 100

# Mostrar resultados
cat("=== PARÁMETROS DE LA DISTRIBUCIÓN ===\n")
cat("Media teórica (μ):", media_teorica, "\n")
cat("Desviación estándar teórica (σ):", desv_teorica, "\n")
cat("Tamaño de muestra (n):", n, "\n")

cat("\n=== ESTADÍSTICAS MUESTRALES ===\n")
cat("Media muestral:", round(media_muestral, 3), "\n")
cat("Desviación estándar muestral:", round(desv_muestral, 3), "\n")
cat("Varianza muestral:", round(var(muestra_normal), 3), "\n")

cat("\n=== COEFICIENTE DE VARIACIÓN ===\n")
cat("CV = (s/x̄) × 100% =", round(cv, 2), "%\n")

# Crear visualización completa
par(mfrow = c(2, 2))

# 1. Histograma con curva normal teórica
hist(muestra_normal,
     breaks = 30,
     freq = FALSE,
     main = "Histograma de la Muestra\ncon Distribución Teórica",
     xlab = "Valores",
     ylab = "Densidad",
     col = "lightblue",
     border = "darkblue")

# Superponer curva normal teórica
x_teorico <- seq(min(muestra_normal), max(muestra_normal), length = 100)
y_teorico <- dnorm(x_teorico, mean = media_teorica, sd = desv_teorica)
lines(x_teorico, y_teorico, col = "red", lwd = 2)

# Agregar líneas de media
abline(v = media_muestral, col = "blue", lwd = 2, lty = 2)
abline(v = media_teorica, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = c("Teórica", "Media teórica", "Media muestral"),
       col = c("red", "red", "blue"),
       lty = c(1, 2, 2),
       lwd = 2,
       cex = 0.8)

# 2. Boxplot
boxplot(muestra_normal,
        main = "Diagrama de Caja",
        ylab = "Valores",
        col = "lightgreen",
        border = "darkgreen")

# Agregar línea de media
abline(h = media_muestral, col = "red", lwd = 2, lty = 2)

# 3. Q-Q plot para verificar normalidad
qqnorm(muestra_normal,
       main = "Q-Q Plot\n(Verificación de Normalidad)",
       col = "purple",
       pch = 20)
qqline(muestra_normal, col = "red", lwd = 2)

# 4. Gráfico de dispersión relativa
plot(1:n, muestra_normal,
     main = "Valores de la Muestra\ncon Bandas de Dispersión",
     xlab = "Observación",
     ylab = "Valor",
     pch = 20,
     col = "steelblue",
     cex = 0.6)

# Agregar líneas de referencia
abline(h = media_muestral, col = "red", lwd = 2)
abline(h = media_muestral + desv_muestral, col = "orange", lwd = 2, lty = 2)
abline(h = media_muestral - desv_muestral, col = "orange", lwd = 2, lty = 2)
abline(h = media_muestral + 2*desv_muestral, col = "red", lwd = 1, lty = 3)
abline(h = media_muestral - 2*desv_muestral, col = "red", lwd = 1, lty = 3)

legend("topright",
       legend = c("Media", "±1 SD", "±2 SD"),
       col = c("red", "orange", "red"),
       lty = c(1, 2, 3),
       lwd = c(2, 2, 1),
       cex = 0.8)

# Restaurar parámetros gráficos
par(mfrow = c(1, 1))

# Análisis detallado del coeficiente de variación
cat("\n=== ANÁLISIS DEL COEFICIENTE DE VARIACIÓN ===\n")

# Calcular CV teórico
cv_teorico <- (desv_teorica / media_teorica) * 100
cat("CV teórico:", round(cv_teorico, 2), "%\n")
cat("CV muestral:", round(cv, 2), "%\n")
cat("Diferencia:", round(abs(cv - cv_teorico), 2), "%\n")

# Interpretación del CV
cat("\n=== INTERPRETACIÓN DE LA DISPERSIÓN RELATIVA ===\n")
cat("Criterios generales para interpretar el CV:\n")
cat("• CV < 10%: Dispersión relativa baja\n")
cat("• 10% ≤ CV < 20%: Dispersión relativa moderada\n")
cat("• 20% ≤ CV < 30%: Dispersión relativa alta\n")
cat("• CV ≥ 30%: Dispersión relativa muy alta\n")

cat("\nPara esta muestra:\n")
if (cv < 10) {
  cat("✓ DISPERSIÓN RELATIVA BAJA (CV =", round(cv, 2), "%)\n")
  cat("  Los datos están poco dispersos en relación a la media.\n")
  cat("  La distribución es relativamente homogénea.\n")
} else if (cv < 20) {
  cat("✓ DISPERSIÓN RELATIVA MODERADA (CV =", round(cv, 2), "%)\n")
  cat("  Los datos presentan una dispersión moderada en relación a la media.\n")
  cat("  La variabilidad es aceptable para la mayoría de aplicaciones.\n")
} else if (cv < 30) {
  cat("⚠ DISPERSIÓN RELATIVA ALTA (CV =", round(cv, 2), "%)\n")
  cat("  Los datos están considerablemente dispersos en relación a la media.\n")
  cat("  La variabilidad es alta.\n")
} else {
  cat("⚠ DISPERSIÓN RELATIVA MUY ALTA (CV =", round(cv, 2), "%)\n")
  cat("  Los datos están muy dispersos en relación a la media.\n")
  cat("  La variabilidad es muy alta.\n")
}

# Estadísticas adicionales
cat("\n=== ESTADÍSTICAS ADICIONALES ===\n")
cat("Mínimo:", round(min(muestra_normal), 3), "\n")
cat("Máximo:", round(max(muestra_normal), 3), "\n")
cat("Rango:", round(max(muestra_normal) - min(muestra_normal), 3), "\n")
cat("Mediana:", round(median(muestra_normal), 3), "\n")
cat("Cuartil 1:", round(quantile(muestra_normal, 0.25), 3), "\n")
cat("Cuartil 3:", round(quantile(muestra_normal, 0.75), 3), "\n")
cat("Rango intercuartílico:", round(IQR(muestra_normal), 3), "\n")

# Comparación con otras distribuciones (ejemplos)
cat("\n=== COMPARACIÓN CON OTROS CONTEXTOS ===\n")
cat("Ejemplos de CV en diferentes contextos:\n")
cat("• Alturas humanas: CV ≈ 3-5% (baja dispersión)\n")
cat("• Pesos humanos: CV ≈ 15-20% (moderada dispersión)\n")
cat("• Ingresos: CV ≈ 50-100% (alta dispersión)\n")
cat("• Precios de acciones: CV ≈ 20-50% (alta dispersión)\n")

# Intervalos de confianza para la media
error_estandar <- desv_muestral / sqrt(n)
ic_inferior <- media_muestral - 1.96 * error_estandar
ic_superior <- media_muestral + 1.96 * error_estandar

cat("\n=== INTERVALO DE CONFIANZA PARA LA MEDIA (95%) ===\n")
cat("Error estándar:", round(error_estandar, 4), "\n")
cat("IC 95%: [", round(ic_inferior, 3), ", ", round(ic_superior, 3), "]\n", sep = "")

# Verificar si los parámetros teóricos están dentro del IC
if (media_teorica >= ic_inferior && media_teorica <= ic_superior) {
  cat("✓ La media teórica (", media_teorica, ") está dentro del IC 95%\n")
} else {
  cat("⚠ La media teórica (", media_teorica, ") está fuera del IC 95%\n")
}
