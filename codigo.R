# 
library(dplyr)
library(ggplot2)
library(tseries)
library(vars)
library(forecast)
library(lubridate)
library(urca)
library(tsDyn)
library(readxl)
# -----------------------------------------------------------------------------

data <- read_xlsx(file.choose())
data$date <- as.Date(data$date)
# Imputar valores NA con interpolación lineal
data$colcap <- na.approx(data$colcap, na.rm = FALSE)
data <- data %>% filter(date <= as.Date("2025-02-12"))

# Crear la serie para por trimestres
data_trimestral <- data %>%
  mutate(Year = year(date), Quarter = quarter(date)) %>%
  group_by(Year, Quarter) %>%
  summarise(
    trm = mean(trm, na.rm = TRUE),       # Promedio trimestral de TRM
    colcap = mean(colcap, na.rm = TRUE)  # Promedio trimestral de COLCAP
  ) %>%
  ungroup() %>%
  mutate(date = as.Date(paste0(Year, "-", (Quarter - 1) * 3 + 1, "-01"))) # Convertir a fecha real

# -----------------------------------------------------------------------------
#                       Estadística descriptiva de las series 
# -----------------------------------------------------------------------------
summary(data_trimestral[, c("colcap", "trm")])
sd(data_trimestral$colcap) 
sd(data_trimestral$trm)

# -----------------------------------------------------------------------------
#                       Gráficas de las series (rojo trm, colcap azul)
# -----------------------------------------------------------------------------
ggplot(data_trimestral, aes(x = date)) +
  geom_line(aes(y = colcap, color = "COLCAP", group = 1), size = 1) +
  geom_line(aes(y = trm, color = "TRM", group = 1), size = 1) +
  scale_color_manual(values = c("COLCAP" = "blue", "TRM" = "red")) +
  labs(title = "Evolución de COLCAP y TRM",
       x = "Fecha (Trimestre)",
       y = "Valor",
       color = "Indicador") +
  theme_minimal()

# -----------------------------------------------------------------------------
#           Pruebas de estacionariedad (Dickey-Fuller aumentado) con k=2 porque como se especificó en el trabajo se considero criterio de hannan-quinn
# -----------------------------------------------------------------------------
VARselect(data_trimestral$colcap, lag.max = 10, type = "const")
VARselect(data_trimestral$trm, lag.max = 10, type = "const")
adf.test(data_trimestral$colcap, k = 2)
adf.test(data_trimestral$trm, k = 2)
# -----------------------------------------------------------------------------
#           Prueba de Cointegración (Johansen porque en una relación nos dió cointegradas) y Engle - Granger
# -----------------------------------------------------------------------------
# Convertir a serie de tiempo trimestral
data_ts <- ts(data_trimestral[, c("trm", "colcap")], 
              start = c(min(data_trimestral$Year), min(data_trimestral$Quarter)), 
              frequency = 4)
# Número óptimo de rezagos VAR
var_lags <- VARselect(data_ts, lag.max = 8, type = "const")
print(var_lags)
# Prueba de cointegración de Johansen
johansen_test <- ca.jo(data_ts, type = "trace", K = 2, ecdet = "const")
summary(johansen_test)

reg <- lm(colcap ~ trm, data = data_trimestral)
residuos <- residuals(reg)
adf.test(residuos, k = 2)


# Transformar las series a diferencias para estimar el modelo VAR
data_diff <- diff(data_ts)
# Modelo VAR con k = 2
var_model <- VAR(data_diff, p = 2, type = "const")
summary(var_model)

# Impulso Respuesta
# shock en trm, respuesta en colcap
irf_result <- irf(var_model, impulse = "trm", response = "colcap", n.ahead = 10, boot = TRUE)
plot(irf_result)
# shock en colcap, respuesta en trm
irf_result2 <- irf(var_model, impulse = "colcap", response = "trm", n.ahead = 10, boot = TRUE)
plot(irf_result2)

# Descomposición de Varianza
fevd_result <- fevd(var_model, n.ahead = 10)
plot(fevd_result, names = "trm")
plot(fevd_result, names = "colcap")

print(fevd_result)

# -----------------------------------------------------------------------------
#                                   Diagnóstico
# -----------------------------------------------------------------------------
serial_test_var <- serial.test(var_model)  # Autocorrelación
print(serial_test_var)

normality_test_var <- normality.test(var_model)  # Normalidad
print(normality_test_var)

stability_test_var <- stability(var_model)  # Estabilidad
plot(stability_test_var)

stability(var_model)
# Causalidad (OPCIONAL, YO CREERÍA QUE NO NECESARIO)
grangertest(data_trimestral$colcap, data$trm, order = 2)
grangertest(data$trm, data_trimestral$colcap, order = 2)



