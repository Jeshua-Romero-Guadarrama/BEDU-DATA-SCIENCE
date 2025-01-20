####################################################################################################
# Jeshua Romero Guadarrama
# Proyecto final programación y estadística con R: 
# Análisis de renta/venta de bicicletas
#
# Tablas involucradas:
#  1) customers.csv
#  2) orders.csv
#  3) order_items.csv
#
# Objetivos principales:
#  1) Conocer cuántos clientes existen de forma única.
#  2) Conocer cuántos clientes nunca han realizado una orden.
#  3) Conocer la distribución de clientes por ciudad.
#  4) Analizar las órdenes por fecha y su volumen.
#  5) Calcular el descuento promedio y las ventas totales por fecha.
#  6) Realizar análisis de series de tiempo (descomposición temporal, ARIMA).
#  7) Construir un modelo de regresión lineal múltiple para predecir ventas.
#  8) Realizar análisis estadístico adicional (dos pruebas de inferencia).
#
####################################################################################################

#--------------------------------------------------------------------------------------------------#
# 1. Carga de librerías
#--------------------------------------------------------------------------------------------------#

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("forecast")
# install.packages("ggcorrplot")

library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(ggcorrplot)

#--------------------------------------------------------------------------------------------------#
# 2. Lectura de datos CSV
#--------------------------------------------------------------------------------------------------#

customers   <- read.csv('customers.csv',   stringsAsFactors = FALSE)
orders      <- read.csv('orders.csv',      stringsAsFactors = FALSE)
order_items <- read.csv('order_items.csv', stringsAsFactors = FALSE)

#--------------------------------------------------------------------------------------------------#
# 3. Exploración y limpieza / Transformaciones básicas
#--------------------------------------------------------------------------------------------------#

# 3.1 Estructura de las tablas
cat("\n=== Estructura de las tablas ===\n")
cat("\n>>> Estructura de customers:\n")
str(customers)
cat("\n>>> Estructura de orders:\n")
str(orders)
cat("\n>>> Estructura de order_items:\n")
str(order_items)

# 3.2 Vista previa de datos
cat("\n=== Vista previa de cada tabla usando 'head' ===\n")
cat("\n>>> customers:\n")
head(customers)
cat("\n>>> orders:\n")
head(orders)
cat("\n>>> order_items:\n")
head(order_items)

# 3.3 Manejo de valores duplicados en customers
# La clave principal y única de clientes es 'customer_id' para verificar si existen IDs repetidos.
num_duplicados_cust_id <- sum(duplicated(customers$customer_id))
cat("\nNúmero de 'customer_id' duplicados en 'customers':", num_duplicados_cust_id, "\n")
# Si existen duplicados, se remueve quedándo con la primera aparición.
if(num_duplicados_cust_id > 0){
  customers <- customers %>% distinct(customer_id, .keep_all = TRUE)
  cat("Se han eliminado los registros duplicados con fundamento en 'customer_id'.\n")
}

# 3.4 Conversión de columnas de fecha en la tabla 'orders'
orders$order_date    <- as.Date(orders$order_date,    format = "%Y-%m-%d")
orders$required_date <- as.Date(orders$required_date, format = "%Y-%m-%d")
orders$shipped_date  <- as.Date(orders$shipped_date,  format = "%Y-%m-%d")

# 3.5 Revisión de valores nulos en cada tabla
cat("\n=== Revisión de valores nulos (NA's) ===\n")
cat("NA's en customers:",   sum(is.na(customers)),   "\n")
cat("NA's en orders:",      sum(is.na(orders)),      "\n")
cat("NA's en order_items:", sum(is.na(order_items)), "\n")

#--------------------------------------------------------------------------------------------------#
# 4. Análisis de Clientes
#--------------------------------------------------------------------------------------------------#
cat("\n=== Análisis de clientes ===\n")

# 4.1. ¿Cuántos clientes únicos hay en total?
total_clientes <- nrow(customers)
cat("El número total de clientes (únicos) es:", total_clientes, "\n")

# 4.2. ¿Cuántos clientes NO han realizado ninguna orden?
# Se usa 'anti_join' para encontrar customers que no estén en orders.
clientes_sin_orden <- anti_join(customers, orders, by = "customer_id")
cat("El número de clientes SIN órdenes es:", nrow(clientes_sin_orden), "\n")

# 4.3. Distribución de clientes por ciudad
clientes_por_ciudad <- customers %>%
  group_by(city) %>%
  summarise(num_clientes = n()) %>%
  arrange(desc(num_clientes))
cat("\n>>> Top cinco ciudades con más clientes:\n")
head(clientes_por_ciudad, 10)

# 4.4. Gráfico de barras sobre top diez ciudades con más clientes
top_diez_ciudades <- head(clientes_por_ciudad, 10)
primer_grafico <- ggplot(top_diez_ciudades, aes(x = reorder(city, -num_clientes), y = num_clientes)) + 
  geom_bar(stat = "identity", fill = "#2E86C1", color = "black", width = 0.7) + 
  labs(
      title = "Top diez ciudades con mayor número de clientes", 
      x     = "Ciudad", 
      y     = "Número de clientes"
    ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.x  = element_text(angle = 45,    hjust = 1),
    axis.text    = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 13),
    axis.title.y = element_text(face = "bold", size = 13)
  )
print(primer_grafico)

# 4.5. Gráfico de barras sobre comparación de las diez ciudades con más clientes vs menos clientes
# Se toman las diez ciudades con más clientes.
top_diez    <- head(clientes_por_ciudad, 10)
# Se toman las diez ciudades con menos clientes
bottom_diez <- tail(clientes_por_ciudad, 10)
# Se unen ambos subconjuntos en un solo data frame y se indica el grupo
df_top_bottom <- dplyr::bind_rows(
  top_diez %>% mutate(grupo = "Diez ciudades con más clientes"),
  bottom_diez %>% mutate(grupo = "Diez ciudades con menos clientes")
)
# Gráfico de barras con comparativo de las diez ciudades con más clientes vs menos clientes
segundo_grafico <- ggplot(df_top_bottom, aes(x = reorder(city, num_clientes), y = num_clientes, fill = grupo)) +
  geom_bar(stat = "identity", color = "white", width = 0.7) +
  coord_flip() +
  labs(
      title = "Comparación de las diez ciudades con más clientes vs menos clientes", 
      x     = "Ciudad", 
      y     = "Número de clientes"
    ) +
  scale_fill_manual(values = c("Diez ciudades con más clientes" = "forestgreen", "Diez ciudades con menos clientes" = "#E74C3C")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.y     = element_text(face = "bold"),
    axis.text.x     = element_text(face = "bold"),
    legend.title    = element_blank(),
    legend.position = "top"
  )
print(segundo_grafico)

#--------------------------------------------------------------------------------------------------#
# 5. Análisis de Órdenes
#--------------------------------------------------------------------------------------------------#
cat("\n=== Análisis de órdenes ===\n")

# 5.1 Órdenes por fecha (agrupación por mes)
ordenes_por_mes <- orders %>%
  mutate(year_month = floor_date(order_date, unit = "month")) %>%
  group_by(year_month) %>%
  summarise(num_ordenes = n()) %>%
  arrange(year_month)
cat(">>> Primeras filas (órdenes por mes):\n")
head(ordenes_por_mes, 5)
# Gráfico de líneas sobre número de órdenes por mes
tercer_grafico <- ggplot(ordenes_por_mes, aes(x = year_month, y = num_ordenes)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
        title = "Número de órdenes por mes",
        x     = "Mes",
        y     = "Número de órdenes"
      ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title  = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45,    hjust = 1,   face = "bold")
  )
print(tercer_grafico)

# 5.2 Descuento promedio (en la tabla order_items)
descuento_promedio <- mean(order_items$discount, na.rm = TRUE)
cat("\nEl porcentaje de descuento promedio es:", 
    round(descuento_promedio * 100, 2), "%\n")

# 5.3 Ventas totales por fecha
df_ventas <- inner_join(order_items, orders, by = "order_id")
# Se calcula "importe" = list_price * quantity * (1 - discount)
df_ventas <- df_ventas %>%
  mutate(importe = list_price * quantity * (1 - discount))
# Se suman las ventas por mes
ventas_por_mes <- df_ventas %>%
  mutate(year_month = floor_date(order_date, "month")) %>%
  group_by(year_month) %>%
  summarise(ventas_totales = sum(importe)) %>%
  arrange(year_month)
cat("\n>>> Primeras filas (Ventas por mes):\n")
head(ventas_por_mes, 5)
# Gráfico de líneas sobre ventas totales por mes
cuarto_grafico <- ggplot(ventas_por_mes, aes(x = year_month, y = ventas_totales)) +
  geom_line(color = "forestgreen", linewidth = 1) +
  geom_point(color = "orange", size = 2) +
  labs(
        title = "Ventas totales por mes",
        x     = "Mes",
        y     = "Ventas totales"
      ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title  = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45,    hjust = 1,   face = "bold")
  )
print(cuarto_grafico)

#--------------------------------------------------------------------------------------------------#
# 6. Análisis Avanzado
#--------------------------------------------------------------------------------------------------#
cat("\n=== Análisis de series de tiempo mediante descomposición ===\n")

# 6.1 Descomposición temporal de ventas
# Se ordenan las ventas por fecha
ventas_por_mes <- ventas_por_mes %>% arrange(year_month)
fecha_inicial  <- min(ventas_por_mes$year_month)
anio_inicial   <- year(fecha_inicial)
mes_inicial    <- month(fecha_inicial)
# Se crea la serie temporal (mensual, freq = 12)
ts_ventas_mensual <- ts(
    ventas_por_mes$ventas_totales, 
    start = c(anio_inicial, mes_inicial), 
    frequency = 12
  )
# Descomposición multiplicativa 
descomp_ventas <- decompose(ts_ventas_mensual, type = "multiplicative")

# Gráfico mostrando cada componente en subplots
par(
    mfrow = c(2, 2),    
    oma   = c(0, 0, 2, 0),
    mar   = c(4, 4, 2, 1)
  )
# a) Serie Original
plot(
    descomp_ventas$x,
    type = "l", 
    col  = "steelblue",
    lwd  = 2,
    main = "Serie original",
    xlab = "",
    ylab = "Ventas"
  )
# b) Tendencia
plot(
    descomp_ventas$trend, 
    type = "l",
    col  = "firebrick",
    lwd  = 2,
    main = "Tendencia",
    xlab = "",
    ylab = "Valor"
  )
# c) Estacionalidad
plot(
    descomp_ventas$seasonal,
    type = "l",
    col  = "forestgreen",
    lwd  = 2,
    main = "Estacionalidad",
    xlab = "",
    ylab = "Factor"
  )
# d) Residuo
plot(
    descomp_ventas$random,
    type = "l",
    col  = "gray40",
    lwd  = 2,
    main = "Residuo",
    xlab = "",
    ylab = "Valor"
  )
mtext(
    "Descomposición multiplicativa de ventas mensuales", 
    outer = TRUE,  
    cex = 1.5, 
    font = 2
  )

# 6.2 Modelo ARIMA
cat("\n=== Modelo ARIMA sobre la serie mensual ===\n")
fit_arima <- auto.arima(ts_ventas_mensual)
cat("Modelo ARIMA seleccionado automáticamente:\n")
print(fit_arima)
cat("\nResumen del modelo ARIMA:\n")
print(summary(fit_arima))
# Se realiza un forecast a 6 periodos (6 meses)
forecast_arima <- forecast(fit_arima, h = 6)
cat("\nPronóstico ARIMA a seis meses:\n")
print(forecast_arima)
# Gráfico del forecast
plot(
    forecast_arima, 
    main = "Pronóstico ARIMA (seis meses)",
    xlab = "Tiempo (mes)",
    ylab = "Ventas pronosticadas"
  )

# 6.2 Modelo de regresión lineal múltiple
cat("\n=== Modelo de regresión lineal mútliple para predecir ventas ===\n")
# Recalcular ordenes_por_mes y clientes_por_mes
ordenes_por_mes <- orders %>%
  mutate(year_month = floor_date(order_date, "month")) %>%
  group_by(year_month) %>%
  summarise(num_ordenes = n())
clientes_por_mes <- orders %>%
  mutate(year_month = floor_date(order_date, "month")) %>%
  group_by(year_month) %>%
  summarise(clientes_distintos = n_distinct(customer_id))
# Se genera un solo dataset
datos_regresion <- ordenes_por_mes %>%
  left_join(clientes_por_mes, by = "year_month") %>%
  left_join(ventas_por_mes,  by = "year_month")
# Se rellenan NA's
datos_regresion$num_ordenes[is.na(datos_regresion$num_ordenes)]               <- 0
datos_regresion$clientes_distintos[is.na(datos_regresion$clientes_distintos)] <- 0
datos_regresion$ventas_totales[is.na(datos_regresion$ventas_totales)]         <- 0
# Se crea el modelo
modelo <- lm(ventas_totales ~ num_ordenes + clientes_distintos,
             data = datos_regresion)
cat("\nResumen del modelo de Regresión:\n")
print(summary(modelo))

# Gráfico de matriz de correlación
variables_modelo <- datos_regresion[, c("num_ordenes", "clientes_distintos", "ventas_totales")]
matriz_correlacion <- round(cor(variables_modelo), 2)
sexto_grafico <- ggcorrplot(
    matriz_correlacion, 
    lab      = TRUE, 
    lab_size = 4, 
    hc.order = FALSE, 
    type     = "lower",
    colors   = c("#E46726", "white", "#6D9EC1"),
    title    = "Matriz de correlación de variables",
    ggtheme  = theme_minimal(base_size = 12)
  )
print(sexto_grafico)

#--------------------------------------------------------------------------------------------------#
# 7. Análisis estadístico inferencial 
#--------------------------------------------------------------------------------------------------#
cat("\n=== Análisis estadístico inferencial ===\n")

# 7.1 T-Test: Diferencia en "importe" promedio según  diez ciudades con más centas vs menos ventas
# Definir la clasificación más/menos.
city_classification <- rbind(
  data.frame(city = top_diez$city,     grupo_ciudad = "Top_diez"),
  data.frame(city = bottom_diez$city,  grupo_ciudad = "Bottom_diez")
)
df_ventas <- order_items %>%
  # Crear columna 'importe'
  mutate(importe = list_price * quantity * (1 - discount)) %>%  
  # Se une con 'orders' para tener 'order_date', entre otros.
  inner_join(orders, by = "order_id") %>%                       
  inner_join(customers, by = "customer_id")  
# Se une con 'df_ventas' para etiquetar cada fila con Top_diez o Bottom_diez
df_ventas_city <- df_ventas %>%
  left_join(city_classification, by = "city") %>%
  filter(grupo_ciudad %in% c("Top_diez", "Bottom_diez"))

# 7.2 Prueba t-student para comparar medias de "importe"
# (asumiendo hipotéticamente una diferencia de medias en la población)
cat("\n>>> T-Test de importe ~ grupo_ciudad (Top diez vs Bottom diez):\n")
t_test_result <- t.test(importe ~ grupo_ciudad, data = df_ventas_city)
print(t_test_result)

# 7.3 Shapiro-Wilk Test de Normalidad sobre los residuos del modelo lineal
cat("\n>>> Test de Normalidad (Shapiro-Wilk) en los residuos de la regresión:\n")
shapiro_test <- shapiro.test(residuals(modelo))
print(shapiro_test)

#--------------------------------------------------------------------------------------------------#
# 8. Conclusiones
#--------------------------------------------------------------------------------------------------#
cat("\n------------------ Conclusiones generales ------------------\n")
cat("1) Clientes:\n")
cat("   - Hay un total de", total_clientes, "clientes únicos.\n")
cat("   -", nrow(clientes_sin_orden), "clientes nunca han realizado una orden.\n")
cat("   - Observa las ciudades con mayor número de clientes en el gráfico de barras.\n\n")

cat("2) Órdenes:\n")
cat("   - Se visualiza la evolución mensual de órdenes.\n")
cat("   - Descuento promedio:", round(descuento_promedio*100, 2), "%.\n")
cat("   - La facturación mensual se muestra en el gráfico de líneas.\n\n")

cat("3) Series de Tiempo (descomposición y ARIMA):\n")
cat("   - La descomposición muestra tendencia, estacionalidad y residuo.\n")
cat("   - El modelo ARIMA sugiere un pronóstico y se puede refinar con más datos.\n\n")

cat("4) Regresión Lineal:\n")
cat("   - 'num_ordenes' y 'clientes_distintos' explican parte de la variabilidad\n",
    "     de 'ventas_totales'. Ver summary(modelo) para significancia.\n")
cat("   - La matriz de correlación sirve para comprender mejor las relaciones.\n\n")

cat("5) Análisis Estadístico Adicional:\n")
cat("   - T-test sugiere si existe (o no) diferencia estadísticamente significativa\n",
    "     entre el importe promedio en Top diez vs Bottom diez ciudades.\n")
cat("   - Shapiro-Wilk test verifica si los residuos de la regresión se distribuyen\n",
    "     aproximadamente de forma normal.\n\n")
