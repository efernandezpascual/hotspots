# Cargar librerías necesarias
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

# Cargar los rasters
bio6_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio6_1981-2010_V.2.1_WGS_1km.tif")
presencia_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_helechos.tif")

# Extraer valores de los rasters
bio6_valores <- values(bio6_raster)
presencia_valores <- values(presencia_raster)

# Ver los primeros valores para confirmar
cat("Primeros 10 valores de bio6 (original):\n")
print(head(bio6_valores, 10))
cat("\nRango de bio6 original:", range(bio6_valores, na.rm = TRUE), "\n")
cat("Media de bio6 original:", mean(bio6_valores, na.rm = TRUE), "\n")

# Crear dataframe con los datos de presencia (para Subtropical  y hotspots)
datos_presencia <- data.frame(
  bio6 = bio6_valores,
  presencia = presencia_valores
)

# ELIMINAR SOLO FILAS CON NA
datos_presencia <- na.omit(datos_presencia)

# CONVERTIR bio6 DE KELVIN × 10 A GRADOS CELSIUS (°C)
datos_presencia$bio6_celsius <- datos_presencia$bio6 * 0.1 - 273.15

# Crear dataframe para Región Iberoatlantica (TODOS los píxeles del raster bio6)
datos_iberoatlantica <- data.frame(
  bio6_celsius = bio6_valores * 0.1 - 273.15,
  Region = "Región Iberoatlantica"
)
# Eliminar NA si los hay
datos_iberoatlantica <- na.omit(datos_iberoatlantica)

# Región Subtropical : píxeles CON presencia (1-9)
datos_Subtropical  <- datos_presencia %>%
  filter(presencia >= 1) %>%
  mutate(Region = "Región Subtropical ") %>%
  select(bio6_celsius, Region)

# Hotspots: píxeles con alta presencia (7-9)
datos_hotspots <- datos_presencia %>%
  filter(presencia >= 7) %>%
  mutate(Region = "Hotspots") %>%
  select(bio6_celsius, Region)

# Verificar los tamaños
cat("\n📊 Tamaño de cada región:\n")
cat("Región Iberoatlantica (todo el raster):", formatC(nrow(datos_iberoatlantica), big.mark = ","), "píxeles\n")
cat("Región Subtropical  (con presencia):", formatC(nrow(datos_Subtropical ), big.mark = ","), "píxeles\n")
cat("Hotspots (≥7 presencias):", formatC(nrow(datos_hotspots), big.mark = ","), "píxeles\n")

# Combinar todos los dataframes
datos_combinados <- bind_rows(
  datos_iberoatlantica,
  datos_Subtropical ,
  datos_hotspots
)

# Crear el boxplot con ggplot2 - TAMAÑOS AUMENTADOS
boxplot_bio6 <- ggplot(datos_combinados, aes(x = Region, y = bio6_celsius, fill = Region)) +
  geom_boxplot() +
  labs(
    title = "Bio6 (Minimum temperature of the coldest month) by Region",
    x = "Region",
    y = "bio6 (°C)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  ) +
  scale_fill_manual(values = c(
    "Región Iberoatlantica" = "skyblue",
    "Región Subtropical " = "orange",
    "Hotspots" = "darkgreen"
  )) +
  scale_x_discrete(labels = c(
    "Región Iberoatlantica" = "Iberian Atlantic",
    "Región Subtropical " = "Subtropical",
    "Hotspots" = "Hotspots"
  ))

# Mostrar el gráfico
print(boxplot_bio6)

# GUARDAR EL GRÁFICO
ruta_guardado <- "C:/Users/JorgeCV/Desktop/TFM/boxplots/presente helechos/bio6_boxplot_regiones_corregido.png"
if(!dir.exists(dirname(ruta_guardado))) {
  dir.create(dirname(ruta_guardado), recursive = TRUE)
}
ggsave(ruta_guardado, plot = boxplot_bio6, width = 10, height = 6, dpi = 300, bg = "white")
cat("\n✅ Gráfico guardado en:\n", ruta_guardado, "\n")

# Estadísticas detalladas por región
estadisticas <- datos_combinados %>%
  group_by(Region) %>%
  summarise(
    n = n(),
    media = mean(bio6_celsius, na.rm = TRUE),
    mediana = median(bio6_celsius, na.rm = TRUE),
    sd = sd(bio6_celsius, na.rm = TRUE),
    min = min(bio6_celsius, na.rm = TRUE),
    max = max(bio6_celsius, na.rm = TRUE),
    p25 = quantile(bio6_celsius, 0.25, na.rm = TRUE),
    p75 = quantile(bio6_celsius, 0.75, na.rm = TRUE)
  )

print(estadisticas)

# Guardar estadísticas
ruta_csv <- "C:/Users/JorgeCV/Desktop/TFM/boxplots/presente helechos/estadisticas_bio6_regiones_corregido.csv"
write.csv(estadisticas, ruta_csv, row.names = FALSE)
cat("\n📊 Estadísticas guardadas en:\n", ruta_csv, "\n")














rm(list=ls())

# Cargar librerías necesarias
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)

# Cargar los rasters
bio7_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio7_1981-2010_V.2.1_WGS_1km.tif")
presencia_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_helechos.tif")

# Extraer valores de los rasters
bio7_valores <- values(bio7_raster)
presencia_valores <- values(presencia_raster)

# Ver los primeros valores para confirmar
cat("Primeros 10 valores de bio7 (original):\n")
print(head(bio7_valores, 10))
cat("\nRango de bio7 original:", range(bio7_valores, na.rm = TRUE), "\n")
cat("Media de bio7 original:", mean(bio7_valores, na.rm = TRUE), "\n")

# Crear dataframe con los datos de presencia (para Subtropical  y hotspots)
datos_presencia <- data.frame(
  bio7 = bio7_valores,
  presencia = presencia_valores
)

# ELIMINAR SOLO FILAS CON NA
datos_presencia <- na.omit(datos_presencia)

# CONVERTIR bio7 (solo multiplicar por 0.1, sin restar 273.15)
datos_presencia$bio7_celsius <- datos_presencia$bio7 * 0.1

# Crear dataframe para Región Iberoatlantica (TODOS los píxeles del raster bio7)
datos_iberoatlantica <- data.frame(
  bio7_celsius = bio7_valores * 0.1,
  Region = "Región Iberoatlantica"
)
# Eliminar NA si los hay
datos_iberoatlantica <- na.omit(datos_iberoatlantica)

# Región Subtropical : píxeles CON presencia (1-9)
datos_Subtropical  <- datos_presencia %>%
  filter(presencia >= 1) %>%
  mutate(Region = "Región Subtropical ") %>%
  select(bio7_celsius, Region)

# Hotspots: píxeles con alta presencia (7-9)
datos_hotspots <- datos_presencia %>%
  filter(presencia >= 7) %>%
  mutate(Region = "Hotspots") %>%
  select(bio7_celsius, Region)

# Verificar los tamaños
cat("\n📊 Tamaño de cada región:\n")
cat("Región Iberoatlantica (todo el raster):", formatC(nrow(datos_iberoatlantica), big.mark = ","), "píxeles\n")
cat("Región Subtropical  (con presencia):", formatC(nrow(datos_Subtropical ), big.mark = ","), "píxeles\n")
cat("Hotspots (≥7 presencias):", formatC(nrow(datos_hotspots), big.mark = ","), "píxeles\n")

# Combinar todos los dataframes
datos_combinados <- bind_rows(
  datos_iberoatlantica,
  datos_Subtropical ,
  datos_hotspots
)

# Crear el boxplot con ggplot2 - TAMAÑOS AUMENTADOS
boxplot_bio7 <- ggplot(datos_combinados, aes(x = Region, y = bio7_celsius, fill = Region)) +
  geom_boxplot() +
  labs(
    title = "Bio7 (Annual Temperature Range) by region",
    x = "Region",
    y = "bio7 (°C)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  ) +
  scale_fill_manual(values = c(
    "Región Iberoatlantica" = "skyblue",
    "Región Subtropical " = "orange",
    "Hotspots" = "darkgreen"
  )) +
  scale_x_discrete(labels = c(
    "Región Iberoatlantica" = "Iberian Atlantic",
    "Región Subtropical " = "Subtropical",
    "Hotspots" = "Hotspots"
  ))

# Mostrar el gráfico
print(boxplot_bio7)

# GUARDAR EL GRÁFICO
ruta_guardado <- "C:/Users/JorgeCV/Desktop/TFM/boxplots/presente helechos/bio7_boxplot_regiones.png"
if(!dir.exists(dirname(ruta_guardado))) {
  dir.create(dirname(ruta_guardado), recursive = TRUE)
}
ggsave(ruta_guardado, plot = boxplot_bio7, width = 10, height = 6, dpi = 300, bg = "white")
cat("\n✅ Gráfico guardado en:\n", ruta_guardado, "\n")

# Estadísticas detalladas por región
estadisticas <- datos_combinados %>%
  group_by(Region) %>%
  summarise(
    n = n(),
    media = mean(bio7_celsius, na.rm = TRUE),
    mediana = median(bio7_celsius, na.rm = TRUE),
    sd = sd(bio7_celsius, na.rm = TRUE),
    min = min(bio7_celsius, na.rm = TRUE),
    max = max(bio7_celsius, na.rm = TRUE),
    p25 = quantile(bio7_celsius, 0.25, na.rm = TRUE),
    p75 = quantile(bio7_celsius, 0.75, na.rm = TRUE)
  )

print(estadisticas)

# Guardar estadísticas
ruta_csv <- "C:/Users/JorgeCV/Desktop/TFM/boxplots/presente helechos/estadisticas_bio7_regiones.csv"
write.csv(estadisticas, ruta_csv, row.names = FALSE)
cat("\n📊 Estadísticas guardadas en:\n", ruta_csv, "\n")






























# Limpiar el entorno
rm(list=ls())

# Cargar librerías necesarias
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)

# Cargar los rasters
bio14_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio14_1981-2010_V.2.1_WGS_1km.tif")
presencia_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_helechos.tif")

# Extraer valores de los rasters
bio14_valores <- values(bio14_raster)
presencia_valores <- values(presencia_raster)

# Ver los primeros valores para confirmar
cat("Primeros 10 valores de bio14 (original):\n")
print(head(bio14_valores, 10))
cat("\nRango de bio14 original:", range(bio14_valores, na.rm = TRUE), "\n")
cat("Media de bio14 original:", mean(bio14_valores, na.rm = TRUE), "\n")

# Crear dataframe con los datos de presencia (para Subtropical  y hotspots)
datos_presencia <- data.frame(
  bio14 = bio14_valores,
  presencia = presencia_valores
)

# ELIMINAR SOLO FILAS CON NA
datos_presencia <- na.omit(datos_presencia)

# CONVERTIR bio14 (precipitación: dividir entre 10 para pasar a mm)
# Los valores de precipitación en CHELSA vienen en kg/m² (equivalente a mm) × 10
datos_presencia$bio14_mm <- datos_presencia$bio14 * 0.1

# Crear dataframe para Región Iberoatlantica (TODOS los píxeles del raster bio14)
datos_iberoatlantica <- data.frame(
  bio14_mm = bio14_valores * 0.1,
  Region = "Región Iberoatlantica"
)
# Eliminar NA si los hay
datos_iberoatlantica <- na.omit(datos_iberoatlantica)

# Región Subtropical : píxeles CON presencia (1-9)
datos_Subtropical  <- datos_presencia %>%
  filter(presencia >= 1) %>%
  mutate(Region = "Región Subtropical ") %>%
  select(bio14_mm, Region)

# Hotspots: píxeles con alta presencia (7-9)
datos_hotspots <- datos_presencia %>%
  filter(presencia >= 7) %>%
  mutate(Region = "Hotspots") %>%
  select(bio14_mm, Region)

# Verificar los tamaños
cat("\n📊 Tamaño de cada región:\n")
cat("Región Iberoatlantica (todo el raster):", formatC(nrow(datos_iberoatlantica), big.mark = ","), "píxeles\n")
cat("Región Subtropical  (con presencia):", formatC(nrow(datos_Subtropical ), big.mark = ","), "píxeles\n")
cat("Hotspots (≥7 presencias):", formatC(nrow(datos_hotspots), big.mark = ","), "píxeles\n")

# Combinar todos los dataframes
datos_combinados <- bind_rows(
  datos_iberoatlantica,
  datos_Subtropical ,
  datos_hotspots
)

# Crear el boxplot con ggplot2 - TAMAÑOS AUMENTADOS
boxplot_bio14 <- ggplot(datos_combinados, aes(x = Region, y = bio14_mm, fill = Region)) +
  geom_boxplot() +
  labs(
    title = "Bio14 (Precipitation of the Driest Month) by region",
    x = "Region",
    y = "bio14 (mm)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  ) +
  scale_fill_manual(values = c(
    "Región Iberoatlantica" = "skyblue",
    "Región Subtropical " = "orange",
    "Hotspots" = "darkgreen"
  )) +
  scale_x_discrete(labels = c(
    "Región Iberoatlantica" = "Iberian Atlantic",
    "Región Subtropical " = "Subtropical",
    "Hotspots" = "Hotspots"
  ))

# Mostrar el gráfico
print(boxplot_bio14)

# GUARDAR EL GRÁFICO
ruta_guardado <- "C:/Users/JorgeCV/Desktop/TFM/boxplots/presente helechos/bio14_boxplot_regiones.png"
if(!dir.exists(dirname(ruta_guardado))) {
  dir.create(dirname(ruta_guardado), recursive = TRUE)
}
ggsave(ruta_guardado, plot = boxplot_bio14, width = 10, height = 6, dpi = 300, bg = "white")
cat("\n✅ Gráfico guardado en:\n", ruta_guardado, "\n")

# Estadísticas detalladas por región
estadisticas <- datos_combinados %>%
  group_by(Region) %>%
  summarise(
    n = n(),
    media = mean(bio14_mm, na.rm = TRUE),
    mediana = median(bio14_mm, na.rm = TRUE),
    sd = sd(bio14_mm, na.rm = TRUE),
    min = min(bio14_mm, na.rm = TRUE),
    max = max(bio14_mm, na.rm = TRUE),
    p25 = quantile(bio14_mm, 0.25, na.rm = TRUE),
    p75 = quantile(bio14_mm, 0.75, na.rm = TRUE)
  )

print(estadisticas)

# Guardar estadísticas
ruta_csv <- "C:/Users/JorgeCV/Desktop/TFM/boxplots/presente helechos/estadisticas_bio14_regiones.csv"
write.csv(estadisticas, ruta_csv, row.names = FALSE)
cat("\n📊 Estadísticas guardadas en:\n", ruta_csv, "\n")































# Limpiar el entorno
rm(list=ls())

# Cargar librerías necesarias
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)

# Cargar los rasters
ph_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Variables/PH_study_area_WGS_final_1km.tif")
presencia_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_helechos.tif")

# Extraer valores de los rasters
ph_valores <- values(ph_raster)
presencia_valores <- values(presencia_raster)

# Ver los primeros valores para confirmar
cat("Primeros 10 valores de pH (original):\n")
print(head(ph_valores, 10))
cat("\nRango de pH original:", range(ph_valores, na.rm = TRUE), "\n")
cat("Media de pH original:", mean(ph_valores, na.rm = TRUE), "\n")

# Crear dataframe con los datos de presencia (para Subtropical  y hotspots)
datos_presencia <- data.frame(
  ph = ph_valores,
  presencia = presencia_valores
)

# ELIMINAR SOLO FILAS CON NA
datos_presencia <- na.omit(datos_presencia)

# CONVERTIR pH (dividir entre 10 porque probablemente está multiplicado por 10)
datos_presencia$ph_valor <- datos_presencia$ph / 10

# Crear dataframe para Región Iberoatlantica (TODOS los píxeles del raster pH)
datos_iberoatlantica <- data.frame(
  ph_valor = ph_valores / 10,
  Region = "Región Iberoatlantica"
)
# Eliminar NA si los hay
datos_iberoatlantica <- na.omit(datos_iberoatlantica)

# Región Subtropical : píxeles CON presencia (1-9)
datos_Subtropical  <- datos_presencia %>%
  filter(presencia >= 1) %>%
  mutate(Region = "Región Subtropical ") %>%
  select(ph_valor, Region)

# Hotspots: píxeles con alta presencia (7-9)
datos_hotspots <- datos_presencia %>%
  filter(presencia >= 7) %>%
  mutate(Region = "Hotspots") %>%
  select(ph_valor, Region)

# Verificar los tamaños
cat("\n📊 Tamaño de cada región:\n")
cat("Región Iberoatlantica (todo el raster):", formatC(nrow(datos_iberoatlantica), big.mark = ","), "píxeles\n")
cat("Región Subtropical  (con presencia):", formatC(nrow(datos_Subtropical ), big.mark = ","), "píxeles\n")
cat("Hotspots (≥7 presencias):", formatC(nrow(datos_hotspots), big.mark = ","), "píxeles\n")

# Combinar todos los dataframes
datos_combinados <- bind_rows(
  datos_iberoatlantica,
  datos_Subtropical ,
  datos_hotspots
)

# Verificar los valores después de la conversión
cat("\n📊 Valores de pH después de conversión (dividido entre 10):\n")
cat("Rango:", range(datos_combinados$ph_valor, na.rm = TRUE), "\n")
cat("Media global:", mean(datos_combinados$ph_valor, na.rm = TRUE), "\n")

# Crear el boxplot con ggplot2 - TAMAÑOS AUMENTADOS
boxplot_ph <- ggplot(datos_combinados, aes(x = Region, y = ph_valor, fill = Region)) +
  geom_boxplot() +
  labs(
    title = "Soil pH by region",
    x = "Region",
    y = "pH"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  ) +
  scale_fill_manual(values = c(
    "Región Iberoatlantica" = "skyblue",
    "Región Subtropical " = "orange",
    "Hotspots" = "darkgreen"
  )) +
  scale_x_discrete(labels = c(
    "Región Iberoatlantica" = "Iberian Atlantic",
    "Región Subtropical " = "Subtropical",
    "Hotspots" = "Hotspots"
  ))

# Mostrar el gráfico
print(boxplot_ph)

# GUARDAR EL GRÁFICO
ruta_guardado <- "C:/Users/JorgeCV/Desktop/TFM/boxplots/presente helechos/ph_boxplot_regiones_corregido.png"
if(!dir.exists(dirname(ruta_guardado))) {
  dir.create(dirname(ruta_guardado), recursive = TRUE)
}
ggsave(ruta_guardado, plot = boxplot_ph, width = 10, height = 6, dpi = 300, bg = "white")
cat("\n✅ Gráfico guardado en:\n", ruta_guardado, "\n")

# Estadísticas detalladas por región
estadisticas <- datos_combinados %>%
  group_by(Region) %>%
  summarise(
    n = n(),
    media = mean(ph_valor, na.rm = TRUE),
    mediana = median(ph_valor, na.rm = TRUE),
    sd = sd(ph_valor, na.rm = TRUE),
    min = min(ph_valor, na.rm = TRUE),
    max = max(ph_valor, na.rm = TRUE),
    p25 = quantile(ph_valor, 0.25, na.rm = TRUE),
    p75 = quantile(ph_valor, 0.75, na.rm = TRUE)
  )

print(estadisticas)

# Guardar estadísticas
ruta_csv <- "C:/Users/JorgeCV/Desktop/TFM/boxplots/presente helechos/estadisticas_ph_regiones_corregido.csv"
write.csv(estadisticas, ruta_csv, row.names = FALSE)
cat("\n📊 Estadísticas guardadas en:\n", ruta_csv, "\n")

# Análisis adicional: ¿Los hotspots prefieren suelos más ácidos?
cat("\n📈 Análisis de acidez del suelo:\n")
cat("Diferencia Hotspots - Iberoatlantica:", 
    round(mean(datos_hotspots$ph_valor) - mean(datos_iberoatlantica$ph_valor), 2), "unidades pH\n")
cat("Los hotspots tienen suelos ", 
    ifelse(mean(datos_hotspots$ph_valor) < mean(datos_iberoatlantica$ph_valor), "más ácidos", "más básicos"), 
    " que el promedio regional\n")


























# Limpiar el entorno
rm(list=ls())

# Cargar librerías necesarias
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)

# Cargar los rasters
arena_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Variables/sand_study_area_WGS_final_1km.tif")
presencia_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_helechos.tif")

# Extraer valores de los rasters
arena_valores <- values(arena_raster)
presencia_valores <- values(presencia_raster)

# Ver los primeros valores para confirmar
cat("Primeros 10 valores de arena (original):\n")
print(head(arena_valores, 10))
cat("\nRango de arena original:", range(arena_valores, na.rm = TRUE), "\n")
cat("Media de arena original:", mean(arena_valores, na.rm = TRUE), "\n")

# Crear dataframe con los datos de presencia (para Subtropical  y hotspots)
datos_presencia <- data.frame(
  arena = arena_valores,
  presencia = presencia_valores
)

# ELIMINAR SOLO FILAS CON NA
datos_presencia <- na.omit(datos_presencia)

# CONVERTIR arena (probablemente viene en porcentaje 0-100, sin conversión)
# Si los valores son muy altos (ej. 5000), habría que dividir, pero parecen porcentajes normales
datos_presencia$arena_porcentaje <- datos_presencia$arena

# Crear dataframe para Región Iberoatlantica (TODOS los píxeles del raster arena)
datos_iberoatlantica <- data.frame(
  arena_porcentaje = arena_valores,
  Region = "Región Iberoatlantica"
)
# Eliminar NA si los hay
datos_iberoatlantica <- na.omit(datos_iberoatlantica)

# Región Subtropical : píxeles CON presencia (1-9)
datos_Subtropical  <- datos_presencia %>%
  filter(presencia >= 1) %>%
  mutate(Region = "Región Subtropical ") %>%
  select(arena_porcentaje, Region)

# Hotspots: píxeles con alta presencia (7-9)
datos_hotspots <- datos_presencia %>%
  filter(presencia >= 7) %>%
  mutate(Region = "Hotspots") %>%
  select(arena_porcentaje, Region)

# Verificar los tamaños
cat("\n📊 Tamaño de cada región:\n")
cat("Región Iberoatlantica (todo el raster):", formatC(nrow(datos_iberoatlantica), big.mark = ","), "píxeles\n")
cat("Región Subtropical  (con presencia):", formatC(nrow(datos_Subtropical ), big.mark = ","), "píxeles\n")
cat("Hotspots (≥7 presencias):", formatC(nrow(datos_hotspots), big.mark = ","), "píxeles\n")

# Combinar todos los dataframes
datos_combinados <- bind_rows(
  datos_iberoatlantica,
  datos_Subtropical ,
  datos_hotspots
)

# Verificar los valores
cat("\n📊 Valores de arena (porcentaje):\n")
cat("Rango:", range(datos_combinados$arena_porcentaje, na.rm = TRUE), "\n")
cat("Media global:", mean(datos_combinados$arena_porcentaje, na.rm = TRUE), "\n")

# Crear el boxplot con ggplot2 - TAMAÑOS AUMENTADOS
boxplot_arena <- ggplot(datos_combinados, aes(x = Region, y = arena_porcentaje, fill = Region)) +
  geom_boxplot() +
  labs(
    title = "Percentage of sand in the soil by region",
    x = "Region",
    y = "Sand (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 12)
  ) +
  scale_fill_manual(values = c(
    "Región Iberoatlantica" = "skyblue",
    "Región Subtropical " = "orange",
    "Hotspots" = "darkgreen"
  )) +
  scale_x_discrete(labels = c(
    "Región Iberoatlantica" = "Iberian Atlantic",
    "Región Subtropical " = "Subtropical",
    "Hotspots" = "Hotspots"
  ))

# Mostrar el gráfico
print(boxplot_arena)

# GUARDAR EL GRÁFICO
ruta_guardado <- "C:/Users/JorgeCV/Desktop/TFM/boxplots/presente helechos/arena_boxplot_regiones.png"
if(!dir.exists(dirname(ruta_guardado))) {
  dir.create(dirname(ruta_guardado), recursive = TRUE)
}
ggsave(ruta_guardado, plot = boxplot_arena, width = 10, height = 6, dpi = 300, bg = "white")
cat("\n✅ Gráfico guardado en:\n", ruta_guardado, "\n")

# Estadísticas detalladas por región
estadisticas <- datos_combinados %>%
  group_by(Region) %>%
  summarise(
    n = n(),
    media = mean(arena_porcentaje, na.rm = TRUE),
    mediana = median(arena_porcentaje, na.rm = TRUE),
    sd = sd(arena_porcentaje, na.rm = TRUE),
    min = min(arena_porcentaje, na.rm = TRUE),
    max = max(arena_porcentaje, na.rm = TRUE),
    p25 = quantile(arena_porcentaje, 0.25, na.rm = TRUE),
    p75 = quantile(arena_porcentaje, 0.75, na.rm = TRUE)
  )

print(estadisticas)

# Guardar estadísticas
ruta_csv <- "C:/Users/JorgeCV/Desktop/TFM/boxplots/presente helechos/estadisticas_arena_regiones.csv"
write.csv(estadisticas, ruta_csv, row.names = FALSE)
cat("\n📊 Estadísticas guardadas en:\n", ruta_csv, "\n")

# Análisis adicional: ¿Los hotspots prefieren suelos más arenosos?
cat("\n📈 Análisis de textura del suelo:\n")
dif_hotspots_ibero <- mean(datos_hotspots$arena_porcentaje) - mean(datos_iberoatlantica$arena_porcentaje)
dif_hotspots_subtrop <- mean(datos_hotspots$arena_porcentaje) - mean(datos_Subtropical $arena_porcentaje)

cat("Diferencia Hotspots - Iberoatlantica:", round(dif_hotspots_ibero, 2), "%\n")
cat("Diferencia Hotspots - Subtropical :", round(dif_hotspots_subtrop, 2), "%\n")
cat("Los hotspots tienen suelos ", 
    ifelse(dif_hotspots_ibero > 0, "más arenosos", "menos arenosos"), 
    " que el promedio regional\n")





















# Limpiar el entorno
rm(list=ls())

# Cargar librerías necesarias
library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(gridExtra)
library(grid)
library(forcats)

# Cargar los rasters de todas las variables
bio6_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio6_1981-2010_V.2.1_WGS_1km.tif")
bio7_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio7_1981-2010_V.2.1_WGS_1km.tif")
bio14_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio14_1981-2010_V.2.1_WGS_1km.tif")
ph_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Variables/PH_study_area_WGS_final_1km.tif")
arena_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Variables/sand_study_area_WGS_final_1km.tif")
presencia_raster <- raster("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_helechos.tif")

# Extraer valores de los rasters
bio6_valores <- values(bio6_raster)
bio7_valores <- values(bio7_raster)
bio14_valores <- values(bio14_raster)
ph_valores <- values(ph_raster)
arena_valores <- values(arena_raster)
presencia_valores <- values(presencia_raster)

# Crear dataframe con todas las variables (solo para Hotspots y Subtropical )
datos_presencia <- data.frame(
  bio6 = bio6_valores,
  bio7 = bio7_valores,
  bio14 = bio14_valores,
  ph = ph_valores,
  arena = arena_valores,
  presencia = presencia_valores
)

# ELIMINAR FILAS CON NA
datos_presencia <- na.omit(datos_presencia)

# APLICAR CONVERSIONES
datos_presencia$bio6_celsius <- datos_presencia$bio6 * 0.1 - 273.15
datos_presencia$bio7_celsius <- datos_presencia$bio7 * 0.1
datos_presencia$bio14_mm <- datos_presencia$bio14 * 0.1
datos_presencia$ph_valor <- datos_presencia$ph / 10
datos_presencia$arena_porcentaje <- datos_presencia$arena

# Crear columna de región para Hotspots y Subtropical 
datos_presencia <- datos_presencia %>%
  mutate(Region = case_when(
    presencia >= 7 ~ "Hotspots",
    presencia >= 1 ~ "Subtropical"
  ))

# AHORA CREAR REGIÓN IBEROATLANTICA (TODOS LOS PÍXELES DEL RASTER)
# Crear dataframe independiente para Iberoatlantica
datos_iberoatlantica <- data.frame(
  bio6_celsius = bio6_valores * 0.1 - 273.15,
  bio7_celsius = bio7_valores * 0.1,
  bio14_mm = bio14_valores * 0.1,
  ph_valor = ph_valores / 10,
  arena_porcentaje = arena_valores,
  Region = "Iberian Atlantic"
)

# Eliminar NA en Iberoatlantica
datos_iberoatlantica <- na.omit(datos_iberoatlantica)

# COMBINAR AMBOS DATAFRAMES
datos_completos <- bind_rows(datos_presencia, datos_iberoatlantica)

# Verificar distribución AHORA SÍ CON LAS TRES REGIONES
cat("\n📊 Region distribution (NOW WITH IBEROATLANTIC):\n")
print(table(datos_completos$Region))

# Seleccionar variables para PCA
datos_pca <- datos_completos %>%
  select(bio6_celsius, bio7_celsius, bio14_mm, ph_valor, arena_porcentaje, Region)

# Realizar PCA con FactoMineR
pca_result <- PCA(datos_pca[, 1:5], scale.unit = TRUE, graph = FALSE)

# Renombrar las dimensiones
colnames(pca_result$eig) <- c("eigenvalue", "percentage of variance", "cumulative percentage of variance")
rownames(pca_result$eig) <- c("PC1", "PC2", "PC3", "PC4", "PC5")

colnames(pca_result$var$coord) <- c("PC1", "PC2", "PC3", "PC4", "PC5")
colnames(pca_result$var$contrib) <- c("PC1", "PC2", "PC3", "PC4", "PC5")
colnames(pca_result$var$cos2) <- c("PC1", "PC2", "PC3", "PC4", "PC5")

colnames(pca_result$ind$coord) <- c("PC1", "PC2", "PC3", "PC4", "PC5")
colnames(pca_result$ind$contrib) <- c("PC1", "PC2", "PC3", "PC4", "PC5")
colnames(pca_result$ind$cos2) <- c("PC1", "PC2", "PC3", "PC4", "PC5")

# Definir ruta de guardado
ruta_guardado <- "C:/Users/JorgeCV/Desktop/TFM/boxplots/presente helechos/"

# Crear directorio si no existe
if(!dir.exists(ruta_guardado)) {
  dir.create(ruta_guardado, recursive = TRUE)
}

# ============================================================
# 1. GRÁFICO DE CONTRIBUCIONES - BARPLOT
# ============================================================
contribuciones <- as.data.frame(pca_result$var$contrib)
contribuciones$variable <- rownames(contribuciones)

# Para PC1
contrib_pc1 <- contribuciones %>%
  select(variable, PC1) %>%
  arrange(desc(PC1))

plot_contrib_pc1 <- ggplot(contrib_pc1, aes(x = reorder(variable, PC1), y = PC1, fill = PC1)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Variable contribution to PC1",
       subtitle = paste0("Explains ", round(pca_result$eig[1, 2], 1), "% of total variance"),
       x = "Variable",
       y = "Contribution (%)") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12))

print(plot_contrib_pc1)
ggsave(paste0(ruta_guardado, "pca_contribucion_PC1.png"), 
       plot = plot_contrib_pc1, width = 8, height = 5, dpi = 300)

# Para PC2
contrib_pc2 <- contribuciones %>%
  select(variable, PC2) %>%
  arrange(desc(PC2))

plot_contrib_pc2 <- ggplot(contrib_pc2, aes(x = reorder(variable, PC2), y = PC2, fill = PC2)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Variable contribution to PC2",
       subtitle = paste0("Explains ", round(pca_result$eig[2, 2], 1), "% of total variance"),
       x = "Variable",
       y = "Contribution (%)") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12))

print(plot_contrib_pc2)
ggsave(paste0(ruta_guardado, "pca_contribucion_PC2.png"), 
       plot = plot_contrib_pc2, width = 8, height = 5, dpi = 300)

# ============================================================
# 2. GRÁFICO DE COS2 - CALIDAD DE REPRESENTACIÓN
# ============================================================
cos2 <- as.data.frame(pca_result$var$cos2)
cos2$variable <- rownames(cos2)

cos2_pc1_pc2 <- cos2 %>%
  select(variable, PC1, PC2) %>%
  tidyr::pivot_longer(cols = c(PC1, PC2), names_to = "PC", values_to = "cos2")

plot_cos2 <- ggplot(cos2_pc1_pc2, aes(x = variable, y = cos2, fill = PC)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Quality of representation of variables",
       subtitle = "Cos2 - Proximity to correlation circle (1 = perfect)",
       x = "Variable",
       y = "Cos2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

print(plot_cos2)
ggsave(paste0(ruta_guardado, "pca_cos2_variables.png"), 
       plot = plot_cos2, width = 8, height = 6, dpi = 300)

# ============================================================
# 3. MAPA DE CALOR DE CONTRIBUCIONES
# ============================================================
contrib_matrix <- as.matrix(pca_result$var$contrib)
rownames(contrib_matrix) <- gsub("_", " ", rownames(contrib_matrix))
colnames(contrib_matrix) <- c("PC1", "PC2", "PC3", "PC4", "PC5")

contrib_long <- as.data.frame(contrib_matrix) %>%
  tibble::rownames_to_column("variable") %>%
  tidyr::pivot_longer(cols = starts_with("PC"), names_to = "PC", values_to = "contribucion")

heatmap_contrib <- ggplot(contrib_long, aes(x = PC, y = variable, fill = contribucion)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", name = "Contribution (%)") +
  labs(title = "Contribution heatmap",
       subtitle = "Intense red = higher contribution to PC",
       x = "Principal Component",
       y = "Variable") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 12))

print(heatmap_contrib)
ggsave(paste0(ruta_guardado, "pca_heatmap_contribuciones.png"), 
       plot = heatmap_contrib, width = 8, height = 6, dpi = 300)

# ============================================================
# 4. GRÁFICO DE CORRELACIONES VARIABLES-PC
# ============================================================
correlaciones <- as.data.frame(pca_result$var$coord)
correlaciones$variable <- rownames(correlaciones)

correlaciones_long <- correlaciones %>%
  select(variable, PC1, PC2, PC3) %>%
  tidyr::pivot_longer(cols = c(PC1, PC2, PC3), names_to = "PC", values_to = "correlacion")

plot_correlaciones <- ggplot(correlaciones_long, aes(x = variable, y = correlacion, fill = PC)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Correlation of variables with first components",
       x = "Variable",
       y = "Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

print(plot_correlaciones)
ggsave(paste0(ruta_guardado, "pca_correlaciones_PCs.png"), 
       plot = plot_correlaciones, width = 10, height = 6, dpi = 300)

# ============================================================
# 5. GRÁFICO DE AUTO-VALORES (SCREE PLOT)
# ============================================================
varianza <- data.frame(
  PC = c("PC1", "PC2", "PC3", "PC4", "PC5"),
  Varianza = pca_result$eig[, 2],
  Acumulado = pca_result$eig[, 3]
)

scree_plot <- ggplot(varianza, aes(x = PC, y = Varianza)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Varianza, 1), "%")), vjust = -0.5, size = 5) +
  geom_line(aes(y = Acumulado/2, group = 1), color = "red", size = 1) +
  geom_point(aes(y = Acumulado/2), color = "red", size = 2) +
  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Cumulative variance (%)")) +
  labs(title = "Variance explained by component",
       subtitle = "Bars: individual variance | Red line: cumulative variance",
       x = "Principal component",
       y = "Variance explained (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12))

print(scree_plot)
ggsave(paste0(ruta_guardado, "pca_scree_plot_mejorado.png"), 
       plot = scree_plot, width = 8, height = 6, dpi = 300)

# ============================================================
# 6. GRÁFICO RESUMEN - INTERPRETACIÓN DE PC1 Y PC2
# ============================================================
interpretacion <- data.frame(
  PC = c("PC1", "PC2"),
  Varianza = c(pca_result$eig[1, 2], pca_result$eig[2, 2]),
  Interpretacion = c(
    "Moisture and soil texture axis\n+ bio14: higher precipitation\n- sand: less sand (more clay)",
    "Temperature and acidity axis\n+ bio7: higher thermal range\n+ pH: more basic soils\n- bio6: colder winters"
  ),
  Color = c("skyblue", "darkgreen")
)

plot_interpretacion <- ggplot(interpretacion, aes(x = PC, y = Varianza, fill = PC)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(Varianza, 1), "%")), vjust = -0.5, size = 5) +
  geom_text(aes(label = Interpretacion), y = 5, size = 4, hjust = 0) +
  labs(title = "Interpretation of principal components",
       subtitle = paste0("PC1: ", round(pca_result$eig[1, 2], 1), 
                         "% | PC2: ", round(pca_result$eig[2, 2], 1), 
                         "% | Total explained: ", round(pca_result$eig[2, 3], 1), "%"),
       x = "",
       y = "Variance explained (%)") +
  ylim(0, 50) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "none",
        axis.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 14, face = "bold"))

print(plot_interpretacion)
ggsave(paste0(ruta_guardado, "pca_interpretacion_resumen.png"), 
       plot = plot_interpretacion, width = 10, height = 6, dpi = 300)

# ============================================================
# 7. TABLA RESUMEN EN FORMATO IMAGEN
# ============================================================
tabla_contrib <- data.frame(
  Variable = c("bio14_mm (Precipitation)", 
               "arena_porcentaje (Sand)", 
               "bio6_celsius (Min. temperature)", 
               "bio7_celsius (Thermal range)", 
               "ph_valor (pH)"),
  PC1 = round(pca_result$var$contrib[, 1], 1),
  PC2 = round(pca_result$var$contrib[, 2], 1),
  PC3 = round(pca_result$var$contrib[, 3], 1)
)

tabla_grob <- tableGrob(tabla_contrib, rows = NULL, 
                        theme = ttheme_minimal(
                          core = list(fg_params = list(hjust = 1, x = 0.95, fontsize = 12)),
                          colhead = list(fg_params = list(fontface = "bold", fontsize = 14))))

title_grob <- textGrob("Variable contribution to each PC (%)", 
                       gp = gpar(fontsize = 18, fontface = "bold"))

tabla_final <- grid.arrange(title_grob, tabla_grob, 
                            ncol = 1, heights = c(0.1, 0.9))

ggsave(paste0(ruta_guardado, "pca_tabla_contribuciones.png"), 
       plot = tabla_final, width = 10, height = 5, dpi = 300)

# ============================================================
# 8. BIPLOT SIMPLE CON LEYENDA Y TEXTOS REPOSICIONADOS
# ============================================================
set.seed(123)

# Verificar tamaños de cada región
cat("\n📊 Region sizes for sampling:\n")
print(table(datos_pca$Region))

# Tomar muestra representativa de CADA región (máximo 1500 por región)
muestra_hotspots <- sample(which(datos_pca$Region == "Hotspots"), 
                           min(800, sum(datos_pca$Region == "Hotspots")))
muestra_Subtropical <- sample(which(datos_pca$Region == "Subtropical"), 
                              min(1200, sum(datos_pca$Region == "Subtropical")))
muestra_ibero <- sample(which(datos_pca$Region == "Iberian Atlantic"), 
                        min(1500, sum(datos_pca$Region == "Iberian Atlantic")))

muestra <- c(muestra_hotspots, muestra_Subtropical, muestra_ibero)

coordenadas_ind <- as.data.frame(pca_result$ind$coord[muestra, 1:2])
coordenadas_ind$Region <- datos_pca$Region[muestra]
colnames(coordenadas_ind)[1:2] <- c("PC1", "PC2")

# Verificar que están las tres regiones
cat("\n📊 Regions in biplot (sample):\n")
print(table(coordenadas_ind$Region))

coordenadas_var <- as.data.frame(pca_result$var$coord[, 1:2])
coordenadas_var$variable <- rownames(coordenadas_var)
colnames(coordenadas_var)[1:2] <- c("PC1", "PC2")

# Escalar variables para visualización
factor_escala <- 3

# Colores para regiones - LLAMATIVOS
colores_regiones <- c("Iberian Atlantic" = "grey", 
                      "Subtropical" = "orange", 
                      "Hotspots" = "darkgreen")

coordenadas_ind <- coordenadas_ind %>%
  mutate(Region = as.factor(Region)) %>%
  mutate(Region = fct_relevel(Region, "Iberian Atlantic", "Subtropical", "Hotspots"))

# Definir posiciones personalizadas para las etiquetas de las variables
# para evitar superposición con las flechas
etiquetas_vars <- data.frame(
  variable = c("bio6_celsius", "bio7_celsius", "bio14_mm", "ph_valor", "arena_porcentaje"),
  PC1 = coordenadas_var$PC1 * factor_escala,
  PC2 = coordenadas_var$PC2 * factor_escala,
  label = c("Min. Temp.\n(Coldest Month)", 
            "Temp. Range", 
            "Precip.\n(Driest Month)",
            "pH", 
            "Sand"),
  # Ajustar posiciones para evitar superposición
  offset_x = c(-0.5, 0.5, -0.7, 0.2, 0.2),
  offset_y = c(-0.5, 0.05, 0.4, 0.4, -0.2)
)

# Calcular posiciones finales con offsets
etiquetas_vars$x_final <- etiquetas_vars$PC1 + etiquetas_vars$offset_x
etiquetas_vars$y_final <- etiquetas_vars$PC2 + etiquetas_vars$offset_y

# Biplot simple con leyenda y textos reposicionados
biplot_simple <- ggplot() +
  # Puntos de los individuos (con leyenda) - ALPHA = 0.6
  geom_point(data = coordenadas_ind, 
             aes(x = PC1, y = PC2, color = Region), 
             size = 1.8, alpha = 0.6) +
  # Flechas de las variables
  geom_segment(data = coordenadas_var,
               aes(x = 0, y = 0, xend = PC1 * factor_escala, yend = PC2 * factor_escala),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "black", size = 1) +
  # Etiquetas de las variables con posiciones ajustadas
  geom_text(data = etiquetas_vars,
            aes(x = x_final, y = y_final, label = label),
            size = 5, fontface = "bold", lineheight = 0.8) +
  # Líneas de referencia
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  # Colores manuales
  scale_color_manual(values = colores_regiones,
                     name = "Region") +
  # Títulos
  labs(title = "Principal Component Analysis",
       subtitle = paste0("PC1: ", round(pca_result$eig[1, 2], 1), 
                         "% | PC2: ", round(pca_result$eig[2, 2], 1), 
                         "% | Total: ", round(pca_result$eig[2, 3], 1), "%"),
       x = paste0("PC1 (", round(pca_result$eig[1, 2], 1), "%) - Moisture/Soil"),
       y = paste0("PC2 (", round(pca_result$eig[2, 2], 1), "%) - Temperature"),
       color = "Region") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 18),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank())

print(biplot_simple)
ggsave(paste0(ruta_guardado, "pca_biplot_completo_con_leyenda.png"), 
       plot = biplot_simple, width = 11, height = 8, dpi = 300)

# ============================================================
# 9. VERSIÓN CON ELIPSES DE CONFIANZA Y TEXTOS REPOSICIONADOS
# ============================================================
biplot_elipses <- ggplot() +
  # Elipses de confianza por región - ALPHA = 0.2
  stat_ellipse(data = coordenadas_ind,
               aes(x = PC1, y = PC2, color = Region, fill = Region),
               geom = "polygon", alpha = 0.2, level = 0.95, show.legend = FALSE) +
  # Puntos de los individuos - ALPHA = 0.6
  geom_point(data = coordenadas_ind, 
             aes(x = PC1, y = PC2, color = Region), 
             size = 2, alpha = 0.6) +
  # Flechas de las variables
  geom_segment(data = coordenadas_var,
               aes(x = 0, y = 0, xend = PC1 * factor_escala, yend = PC2 * factor_escala),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "#36454F", size = 1) +
  # Etiquetas de las variables
  geom_text(data = etiquetas_vars,
            aes(x = x_final, y = y_final, 
                label = c("Min. Temp.\nColdest Month",
                          "Temp.\nAnnual Range",
                          "Precip.\nDriest Month",
                          "pH", 
                          "Sand")),
            size = 5, fontface = "bold", lineheight = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
  scale_color_manual(values = colores_regiones, name = NULL) +
  scale_fill_manual(values = colores_regiones, guide = "none") +
  guides(color = guide_legend(override.aes = list(size = 6))) +  # <- aquí se agrandan los puntos de la leyenda
  labs(x = paste0("PC1 (", round(pca_result$eig[1, 2], 1), "%)"),
       y = paste0("PC2 (", round(pca_result$eig[2, 2], 1), "%)")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 15))

print(biplot_elipses)
ggsave(paste0(ruta_guardado, "pca_biplot_elipses_con_leyenda.png"), 
       plot = biplot_elipses, width = 11, height = 8, dpi = 300)
# ============================================================
# 10. RESULTADOS EN CONSOLA
# ============================================================
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("📊 INTERPRETATION OF PRINCIPAL COMPONENTS\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

cat("📈 EXPLAINED VARIANCE:\n")
for(i in 1:5) {
  cat(sprintf("  %s: %.1f%% (cumulative: %.1f%%)\n", 
              rownames(pca_result$eig)[i], 
              pca_result$eig[i, 2], 
              pca_result$eig[i, 3]))
}

cat("\n📊 CONTRIBUTIONS TO PC1 (", round(pca_result$eig[1, 2], 1), "%):\n", sep="")
contrib_pc1_sorted <- sort(pca_result$var$contrib[, 1], decreasing = TRUE)
for(i in 1:length(contrib_pc1_sorted)) {
  cat(sprintf("  %s: %.1f%%\n", names(contrib_pc1_sorted)[i], contrib_pc1_sorted[i]))
}

cat("\n📊 CONTRIBUTIONS TO PC2 (", round(pca_result$eig[2, 2], 1), "%):\n", sep="")
contrib_pc2_sorted <- sort(pca_result$var$contrib[, 2], decreasing = TRUE)
for(i in 1:length(contrib_pc2_sorted)) {
  cat(sprintf("  %s: %.1f%%\n", names(contrib_pc2_sorted)[i], contrib_pc2_sorted[i]))
}

cat("\n🔍 INTERPRETATION:\n")
cat("  • PC1 (Moisture/soil axis):\n")
cat("    - Positive: higher precipitation in driest month (bio14), more clayey soils (less sand)\n")
cat("    - Negative: lower precipitation, sandier soils\n\n")

cat("  • PC2 (Temperature axis):\n")
cat("    - Positive: higher thermal range (bio7), more basic soils (higher pH)\n")
cat("    - Negative: lower thermal range, milder winters (higher bio6), more acidic soils\n\n")

cat("🌿 POSITION OF REGIONS (based on centroids):\n")
# Calcular medias para cada región
centroides_pca <- datos_pca %>%
  group_by(Region) %>%
  summarise(
    PC1 = mean(pca_result$ind$coord[datos_pca$Region == Region, 1]),
    PC2 = mean(pca_result$ind$coord[datos_pca$Region == Region, 2])
  )

print(centroides_pca)

cat("\n✅ All graphics saved in:\n", ruta_guardado, "\n")