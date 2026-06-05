# =====================================================
# ANÁLISIS DE CORRELACIÓN Y PCA DE VARIABLES CLIMÁTICAS
# =====================================================

# Limpiar workspace
rm(list = ls())

# -------------------------------
# 1. PAQUETES
# -------------------------------
library(raster)
library(sf)
library(dplyr)
library(psych)
library(corrgram)

# -------------------------------
# 2. CONFIGURACIÓN DE RUTAS
# -------------------------------
PATHS <- list(
  ocurrencias = "C:/Users/JorgeCV/Desktop/TFM/Variables climaticas seleccionadas/habitats.csv",
  variables = "C:/Users/JorgeCV/Desktop/TFM/Variables climaticas seleccionadas/Variable climáticas presentes",
  output_dir = "C:/Users/JorgeCV/Desktop/TFM/Variables climaticas seleccionadas"
)

# Crear directorio de salida si no existe
if (!dir.exists(PATHS$output_dir)) dir.create(PATHS$output_dir, recursive = TRUE)

# -------------------------------
# 3. CARGAR DATOS DE OCURRENCIA
# -------------------------------
occurrence_data <- read.csv(PATHS$ocurrencias, header = TRUE)
str(occurrence_data)

# Seleccionar coordenadas
p_xy <- occurrence_data %>% select(Longitude, Latitude)

# -------------------------------
# 4. CARGAR VARIABLES PREDICTORAS
# -------------------------------
files <- list.files(path = PATHS$variables, pattern = ".tif$", full.names = TRUE)
predictors <- raster::stack(files)
print(predictors)

# -------------------------------
# 5. EXTRAER VALORES DE LAS VARIABLES
# -------------------------------
data_env <- extract(predictors, p_xy) %>% 
  data.frame() %>%
  na.omit()  # Eliminar filas con NA

head(data_env)

# -------------------------------
# 6. ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# -------------------------------
pca <- prcomp(data_env, scale = TRUE)
summary(pca)
plot(pca)

# Rotación (contribución de variables a los ejes)
rotation <- as.data.frame(pca$rotation)
write.csv(rotation, file.path(PATHS$output_dir, "PCA_rotation.csv"), row.names = TRUE)

# Valores de PCA para cada punto
axes <- predict(pca, newdata = data_env)[, 1:3]  # Primeros 3 ejes
environment.pca <- cbind(data_env, axes)

# -------------------------------
# 7. CORRELACIONES
# -------------------------------
correlations <- corr.test(environment.pca, use = "complete")

# Extraer y guardar matrices de correlación y p-valores
rval <- as.data.frame(correlations$r)
pval <- as.data.frame(correlations$p)

write.csv(rval, file.path(PATHS$output_dir, "R-value.csv"), row.names = TRUE)
write.csv(pval, file.path(PATHS$output_dir, "P-value.csv"), row.names = TRUE)

# -------------------------------
# 8. VISUALIZACIÓN DE CORRELACIONES
# -------------------------------
windows()  # Abrir ventana nueva en Windows
corrgram(environment.pca, order = TRUE, cex = 1.5, 
         lower.panel = panel.pie, upper.panel = NULL,
         main = "Correlograma de variables")

