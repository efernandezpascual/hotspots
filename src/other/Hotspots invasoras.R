# ==========================================================
# 1️⃣ LIBRERÍAS
# ==========================================================
rm(list=ls())
library(raster)
library(dplyr)
library(tidyr)
library(openxlsx)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

# ==========================================================
# 2️⃣ CARGAR RASTERS DE ESPECIES (PRESENTE - Invasoras)
# ==========================================================

lista_rasters <- list()

lista_rasters[["Crocosmia_crocosmiiflora"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Crocosmia_crocosmiiflora_proj_binary_presente_version_final.tif")
lista_rasters[["Delairea_odorata"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Delairea_odorata_proj_binary_presente_version_final.tif")
lista_rasters[["Euonymus_japonicus"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Euonymus_japonicus_proj_binary_presente_version_final.tif")
lista_rasters[["Hedychium_gardnerianum"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Hedychium_gardnerianum_proj_binary_presente_version_final.tif")
lista_rasters[["Ligustrum_ovalifolium"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Ligustrum_ovalifolium_proj_binary_presente_version_final.tif")
lista_rasters[["Phoenix_canariensis"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Phoenix_canariensis_proj_binary_presente_version_final.tif")
lista_rasters[["Potentilla_indica"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Potentilla_indica_proj_binary_presente_version_final.tif")
lista_rasters[["Trachycarpus_fortunei"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Trachycarpus_fortunei_proj_binary_presente_version_final.tif")
lista_rasters[["Tradescantia_fluminensis"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Tradescantia_fluminensis_proj_binary_presente_version_final.tif")

# ==========================================================
# 3️⃣ CARGAR ARCHIVOS DE REGIONES
# ==========================================================

area_presente <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/AREA PRESENTE/AREA_SUBTROPICAL.tif")
ibero_atlantic <- st_read("C:/Users/JorgeCV/Desktop/2024 TFG Jorge Casado/QGIS/IberoAtlantic.shp")

# ==========================================================
# 4️⃣ VERIFICAR QUE COINCIDEN (PERO SIN RESAMPLE)
# ==========================================================

stack_rasters <- stack(lista_rasters)

if (!compareRaster(stack_rasters, area_presente)) {
  print("ADVERTENCIA: Los rasters NO tienen la misma extensión/resolución")
  print("Revisa tus datos antes de continuar")
}

# ==========================================================
# 5️⃣ APLICAR MÁSCARA presente DIRECTAMENTE
# ==========================================================

stack_presente <- mask(stack_rasters, area_presente)

# ==========================================================
# 6️⃣ MAPA DE HOTSPOTS (SUMA DE ESPECIES)
# ==========================================================

mapa_hotspots <- calc(stack_presente, sum, na.rm = TRUE)
mapa_hotspots[mapa_hotspots == 0] <- NA

print(paste("Valores únicos:", paste(sort(unique(values(mapa_hotspots))), collapse=", ")))

# ==========================================================
# 7️⃣ EXPORTAR DATOS
# ==========================================================

writeRaster(mapa_hotspots,
            "C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_invasoras.tif",
            overwrite = TRUE)

df_pixeles <- as.data.frame(stack_presente, xy = TRUE, na.rm = FALSE)
df_pixeles[is.na(df_pixeles)] <- 0
df_pixeles$hotspots_total <- rowSums(df_pixeles[, -(1:2)])

write.xlsx(df_pixeles,
           "C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_presente_invasoras.xlsx",
           overwrite = TRUE)

# ==========================================================
# 8️⃣ MAPA CON GGPLOT2 (ALTO CONTRASTE)
# ==========================================================

mapa_hotspots_df <- as.data.frame(mapa_hotspots, xy = TRUE)
colnames(mapa_hotspots_df)[3] <- "hotspots"
mapa_hotspots_df <- mapa_hotspots_df[!is.na(mapa_hotspots_df$hotspots), ]

mapa_hotspots_df$hotspots <- as.integer(mapa_hotspots_df$hotspots)

valores_unicos <- sort(unique(mapa_hotspots_df$hotspots))

colores_personalizados <- c()

for(valor in valores_unicos){
  
  if(valor <= 6){
    
    if(valor == 1) colores_personalizados <- c(colores_personalizados, "#FFFF99")
    else if(valor == 2) colores_personalizados <- c(colores_personalizados, "#FFDD77")
    else if(valor == 3) colores_personalizados <- c(colores_personalizados, "#FFBB55")
    else if(valor == 4) colores_personalizados <- c(colores_personalizados, "#FF9933")
    else if(valor == 5) colores_personalizados <- c(colores_personalizados, "#FF7711")
    else if(valor == 6) colores_personalizados <- c(colores_personalizados, "#FF5500")
    
  } else {
    
    if(valor == 7) colores_personalizados <- c(colores_personalizados, "#AA00FF")
    else if(valor == 8) colores_personalizados <- c(colores_personalizados, "#7700FF")
    else if(valor == 9) colores_personalizados <- c(colores_personalizados, "#00FF00")

  }
  
}

if(length(colores_personalizados) < length(valores_unicos)) {
  
  colores_faltantes <- length(valores_unicos) - length(colores_personalizados)
  
  colores_extra <- colorRampPalette(c("#AA00FF", "#00FF00"))(colores_faltantes + 2)[1:colores_faltantes]
  
  colores_personalizados <- c(colores_personalizados, colores_extra)
}

mapa <- ggplot() +
  geom_raster(data = mapa_hotspots_df, aes(x = x, y = y, fill = as.factor(hotspots))) +
  scale_fill_manual(values = colores_personalizados,
                    name = "Nº species",
                    na.translate = FALSE) +
  geom_sf(data = ibero_atlantic, fill = NA, color = "black", size = 1.2, linetype = "solid") +
  labs(title = "Present model (Invasive species)",
       x = "Longitude", y = "Latitude") +
  coord_sf() +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.key.height = unit(0.8, "cm"),
    legend.key.width = unit(0.8, "cm"),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "darkred"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray50"),
    plot.margin = margin(10,10,10,60)
  )

print(mapa)

# ==========================================================
# 9️⃣ GUARDAR MAPA
# ==========================================================

ggsave("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_invasoras_ggsave.png",
       mapa, width = 12, height = 9, dpi = 300, bg = "white")

# ==========================================================
# 🔟 VERSIÓN CON PLOT BASE
# ==========================================================

png("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_invasoras.png",
    width = 2600, height = 2000, res = 300)

par(mar = c(5,5,4,12))

colores_base <- colores_personalizados

plot(mapa_hotspots,
     col = colores_base,
     legend = FALSE,
     main = "Present model (Invasive species)",
     xlab = "Longitude", ylab = "Latitude",
     cex.main = 1.5)

plot(st_geometry(ibero_atlantic), add = TRUE, border = "black",
     lwd = 2.5, lty = "solid")

legend("right", inset = c(-0.35, 0),
       legend = valores_unicos,
       fill = colores_base,
       title = "Nº species",
       xpd = TRUE, bty = "n", cex = 1.2,
       title.adj = 0.5,
       y.intersp = 1.3)

dev.off()

# ==========================================================
# 1️⃣1️⃣ IDENTIFICAR HOTSPOTS PRINCIPALES
# ==========================================================

top_hotspots <- df_pixeles %>%
  arrange(desc(hotspots_total)) %>%
  select(x, y, hotspots_total, everything()) %>%
  head(20)

print("Top 20 hotspots (píxeles con más especies invasoras - Presente):")
print(top_hotspots[, 1:5])

print("Distribución de hotspots (Presente - Invasoras):")
print(table(mapa_hotspots_df$hotspots))

distribucion_hotspots <- as.data.frame(table(mapa_hotspots_df$hotspots))
colnames(distribucion_hotspots) <- c("N_especies", "Frecuencia")

distribucion_hotspots$categoria <- ifelse(as.numeric(as.character(distribucion_hotspots$N_especies)) >= 7,
                                          "Alta riqueza (≥7)", "Baja/Media riqueza")

print("Distribución de hotspots con categorías (Presente - Invasoras):")
print(distribucion_hotspots)

# ==========================================================
# 1️⃣2️⃣ ANÁLISIS ADICIONAL
# ==========================================================

total_pixeles <- nrow(mapa_hotspots_df)
pixeles_alta_riqueza <- sum(mapa_hotspots_df$hotspots >= 7)

porcentaje_alta <- round(100 * pixeles_alta_riqueza / total_pixeles, 2)

print(paste("=== ANÁLISIS DE RIQUEZA (PRESENTE - Invasoras) ==="))
print(paste("Total de píxeles con presencia:", total_pixeles))
print(paste("Píxeles con alta riqueza (≥7 especies):", pixeles_alta_riqueza))
print(paste("Porcentaje con alta riqueza:", porcentaje_alta, "%"))

























# ==========================================================
# 1️⃣ LIBRERÍAS
# ==========================================================
rm(list=ls())
library(raster)
library(dplyr)
library(tidyr)
library(openxlsx)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

# ==========================================================
# 2️⃣ CARGAR RASTERS DE ESPECIES (SUBTROPICAL - Invasoras)
# ==========================================================

lista_rasters <- list()

lista_rasters[["Crocosmia_crocosmiiflora"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Crocosmia_crocosmiiflora_proj_binary_subtropical_version_final.tif")
lista_rasters[["Delairea_odorata"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Delairea_odorata_proj_binary_subtropical_version_final.tif")
lista_rasters[["Euonymus_japonicus"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Euonymus_japonicus_proj_binary_subtropical_version_final.tif")
lista_rasters[["Hedychium_gardnerianum"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Hedychium_gardnerianum_proj_binary_subtropical_version_final.tif")
lista_rasters[["Ligustrum_ovalifolium"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Ligustrum_ovalifolium_proj_binary_subtropical_version_final.tif")
lista_rasters[["Phoenix_canariensis"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Phoenix_canariensis_proj_binary_subtropical_version_final.tif")
lista_rasters[["Potentilla_indica"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Potentilla_indica_proj_binary_subtropical_version_final.tif")
lista_rasters[["Trachycarpus_fortunei"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Trachycarpus_fortunei_proj_binary_subtropical_version_final.tif")
lista_rasters[["Tradescantia_fluminensis"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Tradescantia_fluminensis_proj_binary_subtropical_version_final.tif")

# ==========================================================
# 3️⃣ CARGAR ARCHIVOS DE REGIONES
# ==========================================================

area_subtropical <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/AREA FUTURO SUBTROPICAL/AREA_FUTURO_SUBTROPICAL_HELECHOS_9esp.tif")
ibero_atlantic <- st_read("C:/Users/JorgeCV/Desktop/2024 TFG Jorge Casado/QGIS/IberoAtlantic.shp")

# ==========================================================
# 4️⃣ VERIFICAR QUE COINCIDEN (PERO SIN RESAMPLE)
# ==========================================================

stack_rasters <- stack(lista_rasters)

if (!compareRaster(stack_rasters, area_subtropical)) {
  print("ADVERTENCIA: Los rasters NO tienen la misma extensión/resolución")
  print("Revisa tus datos antes de continuar")
}

# ==========================================================
# 5️⃣ APLICAR MÁSCARA subtropical DIRECTAMENTE
# ==========================================================

stack_subtropical <- mask(stack_rasters, area_subtropical)

# ==========================================================
# 6️⃣ MAPA DE HOTSPOTS (SUMA DE ESPECIES)
# ==========================================================

mapa_hotspots <- calc(stack_subtropical, sum, na.rm = TRUE)
mapa_hotspots[mapa_hotspots == 0] <- NA

print(paste("Valores únicos:", paste(sort(unique(values(mapa_hotspots))), collapse=", ")))

# ==========================================================
# 7️⃣ EXPORTAR DATOS
# ==========================================================

writeRaster(mapa_hotspots,
            "C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_subtropical_invasoras.tif",
            overwrite = TRUE)

df_pixeles <- as.data.frame(stack_subtropical, xy = TRUE, na.rm = FALSE)
df_pixeles[is.na(df_pixeles)] <- 0
df_pixeles$hotspots_total <- rowSums(df_pixeles[, -(1:2)])

write.xlsx(df_pixeles,
           "C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_subtropical_invasoras.xlsx",
           overwrite = TRUE)

# ==========================================================
# 8️⃣ MAPA CON GGPLOT2 (ALTO CONTRASTE)
# ==========================================================

mapa_hotspots_df <- as.data.frame(mapa_hotspots, xy = TRUE)
colnames(mapa_hotspots_df)[3] <- "hotspots"
mapa_hotspots_df <- mapa_hotspots_df[!is.na(mapa_hotspots_df$hotspots), ]

mapa_hotspots_df$hotspots <- as.integer(mapa_hotspots_df$hotspots)

valores_unicos <- sort(unique(mapa_hotspots_df$hotspots))

colores_personalizados <- c()

for(valor in valores_unicos){
  
  if(valor <= 6){
    
    if(valor == 1) colores_personalizados <- c(colores_personalizados, "#FFFF99")
    else if(valor == 2) colores_personalizados <- c(colores_personalizados, "#FFDD77")
    else if(valor == 3) colores_personalizados <- c(colores_personalizados, "#FFBB55")
    else if(valor == 4) colores_personalizados <- c(colores_personalizados, "#FF9933")
    else if(valor == 5) colores_personalizados <- c(colores_personalizados, "#FF7711")
    else if(valor == 6) colores_personalizados <- c(colores_personalizados, "#FF5500")
    
  } else {
    
    if(valor == 7) colores_personalizados <- c(colores_personalizados, "#AA00FF")
    else if(valor == 8) colores_personalizados <- c(colores_personalizados, "#7700FF")
    else if(valor == 9) colores_personalizados <- c(colores_personalizados, "#00FF00")

  }
  
}

if(length(colores_personalizados) < length(valores_unicos)) {
  
  colores_faltantes <- length(valores_unicos) - length(colores_personalizados)
  
  colores_extra <- colorRampPalette(c("#AA00FF", "#00FF00"))(colores_faltantes + 2)[1:colores_faltantes]
  
  colores_personalizados <- c(colores_personalizados, colores_extra)
}

mapa <- ggplot() +
  geom_raster(data = mapa_hotspots_df, aes(x = x, y = y, fill = as.factor(hotspots))) +
  scale_fill_manual(values = colores_personalizados,
                    name = "Nº species",
                    na.translate = FALSE) +
  geom_sf(data = ibero_atlantic, fill = NA, color = "black", size = 1.2, linetype = "solid") +
  labs(title = "Subtropical model (Invasive species)",
       x = "Longitude", y = "Latitude") +
  coord_sf() +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.key.height = unit(0.8, "cm"),
    legend.key.width = unit(0.8, "cm"),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "darkred"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray50"),
    plot.margin = margin(10,10,10,60)
  )

print(mapa)

# ==========================================================
# 9️⃣ GUARDAR MAPA
# ==========================================================

ggsave("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_subtropical_invasoras_ggsave.png",
       mapa, width = 12, height = 9, dpi = 300, bg = "white")

# ==========================================================
# 🔟 VERSIÓN CON PLOT BASE
# ==========================================================

png("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_subtropical_invasoras.png",
    width = 2600, height = 2000, res = 300)

par(mar = c(5,5,4,12))

colores_base <- colores_personalizados

plot(mapa_hotspots,
     col = colores_base,
     legend = FALSE,
     main = "Subtropical model (Invasive species)",
     xlab = "Longitude", ylab = "Latitude",
     cex.main = 1.5)

plot(st_geometry(ibero_atlantic), add = TRUE, border = "black",
     lwd = 2.5, lty = "solid")

legend("right", inset = c(-0.35, 0),
       legend = valores_unicos,
       fill = colores_base,
       title = "Nº species",
       xpd = TRUE, bty = "n", cex = 1.2,
       title.adj = 0.5,
       y.intersp = 1.3)

dev.off()

# ==========================================================
# 1️⃣1️⃣ IDENTIFICAR HOTSPOTS PRINCIPALES
# ==========================================================

top_hotspots <- df_pixeles %>%
  arrange(desc(hotspots_total)) %>%
  select(x, y, hotspots_total, everything()) %>%
  head(20)

print("Top 20 hotspots (píxeles con más especies invasoras - Subtropical):")
print(top_hotspots[, 1:5])

print("Distribución de hotspots (Subtropical - Invasoras):")
print(table(mapa_hotspots_df$hotspots))

distribucion_hotspots <- as.data.frame(table(mapa_hotspots_df$hotspots))
colnames(distribucion_hotspots) <- c("N_especies", "Frecuencia")

distribucion_hotspots$categoria <- ifelse(as.numeric(as.character(distribucion_hotspots$N_especies)) >= 7,
                                          "Alta riqueza (≥7)", "Baja/Media riqueza")

print("Distribución de hotspots con categorías (Subtropical - Invasoras):")
print(distribucion_hotspots)

# ==========================================================
# 1️⃣2️⃣ ANÁLISIS ADICIONAL
# ==========================================================

total_pixeles <- nrow(mapa_hotspots_df)
pixeles_alta_riqueza <- sum(mapa_hotspots_df$hotspots >= 7)

porcentaje_alta <- round(100 * pixeles_alta_riqueza / total_pixeles, 2)

print(paste("=== ANÁLISIS DE RIQUEZA (SUBTROPICAL - Invasoras) ==="))
print(paste("Total de píxeles con presencia:", total_pixeles))
print(paste("Píxeles con alta riqueza (≥7 especies):", pixeles_alta_riqueza))
print(paste("Porcentaje con alta riqueza:", porcentaje_alta, "%"))


























# ==========================================================
# 1️⃣ LIBRERÍAS
# ==========================================================
rm(list=ls())
library(raster)
library(dplyr)
library(tidyr)
library(openxlsx)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

# ==========================================================
# 2️⃣ CARGAR RASTERS DE ESPECIES (MEDITERRÁNEA - Invasoras)
# ==========================================================

lista_rasters <- list()

lista_rasters[["Crocosmia_crocosmiiflora"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Crocosmia_crocosmiiflora_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Delairea_odorata"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Delairea_odorata_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Euonymus_japonicus"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Euonymus_japonicus_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Hedychium_gardnerianum"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Hedychium_gardnerianum_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Ligustrum_ovalifolium"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Ligustrum_ovalifolium_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Phoenix_canariensis"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Phoenix_canariensis_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Potentilla_indica"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Potentilla_indica_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Trachycarpus_fortunei"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Trachycarpus_fortunei_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Tradescantia_fluminensis"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Tradescantia_fluminensis_proj_binary_mediterranea_version_final.tif")

# ==========================================================
# 3️⃣ CARGAR ARCHIVOS DE REGIONES
# ==========================================================

area_mediterranea <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/AREA MEDITERRANEA/AREA_MEDITERRANEA_HELECHOS_9esp.tif")
ibero_atlantic <- st_read("C:/Users/JorgeCV/Desktop/2024 TFG Jorge Casado/QGIS/IberoAtlantic.shp")

# ==========================================================
# 4️⃣ VERIFICAR QUE COINCIDEN (PERO SIN RESAMPLE)
# ==========================================================

stack_rasters <- stack(lista_rasters)

if (!compareRaster(stack_rasters, area_mediterranea)) {
  print("ADVERTENCIA: Los rasters NO tienen la misma extensión/resolución")
  print("Revisa tus datos antes de continuar")
}

# ==========================================================
# 5️⃣ APLICAR MÁSCARA mediterranea DIRECTAMENTE
# ==========================================================

stack_mediterranea <- mask(stack_rasters, area_mediterranea)

# ==========================================================
# 6️⃣ MAPA DE HOTSPOTS (SUMA DE ESPECIES)
# ==========================================================

mapa_hotspots <- calc(stack_mediterranea, sum, na.rm = TRUE)
mapa_hotspots[mapa_hotspots == 0] <- NA

print(paste("Valores únicos:", paste(sort(unique(values(mapa_hotspots))), collapse=", ")))

# ==========================================================
# 7️⃣ EXPORTAR DATOS
# ==========================================================

writeRaster(mapa_hotspots,
            "C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_mediterranea_invasoras.tif",
            overwrite = TRUE)

df_pixeles <- as.data.frame(stack_mediterranea, xy = TRUE, na.rm = FALSE)
df_pixeles[is.na(df_pixeles)] <- 0
df_pixeles$hotspots_total <- rowSums(df_pixeles[, -(1:2)])

write.xlsx(df_pixeles,
           "C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_mediterranea_invasoras.xlsx",
           overwrite = TRUE)

# ==========================================================
# 8️⃣ MAPA CON GGPLOT2 (ALTO CONTRASTE)
# ==========================================================

mapa_hotspots_df <- as.data.frame(mapa_hotspots, xy = TRUE)
colnames(mapa_hotspots_df)[3] <- "hotspots"
mapa_hotspots_df <- mapa_hotspots_df[!is.na(mapa_hotspots_df$hotspots), ]

mapa_hotspots_df$hotspots <- as.integer(mapa_hotspots_df$hotspots)

valores_unicos <- sort(unique(mapa_hotspots_df$hotspots))

colores_personalizados <- c()

for(valor in valores_unicos){
  
  if(valor <= 6){
    
    if(valor == 1) colores_personalizados <- c(colores_personalizados, "#FFFF99")
    else if(valor == 2) colores_personalizados <- c(colores_personalizados, "#FFDD77")
    else if(valor == 3) colores_personalizados <- c(colores_personalizados, "#FFBB55")
    else if(valor == 4) colores_personalizados <- c(colores_personalizados, "#FF9933")
    else if(valor == 5) colores_personalizados <- c(colores_personalizados, "#FF7711")
    else if(valor == 6) colores_personalizados <- c(colores_personalizados, "#FF5500")
    
  } else {
    
    if(valor == 7) colores_personalizados <- c(colores_personalizados, "#AA00FF")
    else if(valor == 8) colores_personalizados <- c(colores_personalizados, "#7700FF")
    else if(valor == 9) colores_personalizados <- c(colores_personalizados, "#00FF00")

  }
  
}

if(length(colores_personalizados) < length(valores_unicos)) {
  
  colores_faltantes <- length(valores_unicos) - length(colores_personalizados)
  
  colores_extra <- colorRampPalette(c("#AA00FF", "#00FF00"))(colores_faltantes + 2)[1:colores_faltantes]
  
  colores_personalizados <- c(colores_personalizados, colores_extra)
}

mapa <- ggplot() +
  geom_raster(data = mapa_hotspots_df, aes(x = x, y = y, fill = as.factor(hotspots))) +
  scale_fill_manual(values = colores_personalizados,
                    name = "Nº species",
                    na.translate = FALSE) +
  geom_sf(data = ibero_atlantic, fill = NA, color = "black", size = 1.2, linetype = "solid") +
  labs(title = "Mediterranean model (Invasive species)",
       x = "Longitude", y = "Latitude") +
  coord_sf() +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.key.height = unit(0.8, "cm"),
    legend.key.width = unit(0.8, "cm"),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "darkred"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray50"),
    plot.margin = margin(10,10,10,60)
  )

print(mapa)

# ==========================================================
# 9️⃣ GUARDAR MAPA
# ==========================================================

ggsave("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_mediterranea_invasoras_ggsave.png",
       mapa, width = 12, height = 9, dpi = 300, bg = "white")

# ==========================================================
# 🔟 VERSIÓN CON PLOT BASE
# ==========================================================

png("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_mediterranea_invasoras.png",
    width = 2600, height = 2000, res = 300)

par(mar = c(5,5,4,12))

colores_base <- colores_personalizados

plot(mapa_hotspots,
     col = colores_base,
     legend = FALSE,
     main = "Mediterranean model (Invasive species)",
     xlab = "Longitude", ylab = "Latitude",
     cex.main = 1.5)

plot(st_geometry(ibero_atlantic), add = TRUE, border = "black",
     lwd = 2.5, lty = "solid")

legend("right", inset = c(-0.35, 0),
       legend = valores_unicos,
       fill = colores_base,
       title = "Nº species",
       xpd = TRUE, bty = "n", cex = 1.2,
       title.adj = 0.5,
       y.intersp = 1.3)

dev.off()

# ==========================================================
# 1️⃣1️⃣ IDENTIFICAR HOTSPOTS PRINCIPALES (continuación)
# ==========================================================

top_hotspots <- df_pixeles %>%
  arrange(desc(hotspots_total)) %>%
  select(x, y, hotspots_total, everything()) %>%
  head(20)

print("Top 20 hotspots (píxeles con más especies invasoras - Mediterránea):")
print(top_hotspots[, 1:5])

print("Distribución de hotspots (Mediterránea - Invasoras):")
print(table(mapa_hotspots_df$hotspots))

distribucion_hotspots <- as.data.frame(table(mapa_hotspots_df$hotspots))
colnames(distribucion_hotspots) <- c("N_especies", "Frecuencia")

distribucion_hotspots$categoria <- ifelse(as.numeric(as.character(distribucion_hotspots$N_especies)) >= 7,
                                          "Alta riqueza (≥7)", "Baja/Media riqueza")

print("Distribución de hotspots con categorías (Mediterránea - Invasoras):")
print(distribucion_hotspots)

# ==========================================================
# 1️⃣2️⃣ ANÁLISIS ADICIONAL
# ==========================================================

total_pixeles <- nrow(mapa_hotspots_df)
pixeles_alta_riqueza <- sum(mapa_hotspots_df$hotspots >= 7)

porcentaje_alta <- round(100 * pixeles_alta_riqueza / total_pixeles, 2)

print(paste("=== ANÁLISIS DE RIQUEZA (MEDITERRÁNEA - Invasoras) ==="))
print(paste("Total de píxeles con presencia:", total_pixeles))
print(paste("Píxeles con alta riqueza (≥7 especies):", pixeles_alta_riqueza))
print(paste("Porcentaje con alta riqueza:", porcentaje_alta, "%"))