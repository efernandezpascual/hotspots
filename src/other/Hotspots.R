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
# 2️⃣ CARGAR RASTERS DE ESPECIES
# ==========================================================

lista_rasters <- list()

lista_rasters[["Culcita_macrocarpa"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Culcita_macrocarpa_proj_binary_subtropical_version_final.tif")
lista_rasters[["Davallia_canariensis"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Davallia_canariensis_proj_binary_subtropical_version_final.tif")
lista_rasters[["Dryopteris_aemula"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Dryopteris_aemula_proj_binary_subtropical_version_final.tif")
lista_rasters[["Dryopteris_corleyi"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Dryopteris_corleyi_proj_binary_subtropical_version_final.tif")
lista_rasters[["Dryopteris_guanchica"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Dryopteris_guanchica_proj_binary_subtropical_version_final.tif")
lista_rasters[["Hymenophyllum_tunbrigense"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Hymenophyllum_tunbrigense_proj_binary_subtropical_version_final.tif")
lista_rasters[["Leptogramma_pozoi"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Leptogramma_pozoi_proj_binary_subtropical_version_final.tif")
lista_rasters[["Vandenboschia_speciosa"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Vandenboschia_speciosa_proj_binary_subtropical_version_final.tif")
lista_rasters[["Woodwardia_radicans"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final/Woodwardia_radicans_proj_binary_subtropical_version_final.tif")

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
            "C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_subtropical_helechos.tif",
            overwrite = TRUE)

df_pixeles <- as.data.frame(stack_subtropical, xy = TRUE, na.rm = FALSE)
df_pixeles[is.na(df_pixeles)] <- 0
df_pixeles$hotspots_total <- rowSums(df_pixeles[, -(1:2)])

write.xlsx(df_pixeles,
           "C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_subtropical_helechos.xlsx",
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
  labs(title = "Subtropical model (Subtropical ferns)",
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

ggsave("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_subtropical_helechos_contraste_ggsave.png",
       mapa, width = 12, height = 9, dpi = 300, bg = "white")

# ==========================================================
# 🔟 VERSIÓN CON PLOT BASE
# ==========================================================

png("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_subtropical_helechos_contraste.png",
    width = 2600, height = 2000, res = 300)

par(mar = c(5,5,4,12))

colores_base <- colores_personalizados

plot(mapa_hotspots,
     col = colores_base,
     legend = FALSE,
     main = "Subtropical model (Subtropical ferns)",
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

print("Top 20 hotspots (píxeles con más especies):")
print(top_hotspots[, 1:5])

print("Distribución de hotspots:")
print(table(mapa_hotspots_df$hotspots))

distribucion_hotspots <- as.data.frame(table(mapa_hotspots_df$hotspots))
colnames(distribucion_hotspots) <- c("N_especies", "Frecuencia")

distribucion_hotspots$categoria <- ifelse(as.numeric(as.character(distribucion_hotspots$N_especies)) >= 7,
                                          "Alta riqueza (≥7)", "Baja/Media riqueza")

print("Distribución de hotspots con categorías:")
print(distribucion_hotspots)

# ==========================================================
# 1️⃣2️⃣ ANÁLISIS ADICIONAL
# ==========================================================

total_pixeles <- nrow(mapa_hotspots_df)
pixeles_alta_riqueza <- sum(mapa_hotspots_df$hotspots >= 7)

porcentaje_alta <- round(100 * pixeles_alta_riqueza / total_pixeles, 2)

print(paste("=== ANÁLISIS DE RIQUEZA ==="))
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
# 2️⃣ CARGAR RASTERS DE ESPECIES
# ==========================================================

lista_rasters <- list()

lista_rasters[["Culcita_macrocarpa"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Culcita_macrocarpa_proj_binary_presente_version_final.tif")
lista_rasters[["Davallia_canariensis"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Davallia_canariensis_proj_binary_presente_version_final.tif")
lista_rasters[["Dryopteris_aemula"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Dryopteris_aemula_proj_binary_presente_version_final.tif")
lista_rasters[["Dryopteris_corleyi"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Dryopteris_corleyi_proj_binary_presente_version_final.tif")
lista_rasters[["Dryopteris_guanchica"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Dryopteris_guanchica_proj_binary_presente_version_final.tif")
lista_rasters[["Hymenophyllum_tunbrigense"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Hymenophyllum_tunbrigense_proj_binary_presente_version_final.tif")
lista_rasters[["Leptogramma_pozoi"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Leptogramma_pozoi_proj_binary_presente_version_final.tif")
lista_rasters[["Vandenboschia_speciosa"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Vandenboschia_speciosa_proj_binary_presente_version_final.tif")
lista_rasters[["Woodwardia_radicans"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final/Woodwardia_radicans_proj_binary_presente_version_final.tif")

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
            "C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_helechos.tif",
            overwrite = TRUE)

df_pixeles <- as.data.frame(stack_presente, xy = TRUE, na.rm = FALSE)
df_pixeles[is.na(df_pixeles)] <- 0
df_pixeles$hotspots_total <- rowSums(df_pixeles[, -(1:2)])

write.xlsx(df_pixeles,
           "C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_presente_helechos.xlsx",
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
  labs(title = "Present model (Subtropical ferns)",
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

ggsave("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_helechos_contraste_ggsave.png",
       mapa, width = 12, height = 9, dpi = 300, bg = "white")

# ==========================================================
# 🔟 VERSIÓN CON PLOT BASE
# ==========================================================

png("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_helechos_contraste.png",
    width = 2600, height = 2000, res = 300)

par(mar = c(5,5,4,12))

colores_base <- colores_personalizados

plot(mapa_hotspots,
     col = colores_base,
     legend = FALSE,
     main = "Present model (Subtropical ferns)",
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

print("Top 20 hotspots (píxeles con más especies):")
print(top_hotspots[, 1:5])

print("Distribución de hotspots:")
print(table(mapa_hotspots_df$hotspots))

distribucion_hotspots <- as.data.frame(table(mapa_hotspots_df$hotspots))
colnames(distribucion_hotspots) <- c("N_especies", "Frecuencia")

distribucion_hotspots$categoria <- ifelse(as.numeric(as.character(distribucion_hotspots$N_especies)) >= 7,
                                          "Alta riqueza (≥7)", "Baja/Media riqueza")

print("Distribución de hotspots con categorías:")
print(distribucion_hotspots)

# ==========================================================
# 1️⃣2️⃣ ANÁLISIS ADICIONAL
# ==========================================================

total_pixeles <- nrow(mapa_hotspots_df)
pixeles_alta_riqueza <- sum(mapa_hotspots_df$hotspots >= 7)

porcentaje_alta <- round(100 * pixeles_alta_riqueza / total_pixeles, 2)

print(paste("=== ANÁLISIS DE RIQUEZA ==="))
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
# 2️⃣ CARGAR RASTERS DE ESPECIES
# ==========================================================

lista_rasters <- list()

lista_rasters[["Culcita_macrocarpa"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Culcita_macrocarpa_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Davallia_canariensis"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Davallia_canariensis_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Dryopteris_aemula"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Dryopteris_aemula_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Dryopteris_corleyi"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Dryopteris_corleyi_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Dryopteris_guanchica"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Dryopteris_guanchica_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Hymenophyllum_tunbrigense"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Hymenophyllum_tunbrigense_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Leptogramma_pozoi"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Leptogramma_pozoi_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Vandenboschia_speciosa"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Vandenboschia_speciosa_proj_binary_mediterranea_version_final.tif")
lista_rasters[["Woodwardia_radicans"]] <- raster("C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final/Woodwardia_radicans_proj_binary_mediterranea_version_final.tif")

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
            "C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_mediterranea_helechos.tif",
            overwrite = TRUE)

df_pixeles <- as.data.frame(stack_mediterranea, xy = TRUE, na.rm = FALSE)
df_pixeles[is.na(df_pixeles)] <- 0
df_pixeles$hotspots_total <- rowSums(df_pixeles[, -(1:2)])

write.xlsx(df_pixeles,
           "C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_mediterranea_helechos.xlsx",
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
  labs(title = "Mediterranean model (Subtropical ferns)",
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

ggsave("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_mediterranea_helechos_contraste_ggsave.png",
       mapa, width = 12, height = 9, dpi = 300, bg = "white")

# ==========================================================
# 🔟 VERSIÓN CON PLOT BASE
# ==========================================================

png("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_mediterranea_helechos_contraste.png",
    width = 2600, height = 2000, res = 300)

par(mar = c(5,5,4,12))

colores_base <- colores_personalizados

plot(mapa_hotspots,
     col = colores_base,
     legend = FALSE,
     main = "Mediterranean model (Subtropical ferns)",
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

print("Top 20 hotspots (píxeles con más especies):")
print(top_hotspots[, 1:5])

print("Distribución de hotspots:")
print(table(mapa_hotspots_df$hotspots))

distribucion_hotspots <- as.data.frame(table(mapa_hotspots_df$hotspots))
colnames(distribucion_hotspots) <- c("N_especies", "Frecuencia")

distribucion_hotspots$categoria <- ifelse(as.numeric(as.character(distribucion_hotspots$N_especies)) >= 7,
                                          "Alta riqueza (≥7)", "Baja/Media riqueza")

print("Distribución de hotspots con categorías:")
print(distribucion_hotspots)

# ==========================================================
# 1️⃣2️⃣ ANÁLISIS ADICIONAL
# ==========================================================

total_pixeles <- nrow(mapa_hotspots_df)
pixeles_alta_riqueza <- sum(mapa_hotspots_df$hotspots >= 7)

porcentaje_alta <- round(100 * pixeles_alta_riqueza / total_pixeles, 2)

print(paste("=== ANÁLISIS DE RIQUEZA ==="))
print(paste("Total de píxeles con presencia:", total_pixeles))
print(paste("Píxeles con alta riqueza (≥7 especies):", pixeles_alta_riqueza))
print(paste("Porcentaje con alta riqueza:", porcentaje_alta, "%"))

