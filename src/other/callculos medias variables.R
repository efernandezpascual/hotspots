# =====================================================
# MEDIAS DE PREDICTORES EN 3 ÁREAS (HELECHOS + INVASORAS)
# =====================================================

rm(list = ls())

library(raster)
library(sf)
library(openxlsx)

cat("\n=== CARGANDO DATOS ===\n")

# -------------------------------
# 1. RUTAS
# -------------------------------

RUTAS <- list(
  bio6 = "C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio6_1981-2010_V.2.1_WGS_1km.tif",
  bio7 = "C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio7_1981-2010_V.2.1_WGS_1km.tif",
  bio14 = "C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio14_1981-2010_V.2.1_WGS_1km.tif",
  sand = "C:/Users/JorgeCV/Desktop/TFM/Variables/sand_study_area_WGS_final_1km.tif",
  ph = "C:/Users/JorgeCV/Desktop/TFM/Variables/PH_study_area_WGS_final_1km.tif",
  helechos = "C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_helechos.tif",
  invasoras = "C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_invasoras.tif",
  ibero_shp = "C:/Users/JorgeCV/Desktop/2024 TFG Jorge Casado/QGIS/IberoAtlantic.shp"
)

# -------------------------------
# 2. FUNCIÓN CARGA
# -------------------------------

cargar_raster <- function(r) {
  if (!file.exists(r)) stop(paste("No existe:", r))
  raster(r)
}

# -------------------------------
# 3. PREDICTORES
# -------------------------------

bio6 <- cargar_raster(RUTAS$bio6) * 0.1 - 273
bio7 <- cargar_raster(RUTAS$bio7) * 0.1
bio14 <- cargar_raster(RUTAS$bio14) * 0.1
sand <- cargar_raster(RUTAS$sand)
ph <- cargar_raster(RUTAS$ph)

predictors <- stack(bio6, bio7, bio14, sand, ph)
names(predictors) <- c("bio6","bio7","bio14","sand","ph")

# -------------------------------
# 4. SHAPE IBEROATLÁNTICO
# -------------------------------

ibero <- st_read(RUTAS$ibero_shp)
ibero <- st_transform(ibero, crs(predictors))

mask_ibero <- rasterize(ibero, predictors[[1]], field = 1)

# -------------------------------
# 5. FUNCIONES
# -------------------------------

extraer_medias <- function(pred, riqueza, umbral) {
  
  # ajustar resolución
  riqueza <- resample(riqueza, pred[[1]], method = "ngb")
  
  # áreas
  area_ibero <- !is.na(mask_ibero)
  area_sub <- riqueza >= 1
  area_hot <- riqueza >= umbral
  
  calc_media <- function(area) {
    vals <- as.data.frame(values(mask(pred, area)))
    colMeans(vals, na.rm = TRUE)
  }
  
  list(
    ibero = calc_media(area_ibero),
    subtropical = calc_media(area_sub),
    hotspots = calc_media(area_hot)
  )
}

# -------------------------------
# 6. RIQUEZAS
# -------------------------------

helechos <- cargar_raster(RUTAS$helechos)
invasoras <- cargar_raster(RUTAS$invasoras)

# -------------------------------
# 7. CÁLCULOS
# -------------------------------

cat("\n=== HELECHOS ===\n")
res_helechos <- extraer_medias(predictors, helechos, 7)

cat("\n=== INVASORAS ===\n")
res_invasoras <- extraer_medias(predictors, invasoras, 7)

# -------------------------------
# 8. TABLAS
# -------------------------------

tabla_helechos <- data.frame(
  Variable = names(predictors),
  Iberoatlantica = res_helechos$ibero,
  Subtropical = res_helechos$subtropical,
  Hotspots = res_helechos$hotspots
)

tabla_invasoras <- data.frame(
  Variable = names(predictors),
  Iberoatlantica = res_invasoras$ibero,
  Subtropical = res_invasoras$subtropical,
  Hotspots = res_invasoras$hotspots
)

# -------------------------------
# 9. EXPORTAR EXCEL
# -------------------------------

write.xlsx(
  list(
    Helechos = tabla_helechos,
    Invasoras = tabla_invasoras
  ),
  "C:/Users/JorgeCV/Desktop/TFM/Resultados/medias_predictores_3_areas.xlsx",
  overwrite = TRUE
)

cat("\n✅ EXPORTACIÓN COMPLETA\n")