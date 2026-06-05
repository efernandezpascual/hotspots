# =====================================================
# MODELADO DE DISTRIBUCIÓN - MÚLTIPLES ESPECIES (PRESENTE)
# CON IMPORTANCIA DE VARIABLES NORMALIZADA
# Y CURVAS DE RESPUESTA GUARDADAS EN PDF
# =====================================================

# Limpiar workspace
rm(list = ls())

# -------------------------------
# 1. PAQUETES
# -------------------------------
library(raster)
library(biomod2)
library(dplyr)
library(terra)
library(tidyr)
library(purrr)

# -------------------------------
# 2. CONFIGURACIÓN DE RUTAS
# -------------------------------
RUTAS <- list(
  bio6 = "C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio6_1981-2010_V.2.1_WGS_1km.tif",
  bio7 = "C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio7_1981-2010_V.2.1_WGS_1km.tif",
  bio14 = "C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio14_1981-2010_V.2.1_WGS_1km.tif",
  sand = "C:/Users/JorgeCV/Desktop/TFM/Variables/sand_study_area_WGS_final_1km.tif",
  ph = "C:/Users/JorgeCV/Desktop/TFM/Variables/PH_study_area_WGS_final_1km.tif",
  ocurrencias = "C:/Users/JorgeCV/Desktop/TFM/Observaciones/ptos-definitivo.csv",
  
  modelos = "C:/Users/JorgeCV/Desktop/TFM/Modelos/modelos presente version final",
  evaluacion = "C:/Users/JorgeCV/Desktop/TFM/Evaluacion modelos/evaluacion presente version final",
  importancia = "C:/Users/JorgeCV/Desktop/TFM/importancia relativa variables/importancia presente version final",
  proyeccion = "C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion presente version final",
  curva_respuesta = "C:/Users/JorgeCV/Desktop/TFM/curva respuesta/curva presente version final"
)


# Crear directorios
for (dir in unique(unlist(RUTAS[6:10]))) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

# -------------------------------
# 3. PREPARAR VARIABLES AMBIENTALES
# -------------------------------

cat("\n=== PREPARANDO VARIABLES AMBIENTALES ===\n")

# cargar raster
cargar_raster <- function(r) {
  if (!file.exists(r)) stop(paste("Archivo no encontrado:", r))
  raster(r)
}

# Cargar rasters
bio6 <- cargar_raster(RUTAS$bio6) * 0.1 - 273
bio7 <- cargar_raster(RUTAS$bio7) * 0.1
bio14 <- cargar_raster(RUTAS$bio14) * 0.1  # Convertir a mm
sand <- cargar_raster(RUTAS$sand)
ph <- cargar_raster(RUTAS$ph)

cat("bio6 (°C):", cellStats(bio6, range, na.rm=TRUE), "\n")
cat("bio7 (°C):", cellStats(bio7, range, na.rm=TRUE), "\n")
cat("bio14 (mm):", cellStats(bio14, range, na.rm=TRUE), "\n")
# Homogeneizar extensiones
extension_comun <- intersect(extent(sand), extent(ph))
plantilla <- raster(extension_comun, res = res(sand), crs = crs(sand))

# Resamplear todos a la misma plantilla
ph <- resample(ph, plantilla, method = "bilinear")
bio6 <- resample(bio6, plantilla, method = "bilinear")
bio7 <- resample(bio7, plantilla, method = "bilinear")
bio14 <- resample(bio14, plantilla, method = "bilinear")

# Crear máscara común
mascara <- !is.na(sand)
mascara[mascara] <- 1

# Aplicar máscara
ph <- mask(ph, mascara)
bio6 <- mask(bio6, mascara)
bio7 <- mask(bio7, mascara)
bio14 <- mask(bio14, mascara)
sand <- mask(sand, mascara)

# Stack final
predictors <- stack(bio6, bio7, bio14, sand, ph)
names(predictors) <- c("bio6", "bio7", "bio14", "sand", "ph")

cat("✅ Variables preparadas\n")

# -------------------------------
# 4. CARGAR Y FILTRAR OBSERVACIONES (TODAS LAS ESPECIES JUNTAS)
# -------------------------------

cat("\n=== CARGANDO Y FILTRANDO OBSERVACIONES ===\n")

obs <- read.csv(RUTAS$ocurrencias) %>%
  dplyr::select(species, longitude, latitude) %>%
  dplyr::rename(Name = species) %>%
  dplyr::mutate(Response = 1)

cat("Observaciones totales:", nrow(obs), "\n")
cat("Especies únicas:", length(unique(obs$Name)), "\n")

# ASIGNAR ID DE PÍXEL A CADA OBSERVACIÓN (TODAS JUNTAS)
obs$cell_id <- cellFromXY(predictors[[1]], obs[, c("longitude", "latitude")])
obs <- obs %>% filter(!is.na(cell_id))  # Eliminar observaciones fuera del raster

cat("Observaciones dentro del área de estudio:", nrow(obs), "\n")

# FILTRAR 1 OBSERVACIÓN POR ESPECIE Y PÍXEL (TODAS JUNTAS)
set.seed(123)  # Para reproducibilidad
obs_filtradas <- obs %>%
  group_by(Name, cell_id) %>%        # Agrupar por especie Y píxel
  sample_n(size = 1) %>%              # Una observación aleatoria por grupo
  ungroup()

cat("Observaciones después de filtrar (1 por especie/píxel):", nrow(obs_filtradas), "\n")

# Verificar cuántas observaciones quedan por especie
resumen_especies <- obs_filtradas %>%
  group_by(Name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

cat("\nResumen por especie después de filtrar:\n")
print(resumen_especies)

# Guardar observaciones filtradas
write.csv(obs_filtradas, file.path(RUTAS$modelos, "observaciones_filtradas_1_por_pixel.csv"), row.names = FALSE)

# -------------------------------
# 5. FUNCIÓN PARA PROCESAR UNA ESPECIE
# -------------------------------

procesar_especie <- function(especie, datos_especie, predictors, rutas) {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("PROCESANDO:", especie, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Verificar número mínimo de observaciones
  if (nrow(datos_especie) < 5) {
    cat("⚠️  Especie con menos de 5 observaciones (", nrow(datos_especie), "). Saltando...\n")
    return(list(
      especie = especie,
      estado = "Saltada (n < 5)",
      n_obs = nrow(datos_especie)
    ))
  }
  
  cat("Observaciones para modelado:", nrow(datos_especie), "\n")
  
  # Nombre para archivos
  nombre_archivo <- gsub(" ", "_", especie)
  
  # 1. MODELOS DE presente version final (RF + GBM)
  cat("\n--- MODELOS DE presente version final ---\n")
  
  mod_test <- BIOMOD_FormatingData(
    resp.var = datos_especie$Response,
    expl.var = predictors,
    resp.xy = datos_especie[, c("longitude", "latitude")],
    resp.name = nombre_archivo,
    PA.strategy = "random",
    PA.nb.rep = 5,
    PA.nb.absences = nrow(datos_especie)
  )
  
  result_test <- BIOMOD_Modeling(
    bm.format = mod_test,
    models = c('RF', 'GBM'),
    CV.nb.rep = 5,
    CV.perc = 0.8,
    var.import = 3
  )
  
  # Guardar modelo de presente version final
  save(result_test, file = file.path(rutas$modelos, 
                                     paste0(nombre_archivo, "_testing_presente version final.RData")))
  
  # Evaluaciones de presente version final
  eval_test <- as.data.frame(get_evaluations(result_test))
  write.csv(eval_test, file.path(rutas$evaluacion, 
                                 paste0(nombre_archivo, "_testing_eval.csv")), row.names = FALSE)
  
  # 2. MODELO FINAL (SOLO RF)
  cat("\n--- MODELO FINAL ---\n")
  
  mod_final <- BIOMOD_FormatingData(
    resp.var = datos_especie$Response,
    expl.var = predictors,
    resp.xy = datos_especie[, c("longitude", "latitude")],
    resp.name = nombre_archivo,
    PA.strategy = "random",
    PA.nb.rep = 1,
    PA.nb.absences = nrow(datos_especie)
  )
  
  result_final <- BIOMOD_Modeling(
    bm.format = mod_final,
    models = c('RF'),
    CV.nb.rep = 1,
    CV.perc = 0.8,
    var.import = 3
  )
  
  # Guardar modelo final
  save(result_final, file = file.path(rutas$modelos, 
                                      paste0(nombre_archivo, "_presente version final.RData")))
  
  # Evaluaciones finales
  eval_final <- as.data.frame(get_evaluations(result_final))
  write.csv(eval_final, file.path(rutas$evaluacion, 
                                  paste0(nombre_archivo, "_final_eval.csv")), row.names = FALSE)
  
  # ============================================================
  # 3. IMPORTANCIA DE VARIABLES CON NORMALIZACIÓN (0-1)
  # ============================================================
  
  # Extraer importancia cruda
  var_imp <- get_variables_importance(result_final)
  
  # Calcular media por variable
  media_var_imp <- tapply(var_imp$var.imp, var_imp$expl.var, mean)
  
  # NORMALIZAR: dividir por la suma total para que sumen 1 (0-1)
  media_var_imp_norm <- media_var_imp / sum(media_var_imp)
  
  # Mostrar en consola
  cat("\n=== IMPORTANCIA DE VARIABLES (NORMALIZADA 0-1) ===\n")
  print(round(media_var_imp_norm, 3))
  
  # Crear dataframe con valores crudos y normalizados
  var_imp_df <- data.frame(
    variable = names(media_var_imp),
    importancia_cruda = as.numeric(media_var_imp),
    importancia_normalizada = as.numeric(media_var_imp_norm),
    porcentaje = as.numeric(media_var_imp_norm * 100)  # Porcentaje (0-100%)
  ) %>%
    arrange(desc(importancia_normalizada))
  
  # Guardar CSV
  write.csv(var_imp_df, file.path(rutas$importancia, 
                                  paste0(nombre_archivo, "_var_imp.csv")), row.names = FALSE)
  
  # ============================================================
  # 4. CURVAS DE RESPUESTA (OPCIÓN 1: PDF)
  # ============================================================
  cat("\n--- CURVAS DE RESPUESTA ---\n")
  
  tryCatch({
    # Cargar modelos Random Forest
    modelos_rf <- BIOMOD_LoadModels(result_final)
    modelos_rf <- grep("allData_allRun", modelos_rf, value = TRUE)
    
    if (length(modelos_rf) > 0) {
      # Abrir dispositivo PDF
      # DESPUÉS (guarda en curva_respuesta)
      pdf(file.path(rutas$curva_respuesta, paste0(nombre_archivo, "_response_curves.pdf")), 
          width = 12, height = 10)
      
      # Generar curvas
      curvas <- bm_PlotResponseCurves(
        bm.out = result_final,
        models.chosen = modelos_rf,
        new.env = get_formal_data(result_final, 'expl.var'),
        show.variables = get_formal_data(result_final, 'expl.var.names'),
        do.bivariate = FALSE,
        fixed.var.metric = 'median',
        col = c("blue", "red"),
        legend = TRUE
      )
      
      # Cerrar PDF
      dev.off()
      
      cat("✅ Curvas de respuesta guardadas en PDF:", 
          paste0(nombre_archivo, "_response_curves.pdf"), "\n")
      
    } else {
      cat("⚠️ No se encontraron modelos para curvas de respuesta\n")
    }
  }, error = function(e) {
    cat("⚠️ Error al generar curvas de respuesta:", e$message, "\n")
  })
  
  # ============================================================
  # 5. PROYECCIÓN
  # ============================================================
  cat("\n--- GENERANDO PROYECCIÓN ---\n")
  
  proyeccion <- BIOMOD_Projection(
    bm.mod = result_final,
    new.env = predictors,
    proj.name = 'map',
    compress = FALSE,
    output.format = '.grd',
    build.clamping.mask = FALSE
  )
  
  save(proyeccion, file = file.path(rutas$proyeccion, 
                                    paste0(nombre_archivo, "_proj_presente_version_final.RData")))
  
  # Raster continuo
  raster_proj <- get_predictions(proyeccion)
  if (nlyr(raster_proj) >= 3) {
    raster_proj <- raster_proj[[3]]  # allData_allRun
  } else {
    raster_proj <- raster_proj[[1]]
  }
  raster_final <- raster_proj / 1000
  
  writeRaster(raster_final, file.path(rutas$proyeccion, 
                                      paste0(nombre_archivo, "_proj_presente_version_final.tif")), overwrite = TRUE)
  
  # Raster binario
  cutoff <- eval_final[eval_final$metric.eval == "TSS" & 
                         eval_final$PA == "allData" & 
                         eval_final$run == "allRun", "cutoff"]
  
  if (length(cutoff) > 0) {
    cutoff_val <- cutoff[1] / 1000
    m <- matrix(c(0, cutoff_val, 0, cutoff_val, 1, 1), ncol = 3, byrow = TRUE)
    raster_bin <- classify(raster_final, m)
    writeRaster(raster_bin, file.path(rutas$proyeccion, 
                                      paste0(nombre_archivo, "_proj_binary_presente_version_final.tif")), overwrite = TRUE)
  }
  
  cat("\n✅", especie, "completada\n")
  
  return(list(
    especie = especie,
    estado = "Completada",
    n_obs = nrow(datos_especie)
  ))
}

# -------------------------------
# 6. LOOP PRINCIPAL (USANDO DATOS YA FILTRADOS)
# -------------------------------

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("=== INICIANDO PROCESAMIENTO DE ESPECIES ===\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

especies_unicas <- unique(obs_filtradas$Name)
cat("Especies a procesar:", length(especies_unicas), "\n")

resultados <- list()

for (i in seq_along(especies_unicas)) {
  sp <- especies_unicas[i]
  cat("\n", paste(rep("-", 40), collapse = ""), "\n")
  cat("Progreso:", i, "de", length(especies_unicas), "\n")
  
  # Datos de esta especie (YA FILTRADOS)
  datos_sp <- obs_filtradas %>% filter(Name == sp)
  
  tryCatch({
    res <- procesar_especie(sp, datos_sp, predictors, RUTAS)
    resultados[[sp]] <- res
  }, error = function(e) {
    cat("❌ Error en", sp, ":", e$message, "\n")
    resultados[[sp]] <<- list(
      especie = sp,
      estado = paste("Error:", e$message),
      n_obs = nrow(datos_sp)
    )
  })
}

# -------------------------------
# 7. RESUMEN FINAL
# -------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("=== RESUMEN FINAL ===\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Crear dataframe de resultados
resultados_df <- map_df(resultados, ~{
  data.frame(
    especie = .x$especie,
    estado = .x$estado,
    n_observaciones = .x$n_obs,
    stringsAsFactors = FALSE
  )
})

print(resultados_df)

# Guardar resumen
write.csv(resultados_df, file.path(RUTAS$modelos, "resumen_procesamiento_especies.csv"), row.names = FALSE)

# Estadísticas finales
cat("\n--- ESTADÍSTICAS ---\n")
cat("Total especies:", nrow(resultados_df), "\n")
cat("Completadas:", sum(resultados_df$estado == "Completada"), "\n")
cat("Saltadas (n<5):", sum(grepl("Saltada", resultados_df$estado)), "\n")
cat("Errores:", sum(grepl("Error", resultados_df$estado)), "\n")

cat("\n✅ PROCESO COMPLETADO\n")
cat("Resumen guardado en:", file.path(RUTAS$modelos, "resumen_procesamiento_especies.csv"), "\n")





















# =====================================================
# MODELADO DE DISTRIBUCIÓN - MÚLTIPLES ESPECIES (MEDITERRANEO)
# CON IMPORTANCIA DE VARIABLES NORMALIZADA
# Y CURVAS DE RESPUESTA GUARDADAS EN PDF
# =====================================================

# Limpiar workspace
rm(list = ls())

# -------------------------------
# 1. PAQUETES
# -------------------------------
library(raster)
library(biomod2)
library(dplyr)
library(terra)
library(tidyr)
library(purrr)

# -------------------------------
# 2. CONFIGURACIÓN DE RUTAS
# -------------------------------
RUTAS <- list(
  bio6 = "C:/Users/JorgeCV/Desktop/TFM/Variables/variables climaticas futuro pesimista/CHELSA_PROMEDIO_ssp585_bio06_2071-2100_V.2.1.tif",
  bio7 = "C:/Users/JorgeCV/Desktop/TFM/Variables/variables climaticas futuro pesimista/CHELSA_PROMEDIO_ssp585_bio07_2071-2100_V.2.1.tif",
  bio14 = "C:/Users/JorgeCV/Desktop/TFM/Variables/variables climaticas futuro pesimista/CHELSA_PROMEDIO_ssp585_bio14_2071-2100_V.2.1.tif",
  sand = "C:/Users/JorgeCV/Desktop/TFM/Variables/sand_study_area_WGS_final_1km.tif",
  ph = "C:/Users/JorgeCV/Desktop/TFM/Variables/PH_study_area_WGS_final_1km.tif",
  ocurrencias = "C:/Users/JorgeCV/Desktop/TFM/Observaciones/ptos-definitivo.csv",
  
  modelos = "C:/Users/JorgeCV/Desktop/TFM/Modelos/modelos mediterranea version final",
  evaluacion = "C:/Users/JorgeCV/Desktop/TFM/Evaluacion modelos/evaluacion mediterranea version final",
  importancia = "C:/Users/JorgeCV/Desktop/TFM/importancia relativa variables/importancia mediterranea version final",
  proyeccion = "C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion mediterranea version final",
  curva_respuesta = "C:/Users/JorgeCV/Desktop/TFM/curva respuesta/curva mediterranea version final"
)


# Crear directorios
for (dir in unique(unlist(RUTAS[6:10]))) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

# -------------------------------
# 3. PREPARAR VARIABLES AMBIENTALES
# -------------------------------

cat("\n=== PREPARANDO VARIABLES AMBIENTALES ===\n")

# Función para cargar raster
cargar_raster <- function(r) {
  if (!file.exists(r)) stop(paste("Archivo no encontrado:", r))
  raster(r)
}

# Cargar rasters
bio6 <- cargar_raster(RUTAS$bio6) 
bio7 <- cargar_raster(RUTAS$bio7) 
bio14 <- cargar_raster(RUTAS$bio14)   # Convertir a mm
sand <- cargar_raster(RUTAS$sand)
ph <- cargar_raster(RUTAS$ph)


# Homogeneizar extensiones
extension_comun <- intersect(extent(sand), extent(ph))
plantilla <- raster(extension_comun, res = res(sand), crs = crs(sand))

# Resamplear todos a la misma plantilla
ph <- resample(ph, plantilla, method = "bilinear")
bio6 <- resample(bio6, plantilla, method = "bilinear")
bio7 <- resample(bio7, plantilla, method = "bilinear")
bio14 <- resample(bio14, plantilla, method = "bilinear")

# Crear máscara común
mascara <- !is.na(sand)
mascara[mascara] <- 1

# Aplicar máscara
ph <- mask(ph, mascara)
bio6 <- mask(bio6, mascara)
bio7 <- mask(bio7, mascara)
bio14 <- mask(bio14, mascara)
sand <- mask(sand, mascara)

cat("bio6 (°C):", cellStats(bio6, range, na.rm=TRUE), "\n")
cat("bio7 (°C):", cellStats(bio7, range, na.rm=TRUE), "\n")
cat("bio14 (mm):", cellStats(bio14, range, na.rm=TRUE), "\n")
# Stack final
predictors <- stack(bio6, bio7, bio14, sand, ph)
names(predictors) <- c("bio6", "bio7", "bio14", "sand", "ph")

cat("✅ Variables preparadas\n")

# -------------------------------
# 4. CARGAR Y FILTRAR OBSERVACIONES (TODAS LAS ESPECIES JUNTAS)
# -------------------------------

cat("\n=== CARGANDO Y FILTRANDO OBSERVACIONES ===\n")

obs <- read.csv(RUTAS$ocurrencias) %>%
  dplyr::select(species, longitude, latitude) %>%
  dplyr::rename(Name = species) %>%
  dplyr::mutate(Response = 1)

cat("Observaciones totales:", nrow(obs), "\n")
cat("Especies únicas:", length(unique(obs$Name)), "\n")

# ASIGNAR ID DE PÍXEL A CADA OBSERVACIÓN (TODAS JUNTAS)
obs$cell_id <- cellFromXY(predictors[[1]], obs[, c("longitude", "latitude")])
obs <- obs %>% filter(!is.na(cell_id))  # Eliminar observaciones fuera del raster

cat("Observaciones dentro del área de estudio:", nrow(obs), "\n")

# FILTRAR 1 OBSERVACIÓN POR ESPECIE Y PÍXEL (TODAS JUNTAS)
set.seed(123)  # Para reproducibilidad
obs_filtradas <- obs %>%
  group_by(Name, cell_id) %>%        # Agrupar por especie Y píxel
  sample_n(size = 1) %>%              # Una observación aleatoria por grupo
  ungroup()

cat("Observaciones después de filtrar (1 por especie/píxel):", nrow(obs_filtradas), "\n")

# Verificar cuántas observaciones quedan por especie
resumen_especies <- obs_filtradas %>%
  group_by(Name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

cat("\nResumen por especie después de filtrar:\n")
print(resumen_especies)

# Guardar observaciones filtradas
write.csv(obs_filtradas, file.path(RUTAS$modelos, "observaciones_filtradas_1_por_pixel.csv"), row.names = FALSE)

# -------------------------------
# 5. FUNCIÓN PARA PROCESAR UNA ESPECIE
# -------------------------------

procesar_especie <- function(especie, datos_especie, predictors, rutas) {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("PROCESANDO:", especie, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Verificar número mínimo de observaciones
  if (nrow(datos_especie) < 5) {
    cat("⚠️  Especie con menos de 5 observaciones (", nrow(datos_especie), "). Saltando...\n")
    return(list(
      especie = especie,
      estado = "Saltada (n < 5)",
      n_obs = nrow(datos_especie)
    ))
  }
  
  cat("Observaciones para modelado:", nrow(datos_especie), "\n")
  
  # Nombre para archivos
  nombre_archivo <- gsub(" ", "_", especie)
  
  # 1. MODELOS DE mediterranea version final (RF + GBM)
  cat("\n--- MODELOS DE mediterranea version final ---\n")
  
  mod_test <- BIOMOD_FormatingData(
    resp.var = datos_especie$Response,
    expl.var = predictors,
    resp.xy = datos_especie[, c("longitude", "latitude")],
    resp.name = nombre_archivo,
    PA.strategy = "random",
    PA.nb.rep = 5,
    PA.nb.absences = nrow(datos_especie)
  )
  
  result_test <- BIOMOD_Modeling(
    bm.format = mod_test,
    models = c('RF', 'GBM'),
    CV.nb.rep = 5,
    CV.perc = 0.8,
    var.import = 3
  )
  
  # Guardar modelo de mediterranea version final
  save(result_test, file = file.path(rutas$modelos, 
                                     paste0(nombre_archivo, "_testing_mediterranea version final.RData")))
  
  # Evaluaciones de mediterranea version final
  eval_test <- as.data.frame(get_evaluations(result_test))
  write.csv(eval_test, file.path(rutas$evaluacion, 
                                 paste0(nombre_archivo, "_testing_eval.csv")), row.names = FALSE)
  
  # 2. MODELO FINAL (SOLO RF)
  cat("\n--- MODELO FINAL ---\n")
  
  mod_final <- BIOMOD_FormatingData(
    resp.var = datos_especie$Response,
    expl.var = predictors,
    resp.xy = datos_especie[, c("longitude", "latitude")],
    resp.name = nombre_archivo,
    PA.strategy = "random",
    PA.nb.rep = 1,
    PA.nb.absences = nrow(datos_especie)
  )
  
  result_final <- BIOMOD_Modeling(
    bm.format = mod_final,
    models = c('RF'),
    CV.nb.rep = 1,
    CV.perc = 0.8,
    var.import = 3
  )
  
  # Guardar modelo final
  save(result_final, file = file.path(rutas$modelos, 
                                      paste0(nombre_archivo, "_mediterranea version final.RData")))
  
  # Evaluaciones finales
  eval_final <- as.data.frame(get_evaluations(result_final))
  write.csv(eval_final, file.path(rutas$evaluacion, 
                                  paste0(nombre_archivo, "_final_eval.csv")), row.names = FALSE)
  
  # ============================================================
  # 3. IMPORTANCIA DE VARIABLES CON NORMALIZACIÓN (0-1)
  # ============================================================
  
  # Extraer importancia cruda
  var_imp <- get_variables_importance(result_final)
  
  # Calcular media por variable
  media_var_imp <- tapply(var_imp$var.imp, var_imp$expl.var, mean)
  
  # NORMALIZAR: dividir por la suma total para que sumen 1 (0-1)
  media_var_imp_norm <- media_var_imp / sum(media_var_imp)
  
  # Mostrar en consola
  cat("\n=== IMPORTANCIA DE VARIABLES (NORMALIZADA 0-1) ===\n")
  print(round(media_var_imp_norm, 3))
  
  # Crear dataframe con valores crudos y normalizados
  var_imp_df <- data.frame(
    variable = names(media_var_imp),
    importancia_cruda = as.numeric(media_var_imp),
    importancia_normalizada = as.numeric(media_var_imp_norm),
    porcentaje = as.numeric(media_var_imp_norm * 100)  # Porcentaje (0-100%)
  ) %>%
    arrange(desc(importancia_normalizada))
  
  # Guardar CSV
  write.csv(var_imp_df, file.path(rutas$importancia, 
                                  paste0(nombre_archivo, "_var_imp.csv")), row.names = FALSE)
  
  # ============================================================
  # 4. CURVAS DE RESPUESTA (OPCIÓN 1: PDF)
  # ============================================================
  cat("\n--- CURVAS DE RESPUESTA ---\n")
  
  tryCatch({
    # Cargar modelos Random Forest
    modelos_rf <- BIOMOD_LoadModels(result_final)
    modelos_rf <- grep("allData_allRun", modelos_rf, value = TRUE)
    
    if (length(modelos_rf) > 0) {
      # Abrir dispositivo PDF
      # DESPUÉS (guarda en curva_respuesta)
      pdf(file.path(rutas$curva_respuesta, paste0(nombre_archivo, "_response_curves.pdf")), 
          width = 12, height = 10)
      
      # Generar curvas
      curvas <- bm_PlotResponseCurves(
        bm.out = result_final,
        models.chosen = modelos_rf,
        new.env = get_formal_data(result_final, 'expl.var'),
        show.variables = get_formal_data(result_final, 'expl.var.names'),
        do.bivariate = FALSE,
        fixed.var.metric = 'median',
        col = c("blue", "red"),
        legend = TRUE
      )
      
      # Cerrar PDF
      dev.off()
      
      cat("✅ Curvas de respuesta guardadas en PDF:", 
          paste0(nombre_archivo, "_response_curves.pdf"), "\n")
      
    } else {
      cat("⚠️ No se encontraron modelos para curvas de respuesta\n")
    }
  }, error = function(e) {
    cat("⚠️ Error al generar curvas de respuesta:", e$message, "\n")
  })
  
  # ============================================================
  # 5. PROYECCIÓN
  # ============================================================
  cat("\n--- GENERANDO PROYECCIÓN ---\n")
  
  proyeccion <- BIOMOD_Projection(
    bm.mod = result_final,
    new.env = predictors,
    proj.name = 'map',
    compress = FALSE,
    output.format = '.grd',
    build.clamping.mask = FALSE
  )
  
  save(proyeccion, file = file.path(rutas$proyeccion, 
                                    paste0(nombre_archivo, "_proj_mediterranea_version_final.RData")))
  
  # Raster continuo
  raster_proj <- get_predictions(proyeccion)
  if (nlyr(raster_proj) >= 3) {
    raster_proj <- raster_proj[[3]]  # allData_allRun
  } else {
    raster_proj <- raster_proj[[1]]
  }
  raster_final <- raster_proj / 1000
  
  writeRaster(raster_final, file.path(rutas$proyeccion, 
                                      paste0(nombre_archivo, "_proj_mediterranea_version_final.tif")), overwrite = TRUE)
  
  # Raster binario
  cutoff <- eval_final[eval_final$metric.eval == "TSS" & 
                         eval_final$PA == "allData" & 
                         eval_final$run == "allRun", "cutoff"]
  
  if (length(cutoff) > 0) {
    cutoff_val <- cutoff[1] / 1000
    m <- matrix(c(0, cutoff_val, 0, cutoff_val, 1, 1), ncol = 3, byrow = TRUE)
    raster_bin <- classify(raster_final, m)
    writeRaster(raster_bin, file.path(rutas$proyeccion, 
                                      paste0(nombre_archivo, "_proj_binary_mediterranea_version_final.tif")), overwrite = TRUE)
  }
  
  cat("\n✅", especie, "completada\n")
  
  return(list(
    especie = especie,
    estado = "Completada",
    n_obs = nrow(datos_especie)
  ))
}

# -------------------------------
# 6. LOOP PRINCIPAL (USANDO DATOS YA FILTRADOS)
# -------------------------------

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("=== INICIANDO PROCESAMIENTO DE ESPECIES ===\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

especies_unicas <- unique(obs_filtradas$Name)
cat("Especies a procesar:", length(especies_unicas), "\n")

resultados <- list()

for (i in seq_along(especies_unicas)) {
  sp <- especies_unicas[i]
  cat("\n", paste(rep("-", 40), collapse = ""), "\n")
  cat("Progreso:", i, "de", length(especies_unicas), "\n")
  
  # Datos de esta especie (YA FILTRADOS)
  datos_sp <- obs_filtradas %>% filter(Name == sp)
  
  tryCatch({
    res <- procesar_especie(sp, datos_sp, predictors, RUTAS)
    resultados[[sp]] <- res
  }, error = function(e) {
    cat("❌ Error en", sp, ":", e$message, "\n")
    resultados[[sp]] <<- list(
      especie = sp,
      estado = paste("Error:", e$message),
      n_obs = nrow(datos_sp)
    )
  })
}

# -------------------------------
# 7. RESUMEN FINAL
# -------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("=== RESUMEN FINAL ===\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Crear dataframe de resultados
resultados_df <- map_df(resultados, ~{
  data.frame(
    especie = .x$especie,
    estado = .x$estado,
    n_observaciones = .x$n_obs,
    stringsAsFactors = FALSE
  )
})

print(resultados_df)

# Guardar resumen
write.csv(resultados_df, file.path(RUTAS$modelos, "resumen_procesamiento_especies.csv"), row.names = FALSE)

# Estadísticas finales
cat("\n--- ESTADÍSTICAS ---\n")
cat("Total especies:", nrow(resultados_df), "\n")
cat("Completadas:", sum(resultados_df$estado == "Completada"), "\n")
cat("Saltadas (n<5):", sum(grepl("Saltada", resultados_df$estado)), "\n")
cat("Errores:", sum(grepl("Error", resultados_df$estado)), "\n")

cat("\n✅ PROCESO COMPLETADO\n")
cat("Resumen guardado en:", file.path(RUTAS$modelos, "resumen_procesamiento_especies.csv"), "\n")





























# =====================================================
# MODELADO DE DISTRIBUCIÓN - MÚLTIPLES ESPECIES (SUBTROPICAL)
# CON IMPORTANCIA DE VARIABLES NORMALIZADA
# Y CURVAS DE RESPUESTA GUARDADAS EN PDF
# =====================================================

# Limpiar workspace
rm(list = ls())

# -------------------------------
# 1. PAQUETES
# -------------------------------
library(raster)
library(biomod2)
library(dplyr)
library(terra)
library(tidyr)
library(purrr)

# -------------------------------
# 2. CONFIGURACIÓN DE RUTAS
# -------------------------------
RUTAS <- list(
  bio6 = "C:/Users/JorgeCV/Desktop/TFM/Variables/variables climaticas futuro pesimista/CHELSA_PROMEDIO_ssp585_bio06_2071-2100_V.2.1.tif",
  bio7 = "C:/Users/JorgeCV/Desktop/TFM/Variables/variables climaticas futuro pesimista/CHELSA_PROMEDIO_ssp585_bio07_2071-2100_V.2.1.tif",
  bio14 = "C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio14_1981-2010_V.2.1_WGS_1km.tif",
  sand = "C:/Users/JorgeCV/Desktop/TFM/Variables/sand_study_area_WGS_final_1km.tif",
  ph = "C:/Users/JorgeCV/Desktop/TFM/Variables/PH_study_area_WGS_final_1km.tif",
  ocurrencias = "C:/Users/JorgeCV/Desktop/TFM/Observaciones/ptos-definitivo.csv",
  
  modelos = "C:/Users/JorgeCV/Desktop/TFM/Modelos/modelos subtropical version final",
  evaluacion = "C:/Users/JorgeCV/Desktop/TFM/Evaluacion modelos/evaluacion subtropical version final",
  importancia = "C:/Users/JorgeCV/Desktop/TFM/importancia relativa variables/importancia subtropical version final",
  proyeccion = "C:/Users/JorgeCV/Desktop/TFM/proyeccion modelos/proyeccion subtropical version final",
  curva_respuesta = "C:/Users/JorgeCV/Desktop/TFM/curva respuesta/curva subtropical version final"
)


# Crear directorios
for (dir in unique(unlist(RUTAS[6:10]))) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

# -------------------------------
# 3. PREPARAR VARIABLES AMBIENTALES
# -------------------------------

cat("\n=== PREPARANDO VARIABLES AMBIENTALES ===\n")

# Función para cargar raster
cargar_raster <- function(r) {
  if (!file.exists(r)) stop(paste("Archivo no encontrado:", r))
  raster(r)
}

# Cargar rasters
bio6 <- cargar_raster(RUTAS$bio6) 
bio7 <- cargar_raster(RUTAS$bio7) 
bio14 <- cargar_raster(RUTAS$bio14) * 0.1  # Convertir a mm
sand <- cargar_raster(RUTAS$sand)
ph <- cargar_raster(RUTAS$ph)


# Homogeneizar extensiones
extension_comun <- intersect(extent(sand), extent(ph))
plantilla <- raster(extension_comun, res = res(sand), crs = crs(sand))

# Resamplear todos a la misma plantilla
ph <- resample(ph, plantilla, method = "bilinear")
bio6 <- resample(bio6, plantilla, method = "bilinear")
bio7 <- resample(bio7, plantilla, method = "bilinear")
bio14 <- resample(bio14, plantilla, method = "bilinear")

# Crear máscara común
mascara <- !is.na(sand)
mascara[mascara] <- 1

# Aplicar máscara
ph <- mask(ph, mascara)
bio6 <- mask(bio6, mascara)
bio7 <- mask(bio7, mascara)
bio14 <- mask(bio14, mascara)
sand <- mask(sand, mascara)

cat("bio6 (°C):", cellStats(bio6, range, na.rm=TRUE), "\n")
cat("bio7 (°C):", cellStats(bio7, range, na.rm=TRUE), "\n")
cat("bio14 (mm):", cellStats(bio14, range, na.rm=TRUE), "\n")
# Stack final
predictors <- stack(bio6, bio7, bio14, sand, ph)
names(predictors) <- c("bio6", "bio7", "bio14", "sand", "ph")

cat("✅ Variables preparadas\n")

# -------------------------------
# 4. CARGAR Y FILTRAR OBSERVACIONES (TODAS LAS ESPECIES JUNTAS)
# -------------------------------

cat("\n=== CARGANDO Y FILTRANDO OBSERVACIONES ===\n")

obs <- read.csv(RUTAS$ocurrencias) %>%
  dplyr::select(species, longitude, latitude) %>%
  dplyr::rename(Name = species) %>%
  dplyr::mutate(Response = 1)

cat("Observaciones totales:", nrow(obs), "\n")
cat("Especies únicas:", length(unique(obs$Name)), "\n")

# ASIGNAR ID DE PÍXEL A CADA OBSERVACIÓN (TODAS JUNTAS)
obs$cell_id <- cellFromXY(predictors[[1]], obs[, c("longitude", "latitude")])
obs <- obs %>% filter(!is.na(cell_id))  # Eliminar observaciones fuera del raster

cat("Observaciones dentro del área de estudio:", nrow(obs), "\n")

# FILTRAR 1 OBSERVACIÓN POR ESPECIE Y PÍXEL (TODAS JUNTAS)
set.seed(123)  # Para reproducibilidad
obs_filtradas <- obs %>%
  group_by(Name, cell_id) %>%        # Agrupar por especie Y píxel
  sample_n(size = 1) %>%              # Una observación aleatoria por grupo
  ungroup()

cat("Observaciones después de filtrar (1 por especie/píxel):", nrow(obs_filtradas), "\n")

# Verificar cuántas observaciones quedan por especie
resumen_especies <- obs_filtradas %>%
  group_by(Name) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

cat("\nResumen por especie después de filtrar:\n")
print(resumen_especies)

# Guardar observaciones filtradas
write.csv(obs_filtradas, file.path(RUTAS$modelos, "observaciones_filtradas_1_por_pixel.csv"), row.names = FALSE)

# -------------------------------
# 5. FUNCIÓN PARA PROCESAR UNA ESPECIE
# -------------------------------

procesar_especie <- function(especie, datos_especie, predictors, rutas) {
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("PROCESANDO:", especie, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Verificar número mínimo de observaciones
  if (nrow(datos_especie) < 5) {
    cat("⚠️  Especie con menos de 5 observaciones (", nrow(datos_especie), "). Saltando...\n")
    return(list(
      especie = especie,
      estado = "Saltada (n < 5)",
      n_obs = nrow(datos_especie)
    ))
  }
  
  cat("Observaciones para modelado:", nrow(datos_especie), "\n")
  
  # Nombre para archivos
  nombre_archivo <- gsub(" ", "_", especie)
  
  # 1. MODELOS DE subtropical version final (RF + GBM)
  cat("\n--- MODELOS DE subtropical version final ---\n")
  
  mod_test <- BIOMOD_FormatingData(
    resp.var = datos_especie$Response,
    expl.var = predictors,
    resp.xy = datos_especie[, c("longitude", "latitude")],
    resp.name = nombre_archivo,
    PA.strategy = "random",
    PA.nb.rep = 5,
    PA.nb.absences = nrow(datos_especie)
  )
  
  result_test <- BIOMOD_Modeling(
    bm.format = mod_test,
    models = c('RF', 'GBM'),
    CV.nb.rep = 5,
    CV.perc = 0.8,
    var.import = 3
  )
  
  # Guardar modelo de subtropical version final
  save(result_test, file = file.path(rutas$modelos, 
                                     paste0(nombre_archivo, "_testing_subtropical version final.RData")))
  
  # Evaluaciones de subtropical version final
  eval_test <- as.data.frame(get_evaluations(result_test))
  write.csv(eval_test, file.path(rutas$evaluacion, 
                                 paste0(nombre_archivo, "_testing_eval.csv")), row.names = FALSE)
  
  # 2. MODELO FINAL (SOLO RF)
  cat("\n--- MODELO FINAL ---\n")
  
  mod_final <- BIOMOD_FormatingData(
    resp.var = datos_especie$Response,
    expl.var = predictors,
    resp.xy = datos_especie[, c("longitude", "latitude")],
    resp.name = nombre_archivo,
    PA.strategy = "random",
    PA.nb.rep = 1,
    PA.nb.absences = nrow(datos_especie)
  )
  
  result_final <- BIOMOD_Modeling(
    bm.format = mod_final,
    models = c('RF'),
    CV.nb.rep = 1,
    CV.perc = 0.8,
    var.import = 3
  )
  
  # Guardar modelo final
  save(result_final, file = file.path(rutas$modelos, 
                                      paste0(nombre_archivo, "_subtropical version final.RData")))
  
  # Evaluaciones finales
  eval_final <- as.data.frame(get_evaluations(result_final))
  write.csv(eval_final, file.path(rutas$evaluacion, 
                                  paste0(nombre_archivo, "_final_eval.csv")), row.names = FALSE)
  
  # ============================================================
  # 3. IMPORTANCIA DE VARIABLES CON NORMALIZACIÓN (0-1)
  # ============================================================
  
  # Extraer importancia cruda
  var_imp <- get_variables_importance(result_final)
  
  # Calcular media por variable
  media_var_imp <- tapply(var_imp$var.imp, var_imp$expl.var, mean)
  
  # NORMALIZAR: dividir por la suma total para que sumen 1 (0-1)
  media_var_imp_norm <- media_var_imp / sum(media_var_imp)
  
  # Mostrar en consola
  cat("\n=== IMPORTANCIA DE VARIABLES (NORMALIZADA 0-1) ===\n")
  print(round(media_var_imp_norm, 3))
  
  # Crear dataframe con valores crudos y normalizados
  var_imp_df <- data.frame(
    variable = names(media_var_imp),
    importancia_cruda = as.numeric(media_var_imp),
    importancia_normalizada = as.numeric(media_var_imp_norm),
    porcentaje = as.numeric(media_var_imp_norm * 100)  # Porcentaje (0-100%)
  ) %>%
    arrange(desc(importancia_normalizada))
  
  # Guardar CSV
  write.csv(var_imp_df, file.path(rutas$importancia, 
                                  paste0(nombre_archivo, "_var_imp.csv")), row.names = FALSE)
  
  # ============================================================
  # 4. CURVAS DE RESPUESTA (OPCIÓN 1: PDF)
  # ============================================================
  cat("\n--- CURVAS DE RESPUESTA ---\n")
  
  tryCatch({
    # Cargar modelos Random Forest
    modelos_rf <- BIOMOD_LoadModels(result_final)
    modelos_rf <- grep("allData_allRun", modelos_rf, value = TRUE)
    
    if (length(modelos_rf) > 0) {
      # Abrir dispositivo PDF
      # DESPUÉS (guarda en curva_respuesta)
      pdf(file.path(rutas$curva_respuesta, paste0(nombre_archivo, "_response_curves.pdf")), 
          width = 12, height = 10)
      
      # Generar curvas
      curvas <- bm_PlotResponseCurves(
        bm.out = result_final,
        models.chosen = modelos_rf,
        new.env = get_formal_data(result_final, 'expl.var'),
        show.variables = get_formal_data(result_final, 'expl.var.names'),
        do.bivariate = FALSE,
        fixed.var.metric = 'median',
        col = c("blue", "red"),
        legend = TRUE
      )
      
      # Cerrar PDF
      dev.off()
      
      cat("✅ Curvas de respuesta guardadas en PDF:", 
          paste0(nombre_archivo, "_response_curves.pdf"), "\n")
      
    } else {
      cat("⚠️ No se encontraron modelos para curvas de respuesta\n")
    }
  }, error = function(e) {
    cat("⚠️ Error al generar curvas de respuesta:", e$message, "\n")
  })
  
  # ============================================================
  # 5. PROYECCIÓN
  # ============================================================
  cat("\n--- GENERANDO PROYECCIÓN ---\n")
  
  proyeccion <- BIOMOD_Projection(
    bm.mod = result_final,
    new.env = predictors,
    proj.name = 'map',
    compress = FALSE,
    output.format = '.grd',
    build.clamping.mask = FALSE
  )
  
  save(proyeccion, file = file.path(rutas$proyeccion, 
                                    paste0(nombre_archivo, "_proj_subtropical_version_final.RData")))
  
  # Raster continuo
  raster_proj <- get_predictions(proyeccion)
  if (nlyr(raster_proj) >= 3) {
    raster_proj <- raster_proj[[3]]  # allData_allRun
  } else {
    raster_proj <- raster_proj[[1]]
  }
  raster_final <- raster_proj / 1000
  
  writeRaster(raster_final, file.path(rutas$proyeccion, 
                                      paste0(nombre_archivo, "_proj_subtropical_version_final.tif")), overwrite = TRUE)
  
  # Raster binario
  cutoff <- eval_final[eval_final$metric.eval == "TSS" & 
                         eval_final$PA == "allData" & 
                         eval_final$run == "allRun", "cutoff"]
  
  if (length(cutoff) > 0) {
    cutoff_val <- cutoff[1] / 1000
    m <- matrix(c(0, cutoff_val, 0, cutoff_val, 1, 1), ncol = 3, byrow = TRUE)
    raster_bin <- classify(raster_final, m)
    writeRaster(raster_bin, file.path(rutas$proyeccion, 
                                      paste0(nombre_archivo, "_proj_binary_subtropical_version_final.tif")), overwrite = TRUE)
  }
  
  cat("\n✅", especie, "completada\n")
  
  return(list(
    especie = especie,
    estado = "Completada",
    n_obs = nrow(datos_especie)
  ))
}

# -------------------------------
# 6. LOOP PRINCIPAL (USANDO DATOS YA FILTRADOS)
# -------------------------------

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("=== INICIANDO PROCESAMIENTO DE ESPECIES ===\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

especies_unicas <- unique(obs_filtradas$Name)
cat("Especies a procesar:", length(especies_unicas), "\n")

resultados <- list()

for (i in seq_along(especies_unicas)) {
  sp <- especies_unicas[i]
  cat("\n", paste(rep("-", 40), collapse = ""), "\n")
  cat("Progreso:", i, "de", length(especies_unicas), "\n")
  
  # Datos de esta especie (YA FILTRADOS)
  datos_sp <- obs_filtradas %>% filter(Name == sp)
  
  tryCatch({
    res <- procesar_especie(sp, datos_sp, predictors, RUTAS)
    resultados[[sp]] <- res
  }, error = function(e) {
    cat("❌ Error en", sp, ":", e$message, "\n")
    resultados[[sp]] <<- list(
      especie = sp,
      estado = paste("Error:", e$message),
      n_obs = nrow(datos_sp)
    )
  })
}

# -------------------------------
# 7. RESUMEN FINAL
# -------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("=== RESUMEN FINAL ===\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Crear dataframe de resultados
resultados_df <- map_df(resultados, ~{
  data.frame(
    especie = .x$especie,
    estado = .x$estado,
    n_observaciones = .x$n_obs,
    stringsAsFactors = FALSE
  )
})

print(resultados_df)

# Guardar resumen
write.csv(resultados_df, file.path(RUTAS$modelos, "resumen_procesamiento_especies.csv"), row.names = FALSE)

# Estadísticas finales
cat("\n--- ESTADÍSTICAS ---\n")
cat("Total especies:", nrow(resultados_df), "\n")
cat("Completadas:", sum(resultados_df$estado == "Completada"), "\n")
cat("Saltadas (n<5):", sum(grepl("Saltada", resultados_df$estado)), "\n")
cat("Errores:", sum(grepl("Error", resultados_df$estado)), "\n")

cat("\n✅ PROCESO COMPLETADO\n")
cat("Resumen guardado en:", file.path(RUTAS$modelos, "resumen_procesamiento_especies.csv"), "\n")