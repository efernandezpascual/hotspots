# Cargar librerías necesarias
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

# -----------------------------
# Rutas de archivos
# -----------------------------
base_path_presente <- "C:/Users/JorgeCV/Desktop/TFM/Resultados/"
base_path_med <- "C:/Users/JorgeCV/Desktop/TFM/Resultados/"
base_path_sub <- "C:/Users/JorgeCV/Desktop/TFM/Resultados/"

# Helechos
helechos_presente <- paste0(base_path_presente, "hotspots_por_pixel_presente_helechos.xlsx")
helechos_med <- paste0(base_path_med, "hotspots_por_pixel_mediterranea_helechos.xlsx")
helechos_sub <- paste0(base_path_sub, "hotspots_por_pixel_subtropical_helechos.xlsx")

# Invasoras
invasoras_presente <- paste0(base_path_presente, "hotspots_por_pixel_presente_invasoras.xlsx")
invasoras_med <- paste0(base_path_med, "hotspots_por_pixel_mediterranea_invasoras.xlsx")
invasoras_sub <- paste0(base_path_sub, "hotspots_por_pixel_subtropical_invasoras.xlsx")

# Ruta para guardar el Excel final
ruta_guardado <- "C:/Users/JorgeCV/Desktop/TFM/Recuento pixeles/resumen_especies_con_totales.xlsx"

# -----------------------------
# Función para leer y limpiar
# -----------------------------
leer_limpiar <- function(archivo) {
  df <- read_xlsx(archivo)
  df <- df[, !grepl("hotspots", names(df), ignore.case = TRUE)]
  names(df)[1:2] <- c("X","Y")
  return(df)
}

# -----------------------------
# Leer archivos
# -----------------------------
df_helechos_pres <- leer_limpiar(helechos_presente)
df_helechos_med <- leer_limpiar(helechos_med)
df_helechos_sub <- leer_limpiar(helechos_sub)

df_invasoras_pres <- leer_limpiar(invasoras_presente)
df_invasoras_med <- leer_limpiar(invasoras_med)
df_invasoras_sub <- leer_limpiar(invasoras_sub)

# -----------------------------
# PASO 1: Comprobación de coordenadas
# -----------------------------
comprobar_coordenadas <- function(df_pres, df_med, df_sub, grupo) {
  coords_pres <- paste(df_pres$X, df_pres$Y)
  coords_med <- paste(df_med$X, df_med$Y)
  coords_sub <- paste(df_sub$X, df_sub$Y)
  
  comunes_pres_med <- intersect(coords_pres, coords_med)
  comunes_pres_sub <- intersect(coords_pres, coords_sub)
  
  pct_pres_med <- round(length(comunes_pres_med)/length(coords_pres)*100,1)
  pct_pres_sub <- round(length(comunes_pres_sub)/length(coords_pres)*100,1)
  
  cat("\n---", grupo, "---\n")
  cat("Presente:", length(coords_pres), "| Mediterráneo:", length(coords_med), "| Subtropical:", length(coords_sub), "\n")
  cat("Coincidencia Pres-Med:", length(comunes_pres_med), "(", pct_pres_med, "%)\n")
  cat("Coincidencia Pres-Sub:", length(comunes_pres_sub), "(", pct_pres_sub, "%)\n")
}

comprobar_coordenadas(df_helechos_pres, df_helechos_med, df_helechos_sub, "Helechos")
comprobar_coordenadas(df_invasoras_pres, df_invasoras_med, df_invasoras_sub, "Invasoras")

# -----------------------------
# PASO 2: Recuento de especies
# -----------------------------
contar_especies <- function(df_pres, df_med, df_sub, grupo) {
  especies_pres <- df_pres[, -c(1,2)]
  especies_med <- df_med[, -c(1,2)]
  especies_sub <- df_sub[, -c(1,2)]
  
  nombres_especies <- names(especies_pres)
  
  presente <- colSums(especies_pres, na.rm=TRUE)
  mediterraneo <- colSums(especies_med[, nombres_especies], na.rm=TRUE)
  subtropical <- colSums(especies_sub[, nombres_especies], na.rm=TRUE)
  
  resultados <- data.frame(
    Grupo = grupo,
    Especie = nombres_especies,
    Presente = presente,
    Mediterraneo = mediterraneo,
    Subtropical = subtropical,
    `%_Mediterraneo` = round((mediterraneo - presente)/presente*100,1),
    `%_Subtropical` = round((subtropical - presente)/presente*100,1),
    stringsAsFactors = FALSE
  )
  
  pixeles_unicos <- rowSums(especies_pres>0)>0
  
  return(list(resultados=resultados,
              pixeles_unicos=pixeles_unicos,
              especies_pres=especies_pres,
              especies_med=especies_med,
              especies_sub=especies_sub))
}

res_helechos_list <- contar_especies(df_helechos_pres, df_helechos_med, df_helechos_sub, "Helechos")
res_invasoras_list <- contar_especies(df_invasoras_pres, df_invasoras_med, df_invasoras_sub, "Invasoras")

res_helechos <- res_helechos_list$resultados
res_invasoras <- res_invasoras_list$resultados

# -----------------------------
# PASO 3: Hotspots y Subtotales
# -----------------------------
calcular_hotspots <- function(especies_pres, especies_med, especies_sub, grupo, nombre_hotspot="Hotspots") {
  pres_hot <- sum(rowSums(especies_pres>0) >= 7)
  med_hot <- sum(rowSums(especies_med>0) >= 7)
  sub_hot <- sum(rowSums(especies_sub>0) >= 7)
  
  data.frame(
    Grupo=grupo,
    Especie=nombre_hotspot,
    Presente=pres_hot,
    Mediterraneo=med_hot,
    Subtropical=sub_hot,
    `%_Mediterraneo`=round((med_hot - pres_hot)/pres_hot*100,1),
    `%_Subtropical`=round((sub_hot - pres_hot)/pres_hot*100,1),
    stringsAsFactors = FALSE
  )
}

hotspots_helechos <- calcular_hotspots(res_helechos_list$especies_pres,
                                       res_helechos_list$especies_med,
                                       res_helechos_list$especies_sub,
                                       "Helechos","Hotspots Helechos")

hotspots_invasoras <- calcular_hotspots(res_invasoras_list$especies_pres,
                                        res_invasoras_list$especies_med,
                                        res_invasoras_list$especies_sub,
                                        "Invasoras","Hotspots Invasoras")

subtotal_helechos <- data.frame(
  Grupo="Helechos",
  Especie="TOTAL Helechos",
  Presente=sum(res_helechos_list$pixeles_unicos),
  Mediterraneo=sum(rowSums(res_helechos_list$especies_med>0)>0),
  Subtropical=sum(rowSums(res_helechos_list$especies_sub>0)>0),
  `%_Mediterraneo`=round((sum(rowSums(res_helechos_list$especies_med>0)>0) - sum(res_helechos_list$pixeles_unicos))/sum(res_helechos_list$pixeles_unicos)*100,1),
  `%_Subtropical`=round((sum(rowSums(res_helechos_list$especies_sub>0)>0) - sum(res_helechos_list$pixeles_unicos))/sum(res_helechos_list$pixeles_unicos)*100,1),
  stringsAsFactors = FALSE
)

subtotal_invasoras <- data.frame(
  Grupo="Invasoras",
  Especie="TOTAL Invasoras",
  Presente=sum(res_invasoras_list$pixeles_unicos),
  Mediterraneo=sum(rowSums(res_invasoras_list$especies_med>0)>0),
  Subtropical=sum(rowSums(res_invasoras_list$especies_sub>0)>0),
  `%_Mediterraneo`=round((sum(rowSums(res_invasoras_list$especies_med>0)>0) - sum(res_invasoras_list$pixeles_unicos))/sum(res_invasoras_list$pixeles_unicos)*100,1),
  `%_Subtropical`=round((sum(rowSums(res_invasoras_list$especies_sub>0)>0) - sum(res_invasoras_list$pixeles_unicos))/sum(res_invasoras_list$pixeles_unicos)*100,1),
  stringsAsFactors = FALSE
)

# Hotspots totales y total combinado
mat_presente <- cbind(res_helechos_list$especies_pres>0, res_invasoras_list$especies_pres>0)
mat_med <- cbind(res_helechos_list$especies_med>0, res_invasoras_list$especies_med>0)
mat_sub <- cbind(res_helechos_list$especies_sub>0, res_invasoras_list$especies_sub>0)

hotspots_totales <- data.frame(
  Grupo="TOTAL",
  Especie="Hotspots Totales",
  Presente=sum(rowSums(mat_presente)>=7),
  Mediterraneo=sum(rowSums(mat_med)>=7),
  Subtropical=sum(rowSums(mat_sub)>=7),
  `%_Mediterraneo`=round((sum(rowSums(mat_med)>=7) - sum(rowSums(mat_presente)>=7))/sum(rowSums(mat_presente)>=7)*100,1),
  `%_Subtropical`=round((sum(rowSums(mat_sub)>=7) - sum(rowSums(mat_presente)>=7))/sum(rowSums(mat_presente)>=7)*100,1),
  stringsAsFactors = FALSE
)

total_general <- data.frame(
  Grupo="TOTAL",
  Especie="TODAS LAS ESPECIES",
  Presente=sum(rowSums(mat_presente)>0),
  Mediterraneo=sum(rowSums(mat_med)>0),
  Subtropical=sum(rowSums(mat_sub)>0),
  `%_Mediterraneo`=round((sum(rowSums(mat_med)>0) - sum(rowSums(mat_presente)>0))/sum(rowSums(mat_presente)>0)*100,1),
  `%_Subtropical`=round((sum(rowSums(mat_sub)>0) - sum(rowSums(mat_presente)>0))/sum(rowSums(mat_presente)>0)*100,1),
  stringsAsFactors = FALSE
)

# -----------------------------
# Combinar resultados finales
# -----------------------------
resultado_final <- bind_rows(
  res_helechos,
  hotspots_helechos,
  subtotal_helechos,
  res_invasoras,
  hotspots_invasoras,
  subtotal_invasoras,
  hotspots_totales,
  total_general
)

# Guardar Excel
write_xlsx(resultado_final, ruta_guardado)

# -----------------------------
# Resumen final
# -----------------------------
cat("\n✅ PROCESO COMPLETADO\n")
cat("Archivo guardado en:", ruta_guardado, "\n")