# ============================================================
# PROMEDIO MANUAL DE ARCHIVOS TIF SELECCIONADOS
# ============================================================

# PASO 1: Cargar librería
library(terra)

# PASO 2: TÚ SELECCIONAS LOS ARCHIVOS (MODIFICA ESTAS RUTAS)
# ------------------------------------------------------------
# Lista manual de los archivos

archivos_a_promediar <- c(
  "C:/Users/JorgeCV/Desktop/TFM/Variables/variables climaticas futuro pesimista/CHELSA_gfdl-esm4_ssp585_bio14_2071-2100_V.2.1.tif",
  "C:/Users/JorgeCV/Desktop/TFM/Variables/variables climaticas futuro pesimista/CHELSA_ipsl-cm6a-lr_ssp585_bio14_2071-2100_V.2.1.tif",
  "C:/Users/JorgeCV/Desktop/TFM/Variables/variables climaticas futuro pesimista/CHELSA_mpi-esm1-2-hr_ssp585_bio14_2071-2100_V.2.1.tif",
  "C:/Users/JorgeCV/Desktop/TFM/Variables/variables climaticas futuro pesimista/CHELSA_mri-esm2-0_ssp585_bio14_2071-2100_V.2.1.tif",
  "C:/Users/JorgeCV/Desktop/TFM/Variables/variables climaticas futuro pesimista/CHELSA_ukesm1-0-ll_ssp585_bio14_2071-2100_V.2.1.tif"
)

# PASO 3: NOMBRE DEL ARCHIVO DE SALIDA
# --------------------------------------
# Cambia esto por el nombre que quieras para tu resultado
archivo_salida <- "C:/Users/JorgeCV/Desktop/TFM/Variables/variables climaticas futuro pesimista/CHELSA_PROMEDIO_ssp585_bio14_2071-2100_V.2.1.tif"

# PASO 4: CARGAR Y PROMEDIAR
# ----------------------------

lista_rasters <- list()

for (i in seq_along(archivos_a_promediar)) {
  print(paste("Cargando:", basename(archivos_a_promediar[i])))
  lista_rasters[[i]] <- rast(archivos_a_promediar[i])
}


stack_modelos <- rast(lista_rasters)
promedio <- mean(stack_modelos, na.rm = TRUE)

# PASO 5: GUARDAR
# -----------------
print(paste("Guardando en:", archivo_salida))
writeRaster(promedio, archivo_salida, overwrite = TRUE)



# ============================================================
# PASO 6: PLOTEAR EL RESULTADO
# ============================================================


# Cargar el archivo generado (o usar el objeto 'promedio' que ya tenemos)
resultado <- rast(archivo_salida)

# Crear el plot
plot(resultado, 
     main = "Promedio bio14 - SSP585 (2071-2100)",
     col = hcl.colors(100, "Blues"))  # Puedes cambiar "Blues" por "Reds", "Viridis", etc.



