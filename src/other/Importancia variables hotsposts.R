# Cargar librerías 
library(tidyverse)
library(readr)

# 1. 
# Establecer la ruta 
carpeta <- "C:/Users/JorgeCV/Desktop/TFM/importancia relativa variables/importancia final futuro subtropical" 

# Obtener lista de todos los archivos CSV
archivos <- list.files(carpeta, pattern = "\\.csv$", full.names = TRUE)

# 2.LEER CADA ARCHIVO Y EXTRAER NOMBRE DE ESPECIE

crear_fila_especie <- function(archivo) {
  # Extraer nombre de la especie del nombre del archivo
  nombre_especie <- tools::file_path_sans_ext(basename(archivo))
  
  # Leer el archivo
  datos <- read_csv(archivo, show_col_types = FALSE)
  
  # Crear un vector con los valores de importancia
  # Usamos los nombres de las variables como nombres del vector
  valores_importancia <- datos$importancia
  names(valores_importancia) <- datos$variable
  
  # Convertir a data frame de una fila
  fila_especie <- as.data.frame(t(valores_importancia))
  
  # Agregar columna con nombre de especie
  fila_especie$especie <- nombre_especie
  
  return(fila_especie)
}

# 3. APLICAR A TODOS LOS ARCHIVOS Y COMBINAR
# ------------------------------------------


# Aplicar la función a todos los archivos
lista_filas <- map(archivos, crear_fila_especie)

# Combinar todas las filas
matriz_especies <- bind_rows(lista_filas)

# 4. REORGANIZAR PARA QUE LA PRIMERA COLUMNA SEA ESPECIE
# ------------------------------------------------------
matriz_especies <- matriz_especies %>%
  select(especie, everything())  # especie primero, luego todas las variables

# 5. RELLENAR VALORES FALTANTES CON 0
# ---------------------------------------------
# Si una especie no tiene cierta variable, poner 0
matriz_especies[is.na(matriz_especies)] <- 0

# 6. VER RESULTADO
# ---------------

print(matriz_especies)

# 7. GUARDAR RESULTADOS
# --------------------

# Guardar
library(openxlsx)
write.xlsx(matriz_especies, "C:/Users/JorgeCV/Desktop/TFM/importancia relativa variables/importancia final futuro subtropical/matriz_importancia_especies_x_variables_futuro_subtropical.xlsx")





