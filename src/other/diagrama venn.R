rm(list = ls())
# -------------------------
# Librerías
# -------------------------
library(tidyverse)
library(readxl)
library(VennDiagram)
library(terra)
library(grid)

# -------------------------
# Cargar datos
# -------------------------
helechos <- read_excel("C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_presente_helechos.xlsx")
invasoras <- read_excel("C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_presente_invasoras.xlsx")

r <- rast("C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio14_1981-2010_V.2.1_WGS_1km.tif")
total_pixeles <- global(!is.na(r), "sum", na.rm=TRUE)[1,1]

# -------------------------
# Filtrar hotspots
# -------------------------
helechos_hot <- helechos %>% filter(hotspots_total >= 7)
invasoras_hot <- invasoras %>% filter(hotspots_total >= 7)

# -------------------------
# Coordenadas a celdas raster
# -------------------------
cells_helechos <- cellFromXY(r, as.matrix(helechos_hot[,c("x","y")]))
cells_invasoras <- cellFromXY(r, as.matrix(invasoras_hot[,c("x","y")]))
cells_helechos <- cells_helechos[!is.na(cells_helechos)]
cells_invasoras <- cells_invasoras[!is.na(cells_invasoras)]

# -------------------------
# Exclusivos e intersección
# -------------------------
interseccion <- intersect(cells_helechos, cells_invasoras)
solo_helechos <- setdiff(cells_helechos, cells_invasoras)
solo_invasoras <- setdiff(cells_invasoras, cells_helechos)

# -------------------------
# Porcentajes
# -------------------------
p_solo_helechos <- round(length(solo_helechos)/total_pixeles*100,2)
p_solo_invasoras <- round(length(solo_invasoras)/total_pixeles*100,2)
p_inter <- round(length(interseccion)/total_pixeles*100,2)
p_total_helechos <- round(p_solo_helechos + p_inter, 2)
p_total_invasoras <- round(p_solo_invasoras + p_inter, 2)

# -------------------------
# Crear PNG
# -------------------------
png("C:/Users/JorgeCV/Desktop/TFM/Resultados/venn_hotspots_presente_porcentaje.png",
    width=2000, height=2000, res=400)

# Dibujar círculos sin etiquetas automáticas
draw.pairwise.venn(
  area1 = length(cells_helechos),
  area2 = length(cells_invasoras),
  cross.area = length(interseccion),
  category = c("", ""),
  fill = c(scales::alpha("blue",0.4), scales::alpha("red",0.4)),
  lwd = 2,
  col = c("blue","red"),
  cex = 0,
  cat.cex = 0,
  scaled = FALSE
)

# -------------------------
# Etiquetas internas centradas (corregido)
# -------------------------
# Helechos: derecha
grid.text(paste0(p_solo_helechos,"%"), x=0.80, y=0.52, gp=gpar(fontsize=24, fontface="bold"))
# Invasoras: izquierda
grid.text(paste0(p_solo_invasoras,"%"), x=0.20, y=0.52, gp=gpar(fontsize=24, fontface="bold"))
# Intersección: centro
grid.text(paste0(p_inter,"%"), x=0.5, y=0.52, gp=gpar(fontsize=24, fontface="bold"))

# -------------------------
# Porcentajes totales fuera de los círculos (arriba, corregido)
# -------------------------
grid.text(paste0(p_total_helechos,"%"), x=0.66, y=0.90, gp=gpar(fontsize=24, col="blue", fontface="bold"))
grid.text(paste0(p_total_invasoras,"%"), x=0.34, y=0.90, gp=gpar(fontsize=24, col="red", fontface="bold"))

# -------------------------
# Leyenda debajo (orden invertido y separada)
# -------------------------
legend_x <- c(0.3, 0.7)
legend_y <- rep(0.08, 2)
legend_colors <- c("red", "blue")
legend_labels <- c("Invasive species","Subtropical ferns")

grid.rect(x=legend_x, y=legend_y, width=0.05, height=0.03, gp=gpar(fill=legend_colors, col=NA))
grid.text(legend_labels, x=legend_x, y=legend_y-0.05, gp=gpar(fontsize=16))

dev.off()










rm(list = ls())
# -------------------------
# Librerías
# -------------------------
library(tidyverse)
library(readxl)
library(VennDiagram)
library(terra)
library(grid)

# -------------------------
# Cargar datos
# -------------------------
helechos <- read_excel("C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_mediterranea_helechos.xlsx")
invasoras <- read_excel("C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_mediterranea_invasoras.xlsx")

r <- rast("C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio14_1981-2010_V.2.1_WGS_1km.tif")
total_pixeles <- global(!is.na(r), "sum", na.rm=TRUE)[1,1]

# -------------------------
# Filtrar hotspots
# -------------------------
helechos_hot <- helechos %>% filter(hotspots_total >= 7)
invasoras_hot <- invasoras %>% filter(hotspots_total >= 7)

# -------------------------
# Coordenadas a celdas raster
# -------------------------
cells_helechos <- cellFromXY(r, as.matrix(helechos_hot[,c("x","y")]))
cells_invasoras <- cellFromXY(r, as.matrix(invasoras_hot[,c("x","y")]))
cells_helechos <- cells_helechos[!is.na(cells_helechos)]
cells_invasoras <- cells_invasoras[!is.na(cells_invasoras)]

# -------------------------
# Exclusivos e intersección
# -------------------------
interseccion <- intersect(cells_helechos, cells_invasoras)
solo_helechos <- setdiff(cells_helechos, cells_invasoras)
solo_invasoras <- setdiff(cells_invasoras, cells_helechos)

# -------------------------
# Porcentajes
# -------------------------
p_solo_helechos <- round(length(solo_helechos)/total_pixeles*100,2)
p_solo_invasoras <- round(length(solo_invasoras)/total_pixeles*100,2)
p_inter <- round(length(interseccion)/total_pixeles*100,2)
p_total_helechos <- round(p_solo_helechos + p_inter, 2)
p_total_invasoras <- round(p_solo_invasoras + p_inter, 2)

# -------------------------
# Crear PNG
# -------------------------
png("C:/Users/JorgeCV/Desktop/TFM/Resultados/venn_hotspots_mediterranea_porcentaje.png",
    width=2000, height=2000, res=400)

# Dibujar círculos sin etiquetas automáticas
draw.pairwise.venn(
  area1 = length(cells_helechos),
  area2 = length(cells_invasoras),
  cross.area = length(interseccion),
  category = c("", ""),
  fill = c(scales::alpha("blue",0.4), scales::alpha("red",0.4)),
  lwd = 2,
  col = c("blue","red"),
  cex = 0,
  cat.cex = 0,
  scaled = FALSE
)

# -------------------------
# Etiquetas internas centradas (corregido)
# -------------------------
# Helechos: derecha
grid.text(paste0(p_solo_helechos,"%"), x=0.80, y=0.52, gp=gpar(fontsize=24, fontface="bold"))
# Invasoras: izquierda
grid.text(paste0(p_solo_invasoras,"%"), x=0.20, y=0.52, gp=gpar(fontsize=24, fontface="bold"))
# Intersección: centro
grid.text(paste0(p_inter,"%"), x=0.5, y=0.52, gp=gpar(fontsize=24, fontface="bold"))

# -------------------------
# Porcentajes totales fuera de los círculos (arriba, corregido)
# -------------------------
grid.text(paste0(p_total_helechos,"%"), x=0.66, y=0.90, gp=gpar(fontsize=24, col="blue", fontface="bold"))
grid.text(paste0(p_total_invasoras,"%"), x=0.34, y=0.90, gp=gpar(fontsize=24, col="red", fontface="bold"))

# -------------------------
# Leyenda debajo (orden invertido y separada)
# -------------------------
legend_x <- c(0.3, 0.7)
legend_y <- rep(0.08, 2)
legend_colors <- c("red", "blue")
legend_labels <- c("Invasive species","Subtropical ferns")

grid.rect(x=legend_x, y=legend_y, width=0.05, height=0.03, gp=gpar(fill=legend_colors, col=NA))
grid.text(legend_labels, x=legend_x, y=legend_y-0.05, gp=gpar(fontsize=16))

dev.off()
















rm(list = ls())
# -------------------------
# Librerías
# -------------------------
library(tidyverse)
library(readxl)
library(VennDiagram)
library(terra)
library(grid)

# -------------------------
# Cargar datos
# -------------------------
helechos <- read_excel("C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_subtropical_helechos.xlsx")
invasoras <- read_excel("C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_subtropical_invasoras.xlsx")

r <- rast("C:/Users/JorgeCV/Desktop/TFM/Variables/CHELSA_bio14_1981-2010_V.2.1_WGS_1km.tif")
total_pixeles <- global(!is.na(r), "sum", na.rm=TRUE)[1,1]

# -------------------------
# Filtrar hotspots
# -------------------------
helechos_hot <- helechos %>% filter(hotspots_total >= 7)
invasoras_hot <- invasoras %>% filter(hotspots_total >= 7)

# -------------------------
# Coordenadas a celdas raster
# -------------------------
cells_helechos <- cellFromXY(r, as.matrix(helechos_hot[,c("x","y")]))
cells_invasoras <- cellFromXY(r, as.matrix(invasoras_hot[,c("x","y")]))
cells_helechos <- cells_helechos[!is.na(cells_helechos)]
cells_invasoras <- cells_invasoras[!is.na(cells_invasoras)]

# -------------------------
# Exclusivos e intersección
# -------------------------
interseccion <- intersect(cells_helechos, cells_invasoras)
solo_helechos <- setdiff(cells_helechos, cells_invasoras)
solo_invasoras <- setdiff(cells_invasoras, cells_helechos)

# -------------------------
# Porcentajes
# -------------------------
p_solo_helechos <- round(length(solo_helechos)/total_pixeles*100,2)
p_solo_invasoras <- round(length(solo_invasoras)/total_pixeles*100,2)
p_inter <- round(length(interseccion)/total_pixeles*100,2)
p_total_helechos <- round(p_solo_helechos + p_inter, 2)
p_total_invasoras <- round(p_solo_invasoras + p_inter, 2)

# -------------------------
# Crear PNG
# -------------------------
png("C:/Users/JorgeCV/Desktop/TFM/Resultados/venn_hotspots_subtropical_porcentaje.png",
    width=2000, height=2000, res=400)

# Dibujar círculos sin etiquetas automáticas
draw.pairwise.venn(
  area1 = length(cells_helechos),
  area2 = length(cells_invasoras),
  cross.area = length(interseccion),
  category = c("", ""),
  fill = c(scales::alpha("blue",0.4), scales::alpha("red",0.4)),
  lwd = 2,
  col = c("blue","red"),
  cex = 0,
  cat.cex = 0,
  scaled = FALSE
)

# -------------------------
# Etiquetas internas centradas (corregido)
# -------------------------
# Helechos: derecha
grid.text(paste0(p_solo_helechos,"%"), x=0.80, y=0.52, gp=gpar(fontsize=24, fontface="bold"))
# Invasoras: izquierda
grid.text(paste0(p_solo_invasoras,"%"), x=0.20, y=0.52, gp=gpar(fontsize=24, fontface="bold"))
# Intersección: centro
grid.text(paste0(p_inter,"%"), x=0.5, y=0.52, gp=gpar(fontsize=24, fontface="bold"))

# -------------------------
# Porcentajes totales fuera de los círculos (arriba, corregido)
# -------------------------
grid.text(paste0(p_total_helechos,"%"), x=0.66, y=0.90, gp=gpar(fontsize=24, col="blue", fontface="bold"))
grid.text(paste0(p_total_invasoras,"%"), x=0.34, y=0.90, gp=gpar(fontsize=24, col="red", fontface="bold"))

# -------------------------
# Leyenda debajo (orden invertido y separada)
# -------------------------
legend_x <- c(0.3, 0.7)
legend_y <- rep(0.08, 2)
legend_colors <- c("red", "blue")
legend_labels <- c("Invasive species","Subtropical ferns")

grid.rect(x=legend_x, y=legend_y, width=0.05, height=0.03, gp=gpar(fill=legend_colors, col=NA))
grid.text(legend_labels, x=legend_x, y=legend_y-0.05, gp=gpar(fontsize=16))

dev.off()

