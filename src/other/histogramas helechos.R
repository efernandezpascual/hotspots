rm(list=ls())

# ================================
# Librerías
# ================================
library(raster)
library(sf)
library(dplyr)
library(rasterVis)
library(latticeExtra)
library(grid)
library(magick)

# ================================
# Paleta y breaks
# ================================
colores_personalizados <- c("orange","darkgreen")
breaks_agrupados <- c(0.5,6.5,9.5)

# ================================
# Cargar contorno Ibero-Atlántico
# ================================
ibero_atlantic <- st_read("C:/Users/JorgeCV/Desktop/2024 TFG Jorge Casado/QGIS/IberoAtlantic.shp")
ibero_atlantic_sp <- as(ibero_atlantic,"Spatial")

# ============================================================
# Función para crear mapas (histograma automático)
# ============================================================
crear_mapa <- function(raster_input, archivo){
  
  png(archivo,width=4000,height=3500,res=300)
  
  p <- levelplot(raster_input,
                 col.regions = colores_personalizados,
                 at = breaks_agrupados,
                 xlab=list("Longitude",cex=1.8),
                 ylab=list("Latitude",cex=1.8),
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 colorkey=FALSE,
                 par.settings = list(
                   layout.heights=list(
                     top.padding=-5,
                     main.key.padding=0.2,
                     axis.xlab.padding=1,
                     bottom.padding=5
                   ),
                   layout.widths=list(
                     left.padding=4,
                     right.padding=1
                   )
                 )) +
    latticeExtra::layer(sp.lines(ibero_atlantic_sp,col="black",lwd=0.8))
  
  print(p)
  
  # ===============================
  # Leyenda personalizada
  # ===============================
  grid.points(x = unit(0.35, "npc"), y = unit(0.13, "npc"),
              pch = 16, size = unit(1.5, "cm"),
              gp = gpar(col = "orange"))
  
  grid.text("Subtropical",
            x = unit(0.38, "npc"), y = unit(0.13, "npc"),
            just = "left", gp = gpar(cex = 1.8, fontface = "bold"))
  
  grid.points(x = unit(0.58, "npc"), y = unit(0.13, "npc"),
              pch = 16, size = unit(1.5, "cm"),
              gp = gpar(col = "darkgreen"))
  
  grid.text("Hotspots",
            x = unit(0.61, "npc"), y = unit(0.13, "npc"),
            just = "left", gp = gpar(cex = 1.8, fontface = "bold"))
  
  dev.off()
}

# ============================================================
# Cargar rasters
# ============================================================
mapa_subtropical <- raster("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_subtropical_helechos.tif")
mapa_presente <- raster("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_presente_helechos.tif")
mapa_mediterranea <- raster("C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_hotspots_mediterranea_helechos.tif")

# ============================================================
# Crear mapas originales
# ============================================================
crear_mapa(mapa_subtropical,"C:/Users/JorgeCV/Desktop/TFM/Resultados/original_subtropical_helechos.png")
crear_mapa(mapa_presente,"C:/Users/JorgeCV/Desktop/TFM/Resultados/original_presente_helechos.png")
crear_mapa(mapa_mediterranea,"C:/Users/JorgeCV/Desktop/TFM/Resultados/original_mediterranea_helechos.png")

# ============================================================
# Filtrar valores (7–9 especies)
# ============================================================
filtrar <- function(r){
  r2 <- r
  r2[!r2[] %in% 7:9] <- NA
  return(r2)
}

filtrado_subtropical <- filtrar(mapa_subtropical)
filtrado_presente <- filtrar(mapa_presente)
filtrado_mediterranea <- filtrar(mapa_mediterranea)

crear_mapa(filtrado_subtropical,"C:/Users/JorgeCV/Desktop/TFM/Resultados/filtrado_subtropical_helechos.png")
crear_mapa(filtrado_presente,"C:/Users/JorgeCV/Desktop/TFM/Resultados/filtrado_presente_helechos.png")
crear_mapa(filtrado_mediterranea,"C:/Users/JorgeCV/Desktop/TFM/Resultados/filtrado_mediterranea_helechos.png")

# ============================================================
# Función de superposición
# ============================================================
superponer_mapa <- function(original_png, filtrado_png, salida){
  
  img_original <- image_read(original_png)
  img_filtrado <- image_read(filtrado_png)
  
  info <- image_info(img_original)
  width <- info$width
  height <- info$height
  
  hist_top_ratio <- 0.28
  hist_right_ratio <- 0.14
  
  map_x <- 0
  map_y <- round(height * hist_top_ratio)
  map_width <- round(width * (1 - hist_right_ratio))
  map_height <- height - map_y
  
  map_recortado <- image_crop(img_original,
                              geometry = geometry_area(map_width, map_height, map_x, map_y))
  
  final <- image_composite(img_filtrado, map_recortado,
                           offset = paste0("+", map_x, "+", map_y))
  
  image_write(final, salida)
}

# ============================================================
# Crear mapas finales
# ============================================================
superponer_mapa("C:/Users/JorgeCV/Desktop/TFM/Resultados/original_subtropical_helechos.png",
                "C:/Users/JorgeCV/Desktop/TFM/Resultados/filtrado_subtropical_helechos.png",
                "C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_final_subtropical_helechos.png")

superponer_mapa("C:/Users/JorgeCV/Desktop/TFM/Resultados/original_presente_helechos.png",
                "C:/Users/JorgeCV/Desktop/TFM/Resultados/filtrado_presente_helechos.png",
                "C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_final_presente_helechos.png")

superponer_mapa("C:/Users/JorgeCV/Desktop/TFM/Resultados/original_mediterranea_helechos.png",
                "C:/Users/JorgeCV/Desktop/TFM/Resultados/filtrado_mediterranea_helechos.png",
                "C:/Users/JorgeCV/Desktop/TFM/Resultados/mapa_final_mediterranea_helechos.png")