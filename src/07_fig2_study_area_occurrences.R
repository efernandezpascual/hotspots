# =============================================================================
# Figure 2 - Study area and occurrence records
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
# Script     : 07_fig2_study_area_occurrences.R
#
# Purpose
#   Map of the Iberian Atlantic study region over a grey elevation background,
#   showing the occurrence records of the nine native ferns and the nine
#   invasive species, with an inset locating the region within Europe.
#
# Inputs
#   data/species-points.csv           columns: species, longitude, latitude (WGS84)
#   data/shape/IberoAtlantic.shp      study region polygon (https://doi.org/10.1111/avsc.12793)
#   data/raster/altitud.tif           elevation raster (produced in the GIC Rewilding
#                                     project, https://doi.org/10.1016/j.ecolind.2025.114532)
#
# Output
#   results/figures/fig2_study_area.png   (170 x 100 mm, 600 dpi)
#
# Notes
#   * Dark green / dark gold identify the two groups (same hues as the other
#     figures; records are not split into presence/hotspot classes here).
#   * The inset position is given as fractions of the figure canvas (INSET);
#     adjust it if the figure height is changed.
#
# Requires
#   sf, terra, tidyterra, ggnewscale, dplyr, ggplot2, cowplot, rnaturalearth,
#   rnaturalearthdata; run from the project root.
# =============================================================================

source("src/figure_common.R")

suppressPackageStartupMessages({
  library(sf)
  library(tidyterra)
  library(ggnewscale)
  library(dplyr)
  library(cowplot)
  library(rnaturalearth)
})

# ---- Settings ---------------------------------------------------------------
PATH_OCCURRENCES <- "data/species-points.csv"
PATH_REGION      <- "data/shape/IberoAtlantic.shp"
PATH_ELEVATION   <- "data/raster/altitud.tif"

FIG_HEIGHT_MM <- 100
INSET <- list(x = 0.475, y = 0.15, width = 0.45, height = 0.45)

# ---- 1. Data ----------------------------------------------------------------
region <- st_read(PATH_REGION, quiet = TRUE) %>% st_transform(4326)
bb     <- st_bbox(region)

elevation <- rast(PATH_ELEVATION)
if (crs(elevation) != st_crs(region)$wkt) {
  elevation <- project(elevation, "EPSG:4326")
}
elevation <- mask(crop(elevation, vect(region)), vect(region))

occurrences <- read.csv(PATH_OCCURRENCES) %>%
  mutate(group = group_factor(species %in% species_label(NATIVE_SPECIES)))

# ---- 2. Main panel ----------------------------------------------------------
main_panel <- ggplot() +
  geom_spatraster(data = elevation, show.legend = FALSE) +
  scale_fill_gradient(low = "grey90", high = "grey10", na.value = "transparent") +
  geom_sf(data = region, fill = NA, colour = "black", linewidth = 0.5) +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"])) +
  ggnewscale::new_scale_fill() +
  geom_point(data = occurrences, aes(x = longitude, y = latitude, fill = group, shape = group),
             size = 1.6, colour = "black", stroke = 0.15, alpha = 0.85) +
  scale_fill_manual(values = GROUP_COLOURS, name = NULL) +
  scale_shape_manual(values = c(21, 24), name = NULL) +
  guides(
    fill  = guide_legend(override.aes = list(shape = c(21, 24), alpha = 1, size = 2.5)),
    shape = "none"
  ) +
  theme_fig_map()

# ---- 3. Europe inset --------------------------------------------------------
europe <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")

inset <- ggplot() +
  geom_sf(data = europe, fill = "grey90", colour = "grey65", linewidth = 0.2) +
  geom_sf(data = region, fill = "grey40", colour = "black", linewidth = 0.3) +
  coord_sf(xlim = c(-12, 20), ylim = c(35, 60), expand = FALSE) +
  theme_void() +
  theme(
    panel.border    = element_rect(fill = NA, colour = "black", linewidth = 0.4),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# ---- 4. Combine and save ----------------------------------------------------
fig2 <- ggdraw() +
  draw_plot(main_panel) +
  draw_plot(inset, x = INSET$x, y = INSET$y, width = INSET$width, height = INSET$height)

save_figure(fig2, "fig2_study_area", height_mm = FIG_HEIGHT_MM)
