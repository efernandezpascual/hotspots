# =============================================================================
# Figure 3 - Modelled distribution under present climate
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
# Script     : 08_fig3_model_maps.R
#
# Purpose
#   Two stacked maps for the present climate: (a) native ferns and
#   (b) invasive species. Cells are classified by the number of species
#   (out of nine per group) predicted present by the species distribution
#   models:
#       presence area : 1 to (HOTSPOT_THRESHOLD - 1) species
#       hotspot       : at least HOTSPOT_THRESHOLD species
#   Cells with no predicted species are left blank.
#
# Inputs
#   results/SDMs/<species>/03_projections/binary/<species>_<scenario>_binary.tif
#                                     binary maps written by 04_SDMs.R
#   data/shape/IberoAtlantic.shp      study region polygon
#
# Output
#   results/figures/fig3_model_maps.png   (170 x 180 mm, 600 dpi)
#
# Requires
#   terra, tidyterra, sf, ggplot2, patchwork; run from the project root.
# =============================================================================

source("src/figure_common.R")

suppressPackageStartupMessages({
  library(tidyterra)
  library(sf)
  library(dplyr)
  library(patchwork)
})

# ---- Settings ---------------------------------------------------------------
PATH_REGION   <- "data/shape/IberoAtlantic.shp"
FIG_HEIGHT_MM <- 180

# ---- 1. Data ----------------------------------------------------------------
region <- st_read(PATH_REGION, quiet = TRUE) %>% st_transform(4326)
bb     <- st_bbox(region)

richness_native   <- load_richness(NATIVE_SPECIES, "present")
richness_invasive <- load_richness(INVASIVE_SPECIES, "present")

# 1 = presence area, 2 = hotspot, NA = no species predicted
classify_richness <- function(richness) {
  classes <- richness
  classes[richness == 0] <- NA
  classes[richness >= 1 & richness < HOTSPOT_THRESHOLD] <- 1
  classes[richness >= HOTSPOT_THRESHOLD] <- 2
  classes
}

native_classes   <- classify_richness(richness_native)
invasive_classes <- classify_richness(richness_invasive)

# Reproject only if needed; nearest neighbour keeps the class codes intact
if (crs(native_classes) != st_crs(region)$wkt) {
  native_classes   <- project(native_classes, "EPSG:4326", method = "near")
  invasive_classes <- project(invasive_classes, "EPSG:4326", method = "near")
}

# ---- 2. Panel builder (used for both groups) --------------------------------
build_panel <- function(classes, col_presence, col_hotspot, title) {
  classes_f <- as.factor(classes)
  levels(classes_f) <- data.frame(id = 1:2, class = c("Presence area", "Hotspot"))

  ggplot() +
    geom_spatraster(data = classes_f) +
    scale_fill_manual(
      values = c("Presence area" = col_presence, "Hotspot" = col_hotspot),
      na.value = "transparent", na.translate = FALSE, name = NULL
    ) +
    geom_sf(data = region, fill = NA, colour = "black", linewidth = 0.4) +
    coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"])) +
    ggtitle(title) +
    theme_fig_map()
}

panel_a <- build_panel(native_classes, COL[["native_presence"]], COL[["native_hotspot"]],
                       paste0("(a) ", GROUP_LABELS[["native"]]))
panel_b <- build_panel(invasive_classes, COL[["invasive_presence"]], COL[["invasive_hotspot"]],
                       paste0("(b) ", GROUP_LABELS[["invasive"]]))

# ---- 3. Combine and save ----------------------------------------------------
fig3 <- panel_a / panel_b

save_figure(fig3, "fig3_model_maps", height_mm = FIG_HEIGHT_MM)
