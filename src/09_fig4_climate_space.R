# =============================================================================
# Figure 4 - Climatic space occupied by native and invasive species
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
# Script     : 09_fig4_climate_space.R
#
# Purpose
#   Scatter plot of the climatic space: BIO6 (minimum temperature of the coldest
#   month) against BIO14 (precipitation of the driest month). Every map cell is
#   one point. Layers, from background to foreground (drawing order = row order
#   of the data):
#       1. whole region (grey)           - climate space available
#       2. invasive presence area
#       3. native presence area
#       4. invasive hotspot
#       5. native hotspot                - drawn last, visible on top
#   Presence area = 1 to (HOTSPOT_THRESHOLD - 1) species; hotspot = at least
#   HOTSPOT_THRESHOLD species (predicted by the species distribution models,
#   present climate). All cells are used (no subsampling).
#
#   Soil pH is a model predictor but is not shown: this figure describes the
#   climatic space only. All points have the same size; visual hierarchy comes
#   from transparency only.
#
# Inputs
#   data/raster/present/bio06_present.tif, bio14_present.tif
#                                     present-day climate layers
#   results/SDMs/<species>/03_projections/binary/<species>_<scenario>_binary.tif
#                                     binary maps written by 04_SDMs.R
#
# Output
#   results/figures/fig4_climate_space.png   (170 x 140 mm, 600 dpi)
#
# Requires
#   terra, dplyr, ggplot2; run from the project root.
# =============================================================================

source("src/figure_common.R")

suppressPackageStartupMessages(library(dplyr))

# ---- Settings ---------------------------------------------------------------
DIR_CLIMATE   <- "data/raster/present"
FIG_HEIGHT_MM <- 140

POINT_SIZE     <- 2
ALPHA_REGION   <- 0.08
ALPHA_PRESENCE <- 0.20
ALPHA_HOTSPOT  <- 0.4

# Legend order and labels (keys are also the names of the colours in COL)
LEGEND_KEYS   <- c("region", "native_presence", "native_hotspot",
                   "invasive_presence", "invasive_hotspot")
LEGEND_LABELS <- c("Region (all cells)", "Native presence area", "Native hotspot",
                   "Invasive presence area", "Invasive hotspot")

# ---- 1. Climate layers and species richness ---------------------------------
climate <- rast(file.path(DIR_CLIMATE, c("bio06_present.tif", "bio14_present.tif")))
names(climate) <- c("bio06", "bio14")

richness_native   <- load_richness(NATIVE_SPECIES, "present")
richness_invasive <- load_richness(INVASIVE_SPECIES, "present")

# ---- 2. Climate values of the cells in each layer ---------------------------
# values() is indexed by cell number; as.data.frame() would drop NA rows and
# shift the indices.
extract_layer <- function(cell_mask, key) {
  cells <- which(values(cell_mask) == 1)
  vals  <- as.data.frame(values(climate)[cells, , drop = FALSE])
  vals$layer <- key
  vals
}

# Row order = drawing order
points_data <- bind_rows(
  extract_layer(!is.na(richness_native), "region"),
  extract_layer(richness_invasive >= 1 & richness_invasive < HOTSPOT_THRESHOLD, "invasive_presence"),
  extract_layer(richness_native >= 1 & richness_native < HOTSPOT_THRESHOLD, "native_presence"),
  extract_layer(richness_invasive >= HOTSPOT_THRESHOLD, "invasive_hotspot"),
  extract_layer(richness_native >= HOTSPOT_THRESHOLD, "native_hotspot")
) %>%
  filter(complete.cases(.))

points_data$layer <- factor(points_data$layer, levels = LEGEND_KEYS)
points_data$point_alpha <- case_when(
  points_data$layer == "region"             ~ ALPHA_REGION,
  grepl("hotspot", points_data$layer)       ~ ALPHA_HOTSPOT,
  TRUE                                      ~ ALPHA_PRESENCE
)

cat("Cells per layer:\n")
print(table(points_data$layer))

# ---- 3. Figure --------------------------------------------------------------
fig4 <- ggplot(points_data, aes(x = bio06, y = bio14, colour = layer)) +
  geom_point(aes(alpha = point_alpha), size = POINT_SIZE) +
  scale_alpha_identity() +
  scale_colour_manual(values = COL[LEGEND_KEYS], breaks = LEGEND_KEYS,
                      labels = LEGEND_LABELS, name = NULL) +
  labs(
    x = "BIO6 - Min. temperature of coldest month (\u00b0C)",
    y = "BIO14 - Precipitation of driest month (mm)"
  ) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(alpha = 1, size = 2.5))) +
  theme_fig()

save_figure(fig4, "fig4_climate_space", height_mm = FIG_HEIGHT_MM)
