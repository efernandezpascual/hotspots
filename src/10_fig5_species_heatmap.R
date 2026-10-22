# =============================================================================
# Figure 5 - Change in suitable area per species under future scenarios
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
# Script     : 10_fig5_species_heatmap.R
#
# Purpose
#   Heatmap of the percentage change in the area predicted suitable for each of
#   the 18 species under the four future scenarios, relative to the present.
#   Rows are grouped into native and invasive species; the present-day area of
#   each species is shown next to its name. The colour scale is diverging and
#   centred on zero.
#
# Inputs
#   results/SDMs/<species>/03_projections/binary/<species>_<scenario>_binary.tif
#                                     binary maps written by 04_SDMs.R
#
# Output
#   results/figures/fig5_species_heatmap.png   (170 x 180 mm, 600 dpi)
#
# Notes
#   * Areas are computed from the true cell area (terra::cellSize), which
#     varies with latitude in a geographic (lon/lat) grid.
#   * The colour scale is capped at +/- COLOUR_CAP percent: larger changes are
#     drawn with the extreme colour but keep their true value in the tile label.
#     This prevents a few species with very small present-day ranges, whose
#     percentage change can be extreme, from compressing the scale for the rest.
#
# Requires
#   terra, dplyr, ggplot2, scales; run from the project root.
# =============================================================================

source("src/figure_common.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(scales)
})

# ---- Settings ---------------------------------------------------------------
FIG_HEIGHT_MM <- 180
COLOUR_CAP    <- 100   # percent; see Notes
TILE_TEXT_MM  <- 3     # size of the percentage labels inside the tiles

# ---- 1. Suitable area per species and scenario ------------------------------
species_area_km2 <- function(species, scenario_key) {
  file <- binary_path(species, scenario_key)
  if (!file.exists(file)) {
    warning("Missing file: ", file)
    return(NA_real_)
  }
  r <- rast(file)
  as.numeric(global(cellSize(r, unit = "km") * (r == 1), "sum", na.rm = TRUE)[1, 1])
}

areas <- expand.grid(species = ALL_SPECIES, scenario = names(SCENARIO_LABELS),
                     stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(area_km2 = species_area_km2(species, scenario)) %>%
  ungroup()

present_area <- areas %>%
  filter(scenario == "present") %>%
  select(species, present_km2 = area_km2)

heat <- areas %>%
  filter(scenario != "present") %>%
  left_join(present_area, by = "species") %>%
  mutate(pct_change = round(100 * (area_km2 - present_km2) / present_km2, 1))

cat("Summary of the percentage change (use it to choose COLOUR_CAP):\n")
print(summary(heat$pct_change))

# ---- 2. Factors, groups and row labels --------------------------------------
heat$species  <- factor(heat$species, levels = rev(ALL_SPECIES))   # first species on top
heat$group    <- group_factor(as.character(heat$species) %in% NATIVE_SPECIES)
heat$scenario <- scenario_factor(heat$scenario, FUTURE_KEYS)

# Row label: species name in italics, present-day area in plain text
present_lookup <- setNames(present_area$present_km2, present_area$species)

row_label <- function(species) {
  area <- formatC(round(present_lookup[[species]]), format = "d", big.mark = ",")
  sprintf('italic("%s")~"(%s km"^2*")"', species_display(species), area)
}

# ---- 3. Figure --------------------------------------------------------------
fig5 <- ggplot(heat, aes(x = scenario, y = species, fill = pct_change)) +
  geom_tile(colour = "black", linewidth = 0.2) +
  geom_text(aes(label = sprintf("%+.0f%%", pct_change)), size = TILE_TEXT_MM, colour = "black") +
  scale_fill_gradient2(
    low = COL[["decrease"]], mid = "white", high = COL[["increase"]], midpoint = 0,
    limits = c(-COLOUR_CAP, COLOUR_CAP), oob = scales::squish,
    name = sprintf("%% change\nvs. present\n(scale capped\nat \u00b1%d%%)", COLOUR_CAP)
  ) +
  scale_y_discrete(labels = function(breaks) {
    parse(text = vapply(breaks, row_label, character(1)))
  }) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_fig() +
  theme(
    axis.text.x        = element_text(angle = 30, hjust = 1),
    axis.ticks.y       = element_blank(),
    axis.ticks.x       = element_blank(),
    legend.position    = "right",
    legend.key.height  = grid::unit(15, "mm"),
    panel.spacing      = grid::unit(4, "mm")
  )

save_figure(fig5, "fig5_species_heatmap", height_mm = FIG_HEIGHT_MM)
