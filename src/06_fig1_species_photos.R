# =============================================================================
# Figure 1 - Photographs of the nine native fern species
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
# Script     : 06_fig1_species_photos.R
#
# Purpose
#   Identification plate (3 x 3) of the nine native fern taxa analysed in the
#   study. Each photograph is centre-cropped to a square and labelled with the
#   species name in italics. Names follow the manuscript nomenclature
#   (DISPLAY_NAMES in figure_common.R).
#
# Inputs
#   data/photos/<Genus_species>.jpg   one photograph per native species, named
#                                     exactly as in NATIVE_SPECIES (see
#                                     figure_common.R); .jpg/.jpeg/.png/.tif
#                                     are accepted. Save photographs with the
#                                     correct orientation.
#
# Output
#   results/figures/fig1_species_photos.png   (170 x 185 mm, 600 dpi)
#
# Notes
#   * A missing photograph is replaced by a grey placeholder and a warning.
#   * Spacing between rows is set by LABEL_HEIGHT_MM and PAD_MM below.
#
# Requires
#   ggplot2, patchwork, magick; run from the project root.
# =============================================================================

source("src/figure_common.R")

suppressPackageStartupMessages({
  library(patchwork)
  library(magick)
})

# ---- Settings ---------------------------------------------------------------
PHOTO_DIR <- "data/photos"

N_COL           <- 3
FIG_WIDTH_MM    <- 170
LABEL_HEIGHT_MM <- 5      # space under each photograph for the (one-line) name
PAD_MM          <- 1.5    # white margin around each photograph

cell_w     <- FIG_WIDTH_MM / N_COL
cell_h     <- cell_w + LABEL_HEIGHT_MM
n_row      <- ceiling(length(NATIVE_SPECIES) / N_COL)
fig_height <- n_row * cell_h
text_mm    <- FONT_SIZE / ggplot2::.pt   # geom text size (mm) for FONT_SIZE pt

# ---- 1. Photograph helpers --------------------------------------------------
find_photo <- function(species) {
  pattern <- paste0("^", species, "\\.(jpe?g|png|tiff?)$")
  hits <- list.files(PHOTO_DIR, pattern = pattern, ignore.case = TRUE, full.names = TRUE)
  if (length(hits) > 0) hits[1] else NA_character_
}

# Centre-crop to a square and downsize (keeps the figure file light)
load_square_photo <- function(path, size_px = 1200) {
  img  <- image_read(path)
  info <- image_info(img)
  w <- info$width[1]
  h <- info$height[1]
  side <- min(w, h)
  crop <- geometry_area(side, side, floor((w - side) / 2), floor((h - side) / 2))
  img  <- image_crop(img, crop)
  image_resize(img, paste0(size_px, "x", size_px))
}

# Plotmath label: species name in italics (manuscript nomenclature)
panel_label <- function(species) {
  sprintf('italic("%s")', species_display(species))
}

# ---- 2. One panel per species -----------------------------------------------
# Panels are drawn in millimetres so that photographs are never distorted.
make_panel <- function(species) {
  side <- cell_w - 2 * PAD_MM
  x0 <- PAD_MM
  x1 <- PAD_MM + side
  y1 <- cell_h - PAD_MM
  y0 <- y1 - side

  p <- ggplot() +
    coord_cartesian(xlim = c(0, cell_w), ylim = c(0, cell_h), expand = FALSE) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))

  path <- find_photo(species)
  if (!is.na(path)) {
    p <- p + annotation_raster(as.raster(load_square_photo(path)),
                               xmin = x0, xmax = x1, ymin = y0, ymax = y1,
                               interpolate = TRUE)
  } else {
    warning("No photograph found for ", species, " in ", PHOTO_DIR)
    p <- p +
      annotate("rect", xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = "grey90") +
      annotate("text", x = (x0 + x1) / 2, y = (y0 + y1) / 2,
               label = "Photo missing", size = text_mm)
  }

  p + annotate("text", x = cell_w / 2, y = y0 / 2, label = panel_label(species),
               parse = TRUE, size = text_mm)
}

# ---- 3. Assemble and save ---------------------------------------------------
panels <- lapply(NATIVE_SPECIES, make_panel)
# Each panel already has zero margin (see make_panel); plot_annotation() removes
# the margin around the whole plate.
fig1   <- wrap_plots(panels, ncol = N_COL) +
  plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))

save_figure(fig1, "fig1_species_photos", height_mm = fig_height, width_mm = FIG_WIDTH_MM)
