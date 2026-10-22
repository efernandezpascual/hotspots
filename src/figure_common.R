# =============================================================================
# figure_common.R
# Shared constants, colour palette, ggplot themes and helper functions used by
# the figure scripts (06_fig1 ... 11_fig6) and by the supplementary documents
# (the *_supplementary.Rmd files in results/supplementary/).
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
#
# Purpose
#   Keeps all figures and supplementary documents visually and terminologically
#   consistent by defining everything that is shared in ONE place: species
#   lists, scenario labels, colours, text sizes, themes, and a few helper
#   functions. To change the look of the whole set, edit this file.
#
# Usage
#   This file is not run on its own. The scripts in src/ start with
#       source("src/figure_common.R")
#   and must be run from the project root (the folder that contains data/,
#   results/ and src/). The supplementary documents (results/supplementary/) are
#   knitted from their own folder, so they define
#       PROJECT_ROOT <- "../.."
#       source(file.path(PROJECT_ROOT, "src", "figure_common.R"),
#              local = knitr::knit_global())
#   All the paths defined below are relative to PROJECT_ROOT.
#
# Requires
#   R >= 4.1; ggplot2 (>= 3.5.0 recommended; older versions are supported); terra
#
# Style rules implemented here
#   * Text: sans-serif, 10 pt throughout. Panel titles are bold and are used
#     only in multi-panel figures. Maps have no axis titles (tick labels
#     already carry the units).
#   * Colours: green = native, gold = invasive; dark shade = hotspot, light
#     shade = presence area; grey = lost cells / whole region.
#   * Groups are always listed "Native" before "Invasive" (not alphabetically).
#   * Legends: inside the panel for maps, on top for bar charts and scatter
#     plots, on the right for the heatmap (colour bar).
#   * Frames: maps have a full black frame; charts have open (L-shaped) axes.
#     Axes, ticks and bar borders use LINE_WIDTH (0.3 mm, ggplot2 units); borders
#     of bars and tiles are black.
#   * Size: 170 mm wide (double column), 600 dpi.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(terra)
})

# -----------------------------------------------------------------------------
# 1. Directories (relative to the project root)
# -----------------------------------------------------------------------------
# PROJECT_ROOT is "." when a script is run from the project root; documents
# knitted from their own folder define it (e.g. "../..") before sourcing this file.
if (!exists("PROJECT_ROOT")) PROJECT_ROOT <- "."

DIR_SDM      <- file.path(PROJECT_ROOT, "results/SDMs")       # SDM outputs
DIR_HOTSPOTS <- file.path(PROJECT_ROOT, "results/hotspots")   # hotspot maps
DIR_FIGURES  <- file.path(PROJECT_ROOT, "results/figures")    # figures are written here
dir.create(DIR_FIGURES, showWarnings = FALSE, recursive = TRUE)

# Output formats written by save_figure(). PNG only by default: the point-based
# and raster figures produce very large vector files. For journal submission use
# e.g. c("png", "tiff") or c("png", "pdf").
FIG_FORMATS <- "png"

# -----------------------------------------------------------------------------
# 2. Species (names as used in the folder and file names of the SDM outputs)
# -----------------------------------------------------------------------------
NATIVE_SPECIES <- c(
  "Culcita_macrocarpa", "Davallia_canariensis", "Dryopteris_aemula",
  "Dryopteris_corleyi", "Dryopteris_guanchica", "Hymenophyllum_tunbrigense",
  "Leptogramma_pozoi", "Vandenboschia_speciosa", "Woodwardia_radicans"
)
INVASIVE_SPECIES <- c(
  "Crocosmia_crocosmiiflora", "Delairea_odorata", "Euonymus_japonicus",
  "Hedychium_gardnerianum", "Ligustrum_ovalifolium", "Phoenix_canariensis",
  "Potentilla_indica", "Trachycarpus_fortunei", "Tradescantia_fluminensis"
)
ALL_SPECIES <- c(NATIVE_SPECIES, INVASIVE_SPECIES)

# "Culcita_macrocarpa" -> "Culcita macrocarpa"  (name as in the data files;
# use this to match the occurrence records)
species_label <- function(x) gsub("_", " ", x)

# Names shown IN THE FIGURES when the manuscript uses a different nomenclature
# than the data files. Names of the vector = name in the data files (with
# underscore); values = name to display. Use c() if no overrides are needed.
DISPLAY_NAMES <- c(
  Leptogramma_pozoi      = "Thelypteris pozoi",
  Vandenboschia_speciosa = "Trichomanes speciosum"
)

# Name to display in a figure label (manuscript nomenclature)
species_display <- function(x) {
  out <- species_label(x)
  if (length(DISPLAY_NAMES) == 0) return(out)
  override <- DISPLAY_NAMES[x]
  has_override <- !is.na(override)
  out[has_override] <- unname(override[has_override])
  out
}

# Hotspot definition: cells where at least this many of the 9 species of a
# group are predicted present. Presence area = 1 to (HOTSPOT_THRESHOLD - 1).
# The hotspot maps in results/hotspots/ (05_hotspots.R) are built with this
# threshold; if it is changed, 05_hotspots.R must be run again.
HOTSPOT_THRESHOLD <- 7

# -----------------------------------------------------------------------------
# 3. Scenarios
# -----------------------------------------------------------------------------
# The keys are the scenario names used in the file names written by 04_SDMs.R
# and 05_hotspots.R; the values are the labels shown in figures and tables.
SCENARIO_LABELS <- c(
  present     = "Present",
  ssp126      = "SSP1-2.6",
  ssp370      = "SSP3-7.0",
  ssp585      = "SSP5-8.5",
  subtropical = "Subtropical"
)
FUTURE_KEYS <- setdiff(names(SCENARIO_LABELS), "present")

# Scenario keys -> ordered factor with the labels shown in the figures
scenario_factor <- function(key, levels_keys = names(SCENARIO_LABELS)) {
  factor(key, levels = levels_keys, labels = unname(SCENARIO_LABELS[levels_keys]))
}

# -----------------------------------------------------------------------------
# 4. Groups and colours
# -----------------------------------------------------------------------------
GROUP_LABELS <- c(native = "Native", invasive = "Invasive")

# Native/Invasive factor with the fixed order Native -> Invasive
group_factor <- function(is_native) {
  factor(ifelse(is_native, "native", "invasive"),
         levels = c("native", "invasive"),
         labels = unname(GROUP_LABELS[c("native", "invasive")]))
}

COL <- c(
  native_hotspot    = "darkgreen",
  native_presence   = "darkseagreen1",
  invasive_hotspot  = "goldenrod4",
  invasive_presence = "lightgoldenrod1",
  region            = "grey80",
  lost              = "grey30",
  decrease          = "#D55E00",   # heatmap: negative change (no named equivalent)
  increase          = "#0072B2"    # heatmap: positive change (no named equivalent)
)

# Dark shades identify the group when records are not split by hotspot status
GROUP_COLOURS <- setNames(
  unname(COL[c("native_hotspot", "invasive_hotspot")]),
  unname(GROUP_LABELS[c("native", "invasive")])
)

# -----------------------------------------------------------------------------
# 5. Themes
# -----------------------------------------------------------------------------
FONT_SIZE  <- 10
LINE_WIDTH <- 0.3

# Charts: bar charts, scatter plots, heatmap
theme_fig <- function(base_size = FONT_SIZE) {
  theme_classic(base_size = base_size, base_family = "sans") +
    theme(
      axis.line           = element_line(colour = "black", linewidth = LINE_WIDTH),
      axis.ticks          = element_line(colour = "black", linewidth = LINE_WIDTH),
      axis.text           = element_text(colour = "black", size = base_size),
      axis.title          = element_text(size = base_size),
      legend.position     = "top",
      legend.title        = element_text(size = base_size),
      legend.text         = element_text(size = base_size),
      legend.key.size     = grid::unit(3.5, "mm"),
      plot.title          = element_text(size = base_size, face = "bold"),
      plot.title.position = "plot",
      strip.text          = element_text(size = base_size, face = "bold"),
      strip.background    = element_rect(fill = "grey90", colour = "black",
                                         linewidth = LINE_WIDTH)
    )
}

# Maps: full black frame, no axis titles, legend inside the panel
# (legend.position = "inside" needs ggplot2 >= 3.5.0; with older versions the
# legend position is given as coordinates, which gives the same result)
theme_fig_map <- function(base_size = FONT_SIZE, legend_xy = c(0.88, 0.14)) {
  legend_position <- if (utils::packageVersion("ggplot2") >= "3.5.0") {
    theme(legend.position = "inside", legend.position.inside = legend_xy)
  } else {
    theme(legend.position = legend_xy)
  }
  theme_fig(base_size) +
    theme(
      axis.line        = element_blank(),
      axis.title       = element_blank(),
      panel.border     = element_rect(fill = NA, colour = "black",
                                      linewidth = LINE_WIDTH),
      legend.direction = "vertical",
      legend.title     = element_blank(),
      legend.margin    = margin(0, 0, 0, 0),
      plot.margin      = margin(2, 2, 2, 2)
    ) +
    legend_position
}

# -----------------------------------------------------------------------------
# 6. Helpers
# -----------------------------------------------------------------------------
# Path of the binary (presence/absence) map of one species and scenario.
# rep = NULL: consensus of the final-model replicates (used in the analyses);
# rep = 1, 2, ...: map of one replicate.
binary_path <- function(species, scenario, rep = NULL) {
  base <- file.path(DIR_SDM, species, "03_projections")
  if (!is.null(rep)) base <- file.path(base, paste0("rep", rep))
  file.path(base, "binary", paste0(species, "_", scenario, "_binary.tif"))
}

# Number of final-model replicates found on disk
n_replicates <- function(species = ALL_SPECIES[1]) {
  sum(grepl("^rep[0-9]+$", list.dirs(file.path(DIR_SDM, species, "03_projections"),
                                     recursive = FALSE, full.names = FALSE)))
}

# Path of the hotspot map (0/1) of one group ("native" or "invasive") and scenario
hotspot_path <- function(group, scenario) {
  file.path(DIR_HOTSPOTS, paste0("hotspot_", group, "_", scenario, ".tif"))
}

# Species richness map: number of species predicted present in each cell
# (rep: see binary_path)
load_richness <- function(species, scenario = "present", rep = NULL) {
  files   <- binary_path(species, scenario, rep)
  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    stop("Missing SDM binary maps:\n", paste(missing, collapse = "\n"))
  }
  sum(terra::rast(files), na.rm = TRUE)
}

# Metrics of the native hotspots (cells with at least n native species), computed with each
# final-model replicate separately (replicate r of all species), in every scenario and for
# every threshold n in `thresholds` (default: HOTSPOT_THRESHOLD). The changes with respect
# to the present are then paired: they are calculated within each replicate. Returns a data
# frame with columns:
#   threshold    n (minimum number of native species of a hotspot)
#   version      "rep1", "rep2", ...
#   scenario     scenario key
#   native_km2   area of the native hotspots (km2, true cell area)
#   lost_pct     % of the present-day native hotspot cells that are no longer hotspot
#                (NA in the present)
#   mean_inv     mean number of invasive species per cell of the native hotspots of the
#                scenario (the hotspots move between scenarios)
#   inv_fixed    mean number of invasive species in the PRESENT-DAY native hotspot cells
#                under the scenario (same cells in all scenarios)
#   novel_pct    % of the native hotspot cells of the scenario where some predictor is
#                outside its present-day range (NA in the present or if the extrapolation
#                maps of 04_SDMs.R are missing)
# native_species / invasive_species: species used (default: all); used by the
# sensitivity analysis that excludes the species with poorly validated models.
hotspot_metrics <- function(thresholds = HOTSPOT_THRESHOLD,
                            native_species = NATIVE_SPECIES,
                            invasive_species = INVASIVE_SPECIES) {

  scenarios <- names(SCENARIO_LABELS)
  versions  <- paste0("rep", seq_len(n_replicates()))

  template <- terra::rast(binary_path(NATIVE_SPECIES[1], "present"))
  cells    <- which(!is.na(terra::values(template)))
  area     <- as.numeric(terra::values(terra::cellSize(template, unit = "km"))[cells])

  richness <- function(species, scenario, rep) {
    as.numeric(terra::values(load_richness(species, scenario, rep))[cells])
  }

  # Cells where some predictor of the scenario is outside its present-day range
  # (results/extrapolation/novel_<scenario>.tif, written by 04_SDMs.R); NULL if missing
  novel <- lapply(setNames(scenarios, scenarios), function(s) {
    f <- file.path(PROJECT_ROOT, "results/extrapolation", paste0("novel_", s, ".tif"))
    if (s == "present" || !file.exists(f)) return(NULL)
    as.numeric(terra::values(terra::rast(f))[cells]) > 0
  })

  out <- list()
  for (v in versions) {
    rep <- as.integer(sub("rep", "", v))
    hot_present <- list()
    for (s in scenarios) {
      rich_native   <- richness(native_species, s, rep)
      rich_invasive <- richness(invasive_species, s, rep)
      for (n in thresholds) {
        hot <- rich_native >= n
        if (s == "present") hot_present[[as.character(n)]] <- hot
        hp <- hot_present[[as.character(n)]]
        out[[length(out) + 1]] <- data.frame(
          threshold  = n,
          version    = v,
          scenario   = s,
          # area of the native hotspots in the scenario
          native_km2 = sum(area[hot]),
          # % of the present-day hotspot cells that are no longer hotspot
          lost_pct   = if (s == "present") NA_real_ else 100 * mean(!hot[hp]),
          # mean number of invasive species per cell of the hotspots OF THE SCENARIO
          mean_inv   = mean(rich_invasive[hot]),
          # mean number of invasive species in the PRESENT-DAY hotspot cells under the
          # scenario (fixed cells: invasion of the current refugia)
          inv_fixed  = mean(rich_invasive[hp]),
          # % of the hotspot cells of the scenario with some predictor outside its
          # present-day range (extrapolation)
          novel_pct  = if (is.null(novel[[s]])) NA_real_ else 100 * mean(novel[[s]][hot], na.rm = TRUE),
          stringsAsFactors = FALSE)
      }
    }
  }
  do.call(rbind, out)
}

# Saves a figure in every format listed in `formats`, in DIR_FIGURES
save_figure <- function(plot, name, height_mm, width_mm = 170,
                        formats = FIG_FORMATS, dpi = 600) {
  for (fmt in formats) {
    file <- file.path(DIR_FIGURES, paste0(name, ".", fmt))
    ggplot2::ggsave(file, plot, width = width_mm, height = height_mm,
                    units = "mm", dpi = dpi, bg = "white")
    cat("Saved:", file, "\n")
  }
  invisible(NULL)
}
