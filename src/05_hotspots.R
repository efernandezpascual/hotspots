# =============================================================================
# Step 5 - Native and invasive hotspot maps
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
# Script     : 05_hotspots.R
#
# Purpose
#   Builds, for each climate scenario, the species richness maps of the nine
#   native ferns and the nine invasive species (sum of the binary maps written
#   by 04_SDMs.R) and saves the hotspot maps: cells where at least
#   HOTSPOT_THRESHOLD species of the group are predicted present.
#
#   The metrics of the native hotspots (area, hotspot cells lost, invasive species in
#   the hotspot cells) are computed with each replicate in the supplementary document
#   results/supplementary/S3_hotspots_supplementary.Rmd (Supporting Information S3) and in
#   11_fig6_hotspot_scenarios.R (hotspot_metrics() in figure_common.R). The maps saved
#   here are the consensus hotspot maps.
#
# Inputs
#   results/SDMs/<species>/03_projections/binary/<species>_<scenario>_binary.tif
#       binary maps written by 04_SDMs.R (18 species x 5 scenarios)
#
# Outputs (results/hotspots/)
#   hotspot_native_<scenario>.tif     0/1 map of native hotspots   (5 files)
#   hotspot_invasive_<scenario>.tif   0/1 map of invasive hotspots (5 files)
#   <scenario> = present, ssp126, ssp370, ssp585, subtropical
#
# Requires
#   terra; run from the project root.
# =============================================================================

source("src/figure_common.R")   # species lists, scenarios, HOTSPOT_THRESHOLD

dir.create(DIR_HOTSPOTS, showWarnings = FALSE, recursive = TRUE)

for (scenario in names(SCENARIO_LABELS)) {

  cat("Scenario:", scenario, "\n")

  richness_native   <- load_richness(NATIVE_SPECIES, scenario)
  richness_invasive <- load_richness(INVASIVE_SPECIES, scenario)

  writeRaster(richness_native >= HOTSPOT_THRESHOLD,
              hotspot_path("native", scenario), overwrite = TRUE)
  writeRaster(richness_invasive >= HOTSPOT_THRESHOLD,
              hotspot_path("invasive", scenario), overwrite = TRUE)
}

cat("\nHotspot maps (threshold =", HOTSPOT_THRESHOLD, "species) saved in",
    DIR_HOTSPOTS, "\n")
