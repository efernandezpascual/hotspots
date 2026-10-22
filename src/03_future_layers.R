# =============================================================================
# Step 3 - Future climate layers
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
# Script     : 03_future_layers.R
#
# Purpose
#   Prepares the future BIO6, BIO10 and BIO14 layers (2071-2100) for the scenarios
#   SSP1-2.6, SSP3-7.0 and SSP5-8.5: for each scenario and variable, the layers
#   of four global climate models (GCMs) are read in raw mode, cropped to the
#   study region, converted to real units, averaged (ensemble mean) and masked
#   to the exact region polygon.
#   The climatic predictors of the models are BIO6 and BIO14 (soil pH is kept
#   constant in the future scenarios). BIO10 is also prepared, although it is not a
#   predictor: it was evaluated as a proxy of summer moisture demand and excluded
#   because its future values fall outside the present-day range in most of the
#   region; its layers are needed to report that extrapolation (Supporting
#   Information S1, variable_selection_common.R). The other candidate
#   variables were discarded in the variable selection (see Supporting
#   Information S1).
#   The mean of each variable over the study region is also saved, for each GCM,
#   for the ensemble and for the present, to document the scenarios.
#
# Input layers (NOT included in this repository)
#   The original global layers are public and very large, so they are not
#   stored in the repository. They were downloaded on 2026-09-18 into
#   data/raster/download/future/ (a local folder excluded from version control):
#     CHELSA_<gcm>_<ssp>_<variable>_2071-2100_V.2.1.tif
#     e.g. CHELSA_gfdl-esm4_ssp126_bio06_2071-2100_V.2.1.tif
#   CHELSA v2.1 (Karger et al. 2017, https://doi.org/10.1038/sdata.2017.122),
#   ISIMIP3b GCMs: gfdl-esm4, ipsl-cm6a-lr, mpi-esm1-2-hr, mri-esm2-0,
#   ukesm1-0-ll. See the section "Data" of the README in the root folder for details.
#
# Other input
#   data/shape/IberoAtlantic.shp      study region polygon
#
# Outputs
#   data/raster/future/<variable>_<ssp>_2071-2100.tif
#       9 files (bio06, bio10, bio14 x ssp126, ssp370, ssp585), ensemble mean in
#       real units
#   results/future_climate/regional_means.csv
#       mean over the study region of each variable: present, each GCM and the
#       ensemble of each scenario (columns: variable, scenario, gcm, mean)
#
# GCM exclusion
#   mpi-esm1-2-hr is excluded from ALL combinations, so that the three scenarios
#   are built from the same four GCMs. Reason: under bio06/ssp585 its mean
#   (2.32 degrees C) was lower than under ssp126 (2.45 degrees C) and well below
#   ssp370 (4.12 degrees C) (means over the crop rectangle), which contradicts the expected increase of warming
#   with forcing. The file was checked (normal size, no spatial artefacts
#   visible in a GIS) and no download problem was found; the anomaly is
#   interpreted as a model-specific downscaling artefact, a possibility
#   acknowledged in the CHELSA technical specification.
#
# Requires
#   terra, sf; run from the project root.
# =============================================================================

library(terra)
library(sf)

# ---- Settings ---------------------------------------------------------------
PATHS <- list(
  layers_dir   = "data/raster/download/future",
  region_shape = "data/shape/IberoAtlantic.shp",
  output_dir   = "data/raster/future",
  present_dir  = "data/raster/present",   # present layers (01_present_layers.R)
  summary_file = "results/future_climate/regional_means.csv"
)

dir.create(PATHS$output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(PATHS$summary_file), showWarnings = FALSE, recursive = TRUE)

margin_deg <- 0.1   # crop margin around the region (same as for the present layers)

# Scale / offset (same values as for the present-day layers, applied to RAW values)
bio_table <- data.frame(
  var    = c("bio06", "bio10", "bio14"),
  scale  = c(0.1, 0.1, 0.1),
  offset = c(-273.15, -273.15, 0)
)

gcms      <- c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll")
ssps      <- c("ssp126", "ssp370", "ssp585")
variables <- c("bio06", "bio10", "bio14")

excluded_gcm <- "mpi-esm1-2-hr"   # see "GCM exclusion" above
gcms <- setdiff(gcms, excluded_gcm)

cat("GCM excluded from ALL combinations:", excluded_gcm,
    "- final ensemble of", length(gcms), "GCMs:",
    paste(gcms, collapse = ", "), "\n")

# ---- 1. Region polygon and crop rectangle (with margin) ---------------------
region <- st_read(PATHS$region_shape)

region_ext <- ext(vect(region))
crop_ext <- ext(
  region_ext$xmin - margin_deg,
  region_ext$xmax + margin_deg,
  region_ext$ymin - margin_deg,
  region_ext$ymax + margin_deg
)

cat("Crop rectangle (with margin):\n")
print(crop_ext)

# ---- Read one GCM layer in raw mode, crop and convert -----------------------
read_crop_convert <- function(file, var_name, table, bbox) {
  ref <- table[table$var == var_name, ]
  r_raw <- rast(file, raw = TRUE)
  r_raw <- crop(r_raw, bbox)
  r_conv <- r_raw * ref$scale + ref$offset
  names(r_conv) <- var_name
  r_conv
}

# ---- 2. For each scenario and variable: read the GCMs, convert, average -----
# Region polygon in the CRS of the CHELSA layers (used to mask and to compute the
# regional means over the study region only, not over the crop rectangle)
first_file <- file.path(PATHS$layers_dir,
                        sprintf("CHELSA_%s_%s_%s_2071-2100_V.2.1.tif",
                                gcms[1], ssps[1], variables[1]))
if (!file.exists(first_file)) stop("Missing file: ", first_file)
region_vect <- vect(st_transform(region, crs(rast(first_file))))

# Mean of each layer over the study region
regional_mean <- function(r) {
  as.numeric(global(mask(r, region_vect), "mean", na.rm = TRUE)$mean)
}

ensembles    <- list()
summary_rows <- list()

for (ssp in ssps) {
  for (v in variables) {

    gcm_files <- file.path(
      PATHS$layers_dir,
      sprintf("CHELSA_%s_%s_%s_2071-2100_V.2.1.tif", gcms, ssp, v)
    )

    missing_files <- gcm_files[!file.exists(gcm_files)]
    if (length(missing_files) > 0) {
      stop("Missing files for ", v, " / ", ssp, ":\n",
           paste(missing_files, collapse = "\n"))
    }

    cat(sprintf("\n[%s | %s] Reading %d GCMs (%s)...\n", v, ssp,
                length(gcms), paste(gcms, collapse = ", ")))

    gcm_layers <- lapply(gcm_files, read_crop_convert,
                         var_name = v, table = bio_table, bbox = crop_ext)
    gcm_layers <- rast(gcm_layers)   # one layer per GCM

    # Quick plausibility check per GCM before averaging. With only four values
    # the 5 * sd threshold has low sensitivity: it is an additional check, not a
    # reliable filter, so the printed means should also be inspected.
    gcm_means <- round(regional_mean(gcm_layers), 2)
    cat("  Regional means per GCM (", paste(gcms, collapse = ", "), "):\n  ",
        paste(gcm_means, collapse = ", "), "\n")
    if (max(gcm_means) - min(gcm_means) > 5 * sd(gcm_means)) {
      cat("  WARNING: unusual spread among GCMs for", v, ssp,
          "- check before accepting the ensemble.\n")
    }

    # Ensemble: mean of the retained GCMs
    ensemble <- mean(gcm_layers)
    names(ensemble) <- v

    ensembles[[paste0(v, "_", ssp)]] <- ensemble

    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      variable = v, scenario = ssp, gcm = c(gcms, "ensemble"),
      mean = c(gcm_means, round(regional_mean(ensemble), 2)))
  }
}

# ---- 3. Mask to the exact region polygon ------------------------------------
ensembles <- lapply(ensembles, function(r) mask(r, region_vect))

# ---- 4. Regional means -------------------------------------------------------
cat("\nGCM excluded from ALL combinations:",
    excluded_gcm, "\n",
    "Final ensemble: mean of", length(gcms), "GCMs (",
    paste(gcms, collapse = ", "), ") for the", length(ssps), "scenarios x",
    length(variables), "variables.\n")

# Regional means of the present-day layers, for comparison with the scenarios
present_rows <- lapply(variables, function(v) {
  f <- file.path(PATHS$present_dir, paste0(v, "_present.tif"))
  if (!file.exists(f)) stop("Missing present layer: ", f, " (run 01_present_layers.R)")
  data.frame(variable = v, scenario = "present", gcm = "observed",
             mean = round(as.numeric(global(rast(f), "mean", na.rm = TRUE)$mean), 2))
})

regional_means <- do.call(rbind, c(present_rows, summary_rows))
write.csv(regional_means, PATHS$summary_file, row.names = FALSE)

cat("\nRegional means (present, each GCM and ensemble):\n")
print(reshape(regional_means[regional_means$gcm %in% c("observed", "ensemble"), ],
              idvar = "variable", timevar = "scenario",
              direction = "wide", drop = "gcm"), row.names = FALSE)
cat("Saved in:", PATHS$summary_file, "\n")

# ---- 5. Save ----------------------------------------------------------------
for (key in names(ensembles)) {
  writeRaster(
    ensembles[[key]],
    filename = file.path(PATHS$output_dir, paste0(key, "_2071-2100.tif")),
    overwrite = TRUE
  )
}

cat("\nFuture layers (ensembled, cropped, converted) saved in:",
    PATHS$output_dir, "\n")
