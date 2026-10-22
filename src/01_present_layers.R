# =============================================================================
# Step 1 - Present-day climatic and edaphic layers
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
# Script     : 01_present_layers.R
#
# Purpose
#   Prepares the 26 candidate predictor layers (19 CHELSA bioclimatic variables,
#   pH and sand content from SoilGrids, and five CHELSA-BIOCLIM+ variables):
#   crop to the study region, convert to real units, resample to a common grid
#   and mask to the exact region polygon.
#
# Input layers (NOT included in this repository)
#   The original global layers are public and very large, so they are not
#   stored in the repository. They were downloaded on 2026-09-18 into
#   data/raster/download/ (a local folder excluded from version control):
#     CHELSA_bio*.tif      19 files, CHELSA v2.1 bioclimatic variables,
#                          1981-2010 (Karger et al. 2017,
#                          https://doi.org/10.1038/sdata.2017.122)
#     CHELSA_clt*.tif, CHELSA_cmi*.tif, CHELSA_hurs*.tif, CHELSA_vpd*.tif,
#     CHELSA_fcf*.tif      CHELSA-BIOCLIM+, 1981-2010 (Brun et al. 2022,
#                          https://doi.org/10.5194/essd-14-5573-2022)
#     SOILGRID_pH*.tif, SOILGRID_sand*.tif
#                          SoilGrids 2.0 (ISRIC), 0-5 cm depth
#   See the section "Data" of the README in the root folder for details.
#
# Other input
#   data/shape/IberoAtlantic.shp      study region polygon
#
# Outputs (data/raster/present/)
#   <variable>_present.tif   26 files: bio01-bio19, ph, sand, clt, cmi, hurs,
#                            vpd, fcf (real units, cropped to the region)
#
# Method notes
#   * Layers are read in raw mode (rast(..., raw = TRUE)): the scale and offset
#     metadata embedded in the GeoTIFF files are ignored because they are not
#     propagated consistently by crop()/resample()/mask(). Conversion to real
#     units is applied explicitly with the tables below.
#   * Each layer is cropped to the region (plus a margin) BEFORE the conversion,
#     which is much faster than converting the global rasters.
#   * Scale/offset factors: bio01-bio19 follow the standard CHELSA convention,
#     except bio12 (scale 1; gives a regional mean of ~1458 mm/year, consistent
#     with known values: Santiago ~1800, Bilbao ~1200, San Sebastian ~1400
#     mm/year) and bio03 (recomputed as 100 * bio02 / bio07). Factors for bio04
#     and bio15 follow the standard convention and were not checked
#     independently. Factors for clt, hurs, vpd, cmi and fcf are those of the
#     CHELSA-BIOCLIM+ technical specification.
#   * clt, cmi, hurs, vpd and fcf are used only in the exploratory variable
#     selection (S1_variable_selection_supplementary.Rmd), not in the models.
#   * Soil no-data cells. In the SoilGrids files, cells without a soil estimate
#     (urban areas, water bodies, sea) have the value 0 (or a negative NoData
#     code), which raw mode does not convert to NA. Left as they were, these
#     cells would give pH = 0 and sand = 0, impossible values, and bilinear
#     resampling would mix them with real values at their edges (spurious pH
#     values between 0 and ~4 around towns and along the coast). Therefore:
#       1. raw values <= 0 are set to NA in both soil layers, and a cell that is
#          no-data in one soil layer is set to NA in the other one as well;
#       2. the gaps inside the study region are filled, on the native SoilGrids
#          grid, with the mean of the neighbouring cells (3 x 3 moving window,
#          repeated until no gap is left), so that occurrence records in towns
#          and on the coast keep a soil value;
#       3. after resampling to the CHELSA grid, any region cell with climate but
#          without soil value (coastal cells whose resampling window reached the
#          sea) is filled in the same way on the CHELSA grid.
#     The number of filled cells is printed. Only the soil layers need this step.
#
# Requires
#   terra, sf; run from the project root.
# =============================================================================

library(terra)
library(sf)

# ---- Settings ---------------------------------------------------------------
PATHS <- list(
  layers_dir   = "data/raster/download/present",
  region_shape = "data/shape/IberoAtlantic.shp",
  output_dir   = "data/raster/present"
)

dir.create(PATHS$output_dir, showWarnings = FALSE, recursive = TRUE)

margin_deg <- 0.1   # crop margin around the region (~11 km at this latitude)

FILL_MAX_ITER <- 200   # maximum passes of the 3 x 3 gap-filling window (soil layers)
PH_MIN_PLAUSIBLE <- 3.5   # lower bound of plausible topsoil pH (H2O); checked at the end

# ---- Scale / offset tables (deterministic, applied to RAW values) -----------
bio_table <- data.frame(
  var    = paste0("bio", sprintf("%02d", 1:19)),
  scale  = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
             1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
  offset = c(-273.15, 0, 0, 0, -273.15, -273.15, 0, -273.15,
             -273.15, -273.15, -273.15, 0, 0, 0, 0, 0, 0, 0, 0)
)

extra_table <- data.frame(
  var    = c("clt", "hurs", "vpd", "cmi", "fcf"),
  scale  = c(0.01,   0.01,   0.1,   0.1,   1),
  offset = c(0,       0,      0,     0,     0)
)

# ---- 1. Region polygon and crop rectangle (with margin) ---------------------
# The rectangle is computed first so that every layer can be cropped as soon as
# it is opened, instead of operating on the whole planet.
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

# ---- 2. Locate the input files ----------------------------------------------
bio_files  <- list.files(PATHS$layers_dir, pattern = "CHELSA_bio.*\\.tif$", full.names = TRUE)
ph_file    <- list.files(PATHS$layers_dir, pattern = "SOILGRID_pH.*\\.tif$", full.names = TRUE)
sand_file  <- list.files(PATHS$layers_dir, pattern = "SOILGRID_sand.*\\.tif$", full.names = TRUE)
clt_file   <- list.files(PATHS$layers_dir, pattern = "CHELSA_clt.*\\.tif$", full.names = TRUE)
cmi_file   <- list.files(PATHS$layers_dir, pattern = "CHELSA_cmi.*\\.tif$", full.names = TRUE)
hurs_file  <- list.files(PATHS$layers_dir, pattern = "CHELSA_hurs.*\\.tif$", full.names = TRUE)
vpd_file   <- list.files(PATHS$layers_dir, pattern = "CHELSA_vpd.*\\.tif$", full.names = TRUE)
fcf_file   <- list.files(PATHS$layers_dir, pattern = "CHELSA_fcf.*\\.tif$", full.names = TRUE)

stopifnot(length(bio_files) == 19)
stopifnot(length(ph_file) == 1, length(sand_file) == 1)
stopifnot(length(clt_file) == 1, length(cmi_file) == 1,
          length(hurs_file) == 1, length(vpd_file) == 1,
          length(fcf_file) == 1)

# Order the bioclimatic files by the variable number in the file name (so that
# e.g. "bio10" is not placed before "bio2" by alphabetical sorting)
bio_numbers <- as.integer(sub(".*bio_?0*([0-9]+)[^0-9].*", "\\1", basename(bio_files)))
stopifnot(setequal(bio_numbers, 1:19))
bio_files <- bio_files[order(bio_numbers)]

bio_names   <- paste0("bio", sprintf("%02d", 1:19))
extra_names <- c("clt", "cmi", "hurs", "vpd", "fcf")

# ---- 3. Read (raw), crop, then convert --------------------------------------
read_crop_convert <- function(file, var_name, table, bbox) {
  ref <- table[table$var == var_name, ]
  r_raw <- rast(file, raw = TRUE)
  r_raw <- crop(r_raw, bbox)                # crop first (cheap)
  r_conv <- r_raw * ref$scale + ref$offset  # convert the small cropped piece
  names(r_conv) <- var_name
  cat(sprintf("[%s]: cropped and converted (scale=%.4f, offset=%.4f)\n",
              var_name, ref$scale, ref$offset))
  r_conv
}

# BIO1-19
bio_list <- vector("list", 19)
for (i in 1:19) {
  bio_list[[i]] <- read_crop_convert(bio_files[i], bio_names[i], bio_table, crop_ext)
}
bio_layers <- rast(bio_list)

# bio03 is recomputed from bio02 and bio07 instead of using its own file
bio_layers[["bio03"]] <- 100 * (bio_layers[["bio02"]] / bio_layers[["bio07"]])
cat("[bio03]: recomputed as 100 * bio02 / bio07 (not read from its own file).\n")

# clt, cmi, hurs, vpd, fcf
extra_list <- list(
  clt  = read_crop_convert(clt_file,  "clt",  extra_table, crop_ext),
  cmi  = read_crop_convert(cmi_file,  "cmi",  extra_table, crop_ext),
  hurs = read_crop_convert(hurs_file, "hurs", extra_table, crop_ext),
  vpd  = read_crop_convert(vpd_file,  "vpd",  extra_table, crop_ext),
  fcf  = read_crop_convert(fcf_file,  "fcf",  extra_table, crop_ext)
)

# Soil: pH and sand (crop first, then no-data handling, then conversion)
ph_raw   <- crop(rast(ph_file, raw = TRUE), crop_ext)
sand_raw <- crop(rast(sand_file, raw = TRUE), crop_ext)

# (1) No-data cells: raw values <= 0 (0 = no soil estimate; negative = NoData code)
ph_raw   <- ifel(ph_raw   <= 0, NA, ph_raw)
sand_raw <- ifel(sand_raw <= 0, NA, sand_raw)

# A cell without data in one soil layer is treated as no-data in both
if (compareGeom(ph_raw, sand_raw, stopOnError = FALSE)) {
  joint_nodata <- is.na(ph_raw) | is.na(sand_raw)
  ph_raw   <- mask(ph_raw,   joint_nodata, maskvalues = TRUE)
  sand_raw <- mask(sand_raw, joint_nodata, maskvalues = TRUE)
}

# Fills the NA cells of `r` that fall in `target` (a raster with 1 in the cells that
# must have a value and NA elsewhere) with the mean of the neighbouring cells (3 x 3
# window), repeating the pass until no target cell is left without value. Cells that
# already have a value are never modified (na.policy = "only").
fill_gaps <- function(r, target, label, max_iter = FILL_MAX_ITER) {
  n_gaps <- function(x) {
    as.numeric(global(mask(is.na(x), target), "sum", na.rm = TRUE)[1, 1])
  }
  n_target <- as.numeric(global(target, "sum", na.rm = TRUE)[1, 1])
  gaps_start <- n_gaps(r)
  iter <- 0
  while (n_gaps(r) > 0 && iter < max_iter) {
    r <- focal(r, w = 3, fun = "mean", na.policy = "only", na.rm = TRUE)
    iter <- iter + 1
  }
  gaps_end <- n_gaps(r)
  cat(sprintf("[%s]: %d of %d region cells (%.2f%%) filled with neighbouring values (%d passes)\n",
              label, gaps_start, n_target, 100 * gaps_start / n_target, iter))
  if (gaps_end > 0) {
    stop(label, ": ", gaps_end, " region cells still without value after ", max_iter,
         " passes - increase FILL_MAX_ITER")
  }
  r
}

# (2) Fill the gaps inside the region on the native SoilGrids grid
region_soil <- vect(st_transform(region, crs(ph_raw)))
ph_raw   <- fill_gaps(ph_raw,   rasterize(region_soil, ph_raw,   touches = TRUE),
                      "ph, native grid")
sand_raw <- fill_gaps(sand_raw, rasterize(region_soil, sand_raw, touches = TRUE),
                      "sand, native grid")

# Conversion to real units
ph_layer   <- ph_raw / 10   # SoilGrids stores pH x 10
sand_layer <- sand_raw      # g/kg, native SoilGrids units
names(ph_layer)   <- "ph"
names(sand_layer) <- "sand"

cat("All layers cropped and converted to real units.\n")

# ---- 4. Resample to a common grid -------------------------------------------
# The BIO layers share a grid (same source). The soil layers and the BIOCLIM+
# variables may not, so they are resampled to the BIO grid when needed.
extra_aligned <- list()
for (v in extra_names) {
  if (!compareGeom(extra_list[[v]], bio_layers, stopOnError = FALSE)) {
    aligned <- resample(extra_list[[v]], bio_layers, method = "bilinear")
  } else {
    aligned <- extra_list[[v]]
  }
  names(aligned) <- v
  extra_aligned[[v]] <- aligned
}

if (!compareGeom(ph_layer, bio_layers, stopOnError = FALSE)) {
  ph_layer <- resample(ph_layer, bio_layers, method = "bilinear")
}
if (!compareGeom(sand_layer, bio_layers, stopOnError = FALSE)) {
  sand_layer <- resample(sand_layer, bio_layers, method = "bilinear")
}
names(ph_layer)   <- "ph"
names(sand_layer) <- "sand"

# (3) Region cells with climate but without soil value after resampling (coastal
# cells whose resampling window reached the sea) are filled on the CHELSA grid
region_bio  <- vect(st_transform(region, crs(bio_layers)))
target_bio  <- mask(ifel(is.na(bio_layers[["bio06"]]), NA, 1), region_bio, touches = TRUE)
ph_layer    <- fill_gaps(ph_layer,   target_bio, "ph, CHELSA grid")
sand_layer  <- fill_gaps(sand_layer, target_bio, "sand, CHELSA grid")
names(ph_layer)   <- "ph"
names(sand_layer) <- "sand"

converted_layers <- c(bio_layers, ph_layer, sand_layer, rast(extra_aligned))

# ---- 5. Mask to the exact region polygon ------------------------------------
# The crop uses a rectangle WITH margin (needed so that bilinear resampling has
# real neighbouring cells at the polygon edge). That leaves areas outside the
# study region (e.g. Northern Meseta, Ebro valley) in the object, which would
# contaminate descriptive statistics, so the layers are masked to the exact
# polygon after cropping.
region <- st_transform(region, crs(converted_layers))
converted_layers <- mask(converted_layers, vect(region))

cat("\nFinal summary (mean, minimum and maximum, in real units):\n")
print(cbind(global(converted_layers, "mean", na.rm = TRUE),
            global(converted_layers, "min",  na.rm = TRUE),
            global(converted_layers, "max",  na.rm = TRUE)))

# Plausibility of the soil layers after the no-data correction
ph_min   <- as.numeric(global(converted_layers[["ph"]],   "min", na.rm = TRUE)[1, 1])
sand_min <- as.numeric(global(converted_layers[["sand"]], "min", na.rm = TRUE)[1, 1])
if (ph_min < PH_MIN_PLAUSIBLE || sand_min <= 0) {
  warning(sprintf("Implausible soil values remain (minimum pH = %.2f, minimum sand = %.0f g/kg): check the no-data handling",
                  ph_min, sand_min))
}

# ---- 6. Save ----------------------------------------------------------------
for (v in names(converted_layers)) {
  writeRaster(
    converted_layers[[v]],
    filename = file.path(PATHS$output_dir, paste0(v, "_present.tif")),
    overwrite = TRUE
  )
}

cat("\nCropped and converted layers saved in:", PATHS$output_dir, "\n")
