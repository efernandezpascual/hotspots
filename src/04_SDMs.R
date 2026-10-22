# =============================================================================
# Step 4 - Species distribution models (SDMs)
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
# Script     : 04_SDMs.R
#
# Purpose
#   For each of the 18 species (9 native ferns and 9 invasive species):
#   (1) spatial validation of Random Forest (RF) and Generalized Boosted Models
#       (GBM) with spatially blocked cross-validation (blockCV), repeated for 5
#       sets of random pseudo-absences;
#   (2) N_FINAL_REPS (11) independent final RF models, fitted with present-day
#       climate, each with its own random set of pseudo-absences and its own seed;
#   (3) response curves of the first final model;
#   (4) projection of each final model (no refitting) to the climate scenarios,
#       conversion to binary maps with the TSS-maximising threshold of that model,
#       and consensus of the replicates (a cell is predicted present if at least
#       half of the replicates predict it).
#
#   Predictors: BIO6, BIO14 and soil pH (see Supporting Information S1).
#   Scenarios : present; ssp126, ssp370 and ssp585 (BIO6 and BIO14 of each SSP,
#               pH unchanged); subtropical = BIO6 of ssp585 + present-day BIO14
#               + pH. It differs from ssp585 only in BIO14, so the comparison of
#               the two isolates the effect of summer drying.
#
#   Section 1b compares the block size with the spatial autocorrelation range
#   of the predictors; it only prints a message and saves nothing.
#   Section 1c quantifies the extrapolation of each future scenario: the cells
#   where a predictor falls outside its present-day range in the study region
#   (Random Forest predicts there as at the edge of the calibrated range).
#   Section 1d sets the number of final replicates, the seeds and the species to
#   run. The spatial validation (the slowest step) does not depend on the final
#   models: with RUN_SPATIAL_VALIDATION <- FALSE only the final models and
#   projections are run.
#   Replicates can be added to a previous run without repeating the existing ones:
#   set N_FINAL_REPS to the new total, REPS_TO_RUN to the new replicates (e.g. 12:15) and
#   CLEAN_PREVIOUS_FINAL <- FALSE; the consensus is recomputed with all the replicates found.
#
# Inputs
#   data/raster/present/{bio06,bio14,ph}_present.tif      (01_present_layers.R)
#   data/raster/future/{bio06,bio14}_{ssp126,ssp370,ssp585}_2071-2100.tif
#                                                        (03_future_layers.R)
#   data/species-points.csv      presence records: species, longitude, latitude
#
# Outputs (results/SDMs/)
#   Global tables : evaluation_all_species.csv, variable_importance_all_species.csv,
#                   TSS_threshold_all_species.csv (one row per species and replicate),
#                   summary_evaluation.csv, summary_vars.csv
#   Per species (results/SDMs/<Species_name>/):
#     parameters.rds, summary.csv
#     01_spatial_validation/  PA1 ... PA5 (biomod2 objects), evaluations_spatial.csv,
#                             variable_importance_spatial.csv, summary_RF_GBM.csv
#     02_final_models/        rep1 ... rep11 (biomod2 objects of each replicate),
#                             evaluations_final.csv and variable_importance_final.csv
#                             (all replicates, column `replicate`),
#                             response_curves/response_curves_RF.png and .csv (first
#                             replicate; the .csv holds the values of the curves)
#     03_projections/         binary/<Species>_<scenario>_binary.tif      consensus map
#                             agreement/<Species>_<scenario>_agreement.tif    fraction of
#                                                  replicates predicting presence
#                             rep1 ... rep11/binary/<Species>_<scenario>_binary.tif and
#                             <Species>_TSS_threshold.txt of each replicate
#                             (<scenario> = present, ssp126, ssp370, ssp585, subtropical)
#   biomod2 also writes its own model and projection folders inside these directories.
#   Extrapolation (results/extrapolation/):
#     novel_<scenario>.tif        number of predictors outside their present-day
#                                 regional range in each cell (0 = no extrapolation)
#     extrapolation_summary.csv   % of the region cells below / above the present-day
#                                 range, per scenario and predictor
#   results/sessionInfo_04_SDMs.txt   R and package versions of the run
#
# Requires
#   biomod2 (>= 4.2), sf, dplyr, terra, blockCV (and automap, used by the block
#   size check); run from the project root.
#
# Note
#   The whole workflow is slow (18 species x 5 pseudo-absence sets x 2 algorithms
#   x 5 folds, plus the final models and projections).
# =============================================================================

##### load required packages #####

library(biomod2)
library(sf)
library(dplyr)
library(terra)
library(blockCV)

##### 1. VARIABLES #####

# IMPORTANT: the layers must already be in real units (conversions are applied
# in 01_present_layers.R and 03_future_layers.R).

PREDICTOR_NAMES <- c("bio06", "bio14", "ph")

# Reads the layers of one scenario as a terra SpatRaster with fixed layer names
# (biomod2 matches the predictors of the projections to those of the model by name)
read_predictors <- function(files) {
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    stop("Missing predictor layers:\n", paste(missing_files, collapse = "\n"))
  }
  r <- terra::rast(files)
  names(r) <- PREDICTOR_NAMES
  r
}

present_file <- function(v) file.path("data/raster/present", paste0(v, "_present.tif"))
future_file  <- function(v, ssp) file.path("data/raster/future", paste0(v, "_", ssp, "_2071-2100.tif"))

# Present
predictors_present <- read_predictors(present_file(PREDICTOR_NAMES))

# SSPs: future BIO6 and BIO14; pH unchanged
predictors_ssp126 <- read_predictors(c(future_file(c("bio06", "bio14"), "ssp126"), present_file("ph")))
predictors_ssp370 <- read_predictors(c(future_file(c("bio06", "bio14"), "ssp370"), present_file("ph")))
predictors_ssp585 <- read_predictors(c(future_file(c("bio06", "bio14"), "ssp585"), present_file("ph")))

# Subtropical: future winter temperature of ssp585 (BIO6) + present-day BIO14 + pH
predictors_subtropical <- read_predictors(c(future_file("bio06", "ssp585"),
                                            present_file(c("bio14", "ph"))))

predictors_present
if (interactive()) plot(predictors_present)

##### 1b. BLOCK SIZE CHECK ##### (prints a message; saves nothing)

# Size (m) of the spatial blocks used in the spatial cross-validation
BLOCK_SIZE <- 25000

# Compares the block size with the range of spatial autocorrelation of the
# predictors, estimated with blockCV::cv_spatial_autocor (empirical variogram of
# each layer). The range is the distance beyond which two cells are
# approximately independent, so blocks smaller than the range do not separate
# training and validation data completely. The median range across predictors
# is used. The range of pH is not interpretable (strong east-west trend: its
# variogram does not stabilise), so only the climatic ranges should be read.
# The check never stops the analysis.
# The variogram is estimated from a random sample of cells, so the seed is fixed to
# report the same range in every run.
local({
  set.seed(42)
  autocor <- tryCatch(
    blockCV::cv_spatial_autocor(r = predictors_present, num_sample = 5000, plot = FALSE),
    error = function(e) {
      message("Block size check skipped: ", conditionMessage(e))
      NULL
    })

  if (!is.null(autocor)) {
    cat("\n")
    cat("============================================================\n")
    cat("BLOCK SIZE CHECK\n")
    cat("Block size used:", BLOCK_SIZE / 1000, "km\n")
    cat("Spatial autocorrelation range per predictor (km):\n")
    print(data.frame(layer = autocor$range_table$layers,
                     range_km = round(autocor$range_table$range / 1000, 1)))
    cat("Median range across predictors:", round(autocor$range / 1000, 1), "km\n")
    if (autocor$range > BLOCK_SIZE) {
      cat("NOTE: the block size is smaller than the median autocorrelation range.\n")
      cat("Training and validation blocks are therefore not fully independent;\n")
      cat("the block size is a compromise given the elongated shape of the study\n")
      cat("region, which limits the number of independent blocks.\n")
    } else {
      cat("The block size is at least as large as the median autocorrelation range.\n")
    }
    cat("============================================================\n")
  }
})

##### 1c. EXTRAPOLATION ##### (cheap; does not depend on the models)

# For each future scenario: cells of the study region where a predictor is outside its
# present-day range in the region. The pseudo-absences are drawn across the whole
# region, so this range is (approximately) the calibrated range of the models; Random
# Forest predicts outside it as at the edge of the range (constant response), so these
# cells must be interpreted with caution. pH is constant across scenarios.
EXTRAPOLATION_DIR <- "results/extrapolation"
dir.create(EXTRAPOLATION_DIR, recursive = TRUE, showWarnings = FALSE)

local({
  future_list <- list(ssp126 = predictors_ssp126, ssp370 = predictors_ssp370,
                      ssp585 = predictors_ssp585, subtropical = predictors_subtropical)
  region_cells <- as.numeric(global(!is.na(predictors_present[["bio06"]]), "sum", na.rm = TRUE)[1, 1])

  rows <- list()
  for (scenario in names(future_list)) {
    novel <- 0
    for (v in PREDICTOR_NAMES) {
      lo <- as.numeric(global(predictors_present[[v]], "min", na.rm = TRUE)[1, 1])
      hi <- as.numeric(global(predictors_present[[v]], "max", na.rm = TRUE)[1, 1])
      x  <- future_list[[scenario]][[v]]
      below <- x < lo
      above <- x > hi
      novel <- novel + below + above
      rows[[length(rows) + 1]] <- data.frame(
        scenario  = scenario,
        variable  = v,
        present_min = lo,
        present_max = hi,
        pct_below = 100 * as.numeric(global(below, "sum", na.rm = TRUE)[1, 1]) / region_cells,
        pct_above = 100 * as.numeric(global(above, "sum", na.rm = TRUE)[1, 1]) / region_cells)
    }
    names(novel) <- "n_predictors_outside_range"
    writeRaster(novel, file.path(EXTRAPOLATION_DIR, paste0("novel_", scenario, ".tif")),
                overwrite = TRUE)
    rows[[length(rows) + 1]] <- data.frame(
      scenario = scenario, variable = "any", present_min = NA, present_max = NA,
      pct_below = NA,
      pct_above = 100 * as.numeric(global(novel > 0, "sum", na.rm = TRUE)[1, 1]) / region_cells)
  }
  extrapolation <- do.call(rbind, rows)
  write.csv(extrapolation, file.path(EXTRAPOLATION_DIR, "extrapolation_summary.csv"),
            row.names = FALSE)
  cat("\n============================================================\n")
  cat("EXTRAPOLATION: % of region cells outside the present-day range\n")
  cat("(variable 'any': % of cells with at least one predictor outside its range)\n")
  print(transform(extrapolation, pct_below = round(pct_below, 2), pct_above = round(pct_above, 2)),
        row.names = FALSE)
  cat("============================================================\n")
})

##### 1d. FINAL MODEL SETTINGS #####

# Number of independent final RF models per species. Each one is fitted with its own
# random set of pseudo-absences and its own seed (SEED_FINAL + replicate number), and
# the maps used in the analyses are the consensus of the replicates. An odd number
# avoids ties in the consensus (a cell is present if at least half of the replicates
# predict it).
N_FINAL_REPS <- 11
SEED_FINAL   <- 1000

# Species to run. NULL: all the species in the occurrence file. To run only some species
# (e.g. to repeat one that failed) give their names as written in data/species-points.csv,
# e.g. "Culcita macrocarpa".
SPECIES_SUBSET <- NULL

# TRUE : run the whole workflow (spatial validation, final models and projections).
# FALSE: skip the spatial validation and reuse the files of a previous run
#        (01_spatial_validation/ of each species). The spatial validation does not
#        depend on the final models, so it does not need to be repeated when only
#        the final models or the projections change.
RUN_SPATIAL_VALIDATION <- TRUE

# Replicates to fit in this run. By default all of them. To add replicates to a previous run
# (e.g. from 11 to 15), use REPS_TO_RUN <- 12:15 together with CLEAN_PREVIOUS_FINAL <- FALSE
# and RUN_SPATIAL_VALIDATION <- FALSE: the existing replicates are kept as they are.
REPS_TO_RUN <- NULL   # NULL = all the replicates (1:N_FINAL_REPS)

# TRUE: delete 02_final_models/ and 03_projections/ of each species before fitting the
# final models, so that files of a previous run are not mixed with the new ones.
CLEAN_PREVIOUS_FINAL <- TRUE

# Folder of the results
results_dir <- "results/SDMs"

if (is.null(REPS_TO_RUN)) REPS_TO_RUN <- seq_len(N_FINAL_REPS)
if (N_FINAL_REPS %% 2 == 0) warning("N_FINAL_REPS is even: the consensus of the replicates may have ties")
cat("\n============================================================\n")
cat("SETTINGS OF THIS RUN\n")
cat("N_FINAL_REPS:", N_FINAL_REPS, "| REPS_TO_RUN:", paste(REPS_TO_RUN, collapse = " "),
    "| RUN_SPATIAL_VALIDATION:", RUN_SPATIAL_VALIDATION,
    "| CLEAN_PREVIOUS_FINAL:", CLEAN_PREVIOUS_FINAL, "\n")
cat("Results folder:", results_dir, "\n")
cat("SPECIES_SUBSET:", if (is.null(SPECIES_SUBSET)) "all species" else paste(SPECIES_SUBSET, collapse = ", "), "\n")
cat("============================================================\n")

if (CLEAN_PREVIOUS_FINAL && !setequal(REPS_TO_RUN, seq_len(N_FINAL_REPS))) {
  stop("CLEAN_PREVIOUS_FINAL deletes all the previous final models: use it only when REPS_TO_RUN covers all the replicates")
}

##### 2. LOAD PRESENCE POINTS #####

occurrence_data <- read.csv("data/species-points.csv", header = TRUE)

occurrence_data <- occurrence_data %>%
  select(
    species,
    longitude,
    latitude) %>%
  rename(
    Name = species) %>%
  mutate(
    Response = 1) # All records are presences (Response = 1); there are no true absences (Response = 0)

##### 3. RESULTS DIRECTORY AND HELPERS #####

# results_dir is set in section 1d

# Create the results folder if it does not exist
if (!dir.exists(results_dir)) {
  
  dir.create(
    results_dir,
    recursive = TRUE)
  
} 

# biomod2 functions with an explicit seed. biomod2 (>= 4.2) accepts a `seed.val`
# argument that fixes the random number generator inside its own functions
# (pseudo-absences, cross-validation split, model fitting); set.seed() before the
# call is not enough for an exactly reproducible run. The argument is passed only
# if the installed version of biomod2 supports it.
has_seed_arg <- function(fun) "seed.val" %in% names(formals(fun))

format_data <- function(..., seed) {
  set.seed(seed)
  if (has_seed_arg(BIOMOD_FormatingData)) {
    BIOMOD_FormatingData(..., seed.val = seed)
  } else {
    BIOMOD_FormatingData(...)
  }
}

run_modeling <- function(..., seed) {
  set.seed(seed)
  if (has_seed_arg(BIOMOD_Modeling)) {
    BIOMOD_Modeling(..., seed.val = seed)
  } else {
    BIOMOD_Modeling(...)
  }
}

cat("biomod2 version:", as.character(packageVersion("biomod2")),
    "| seed.val supported by BIOMOD_FormatingData:", has_seed_arg(BIOMOD_FormatingData),
    "| by BIOMOD_Modeling:", has_seed_arg(BIOMOD_Modeling), "\n")

# Number of grid cells with at least one presence of the species (and values for all
# predictors). With filter.raster = TRUE biomod2 keeps one presence per cell, so the
# number of pseudo-absences is set to this number (prevalence 0.5).
n_presence_cells <- function(species_data, predictors) {
  cells <- terra::cellFromXY(predictors, as.matrix(species_data[, c("longitude", "latitude")]))
  cells <- unique(cells[!is.na(cells)])
  values <- terra::extract(predictors, cells)
  sum(stats::complete.cases(values))
}

# folder name for each species inside results/SDMs
species_folder <- function(species_name) {
  
  x <- gsub(
    " ",
    "_",
    species_name
  )
  
  x <- gsub(
    "[^[:alnum:]_\\-]",
    "",
    x
  )
  
  return(x)
} 

##### 4. SPATIAL VALIDATION ##### function for model (random forest and GBM) fitting and block CV validation

run_spatial_validation <- function(
  species_name,
  occurrence_data,
  predictors,
  species_dir,
  n_pa = 5, # number of pseudo-absence replicates
  k = 5, # number of cross-validation folds (4 for training, ~80%, and 1 for validation, ~20%)
  block_size = BLOCK_SIZE, # size of the spatial blocks (m)
  models = c("RF", "GBM"),
  seed = 100
  
) {
  
  # folder
  validation_dir <- file.path(
    species_dir,
    "01_spatial_validation")
  
  dir.create(
    validation_dir,
    recursive = TRUE,
    showWarnings = FALSE)
  
  # species data
  species_data <- subset(
    occurrence_data,
    Name == species_name)
  
  if (nrow(species_data) == 0) {
    
    stop(
      paste0(
        "No data for ",
        species_name))
  }
  
  # Information
  cat("\n\n")
  cat("============================================================\n")
  cat("SPATIAL VALIDATION\n")
  cat("SPECIES:", species_name, "\n")
  cat("PRESENCES:", nrow(species_data), "\n")
  cat("MODELS:", paste(models, collapse = ", "), "\n")
  cat("PSEUDOABSENCES:", n_pa, "\n")
  cat("FOLDS:", k, "\n")
  cat("BLOCK SIZE:", block_size, "m\n")
  cat("============================================================\n")
  
  # List of the biomod2 results of each pseudo-absence set
  results <- vector(
    "list",
    n_pa)
  
  # Loop pseudoabsences
  for (i in 1:n_pa) {
    
    time_start <- Sys.time()
    
    cat("\n")
    cat("------------------------------------------------------------\n")
    cat(
      "STARTING PSEUDOABSENCES",
      i,
      "/",
      n_pa,
      "\n"
    )
    cat(
      "SPECIES:",
      species_name,
      "\n"
    )
    cat(
      "TIME:",
      format(
        time_start,
        "%H:%M:%S"
      ),
      "\n"
    )
    cat("------------------------------------------------------------\n")
  
  # Pseudoabsences folder
    pa_dir <- file.path(
      validation_dir,
      paste0(
        "PA",
        i))
    
    dir.create(
      pa_dir,
      recursive = TRUE,
      showWarnings = FALSE)
    
    # 1. Pseudoabsences
    mod_pre <- format_data(
      seed = seed + i,
      resp.var = species_data[,4],
      expl.var = predictors,
      resp.xy = species_data[,2:3],
      resp.name = paste0(
        "PA",
        i
      ),
      PA.strategy = "random",
      PA.nb.rep = 1, # one pseudo-absence set per loop iteration (n_pa sets in total)
      PA.nb.absences = n_presence_cells(species_data, predictors),
      filter.raster = TRUE, # one presence per grid cell
      dir.name = validation_dir)
    
    # 2. Coordinates and response
    xy_data <- mod_pre@coord
    
    response_data <- mod_pre@data.species
    
    combined_data <- data.frame(
      x = xy_data[,1],
      y = xy_data[,2],
      response = response_data)
    
    combined_data$response[
      is.na(
        combined_data$response)
    ] <- 0
    
    # 3. SF (coordinates to spatial object)
    combined_sf <- st_as_sf(
      combined_data,
      coords = c(
        "x",
        "y"),
      crs = 4326)
    
    # 4. EPSG:3035 (degrees to metres, because the blocks are defined in metres)
    combined_sf <- st_transform(
      combined_sf,
      3035)
    
    # 5. BLOCKCV
    sb1 <- cv_spatial(
      x = combined_sf, # presences and pseudo-absences
      column = "response",
      k = k,
      size = block_size,
      selection = "random", # blocks are assigned to folds at random
      iteration = 1, # one iteration per pseudo-absence set
      biomod2 = TRUE,
      plot = interactive()
    )
    
    # 6. TABLE BIOMOD2
    cv_table <- sb1$biomod_table
    
    colnames(cv_table) <- paste0(
      "_allData_RUN",
      1:ncol(cv_table))
    
    cv_table <- cbind(
      cv_table,
      "_allData_allRun" = TRUE)
    
    # 7. FORMAT DATA
    mod_eval <- format_data(
      seed = seed + i,
      resp.var = combined_data$response,
      expl.var = predictors,
      resp.xy = combined_data[,1:2],
      resp.name = paste0(
        "PA",
        i),
      filter.raster = TRUE,
      dir.name = validation_dir)
    
    # CHECK ROWS
    if (
      nrow(cv_table) !=
      nrow(mod_eval@coord)) {
      
      stop(
        paste0(
          "ERROR in ",
          species_name,
          " PA",
          i,
          ": cv_table has ",
          nrow(cv_table),
          " rows and BIOMOD has ",
          nrow(mod_eval@coord),
          "."))
    }
    
    # 8. MODELING
    cat("\n")
    cat("STARTING BIOMOD_MODELING\n")
    cat(
      "MODELS:",
      paste(
        models,
        collapse = ", "
      ),
      "\n")
    
    result <- run_modeling(
      seed = seed + i,
      bm.format = mod_eval,
      models = models,
      metric.eval = c(
        "TSS",
        "AUCroc" # biomod2 >= 4.2 uses AUCroc instead of ROC
      ),
      CV.strategy = "user.defined",
      CV.user.table = cv_table,
      CV.do.full.models = TRUE,
      var.import = 3
    )
    
    # 9. SAVE DATA
    saveRDS(
      mod_eval,
      file = file.path(
        pa_dir,
        "formatted_data.rds"))
    
    saveRDS(
      sb1,
      file = file.path(
        pa_dir,
        "blockCV.rds"))
    
    saveRDS(
      cv_table,
      file = file.path(
        pa_dir,
        "CV_table.rds"))
    
    # SAVE RESULTS
    saveRDS(
      result,
      file = file.path(
        pa_dir,
        "model_result.rds"))
    
    # SAVE LIST
    results[[i]] <- result
    
    # TIME
    time_end <- Sys.time()
    
    cat("\n")
    cat("------------------------------------------------------------\n")
    cat(
      "PSEUDOABSENCE FINISHED",
      i,
      "/",
      n_pa,
      "\n"
    )
    cat(
      "TIME:",
      round(
        as.numeric(
          difftime(
            time_end,
            time_start,
            units = "mins")),
        2),
      "minutes\n"
    )
    cat("------------------------------------------------------------\n")
  }
    
  # The biomod2 objects of each pseudo-absence set are already saved in PA1 ... PA5;
  # the results are returned for extract_spatial_results()
  return(
    list(
      species = species_name,
      results = results))
}

##### 5. EXTRACT MODELING RESULTS ##### function for extracting statistics and variable importance from previous models

extract_spatial_results <- function(
    validation_object,
    species_dir,
    species_name) {
  
  validation_dir <- file.path(
    species_dir,
    "01_spatial_validation")
  
  # EVALUATIONS
  evaluation_list <- list()
  
  for (
    i in seq_along(
      validation_object$results)) {
    
    ev <- get_evaluations(
      validation_object$results[[i]])
    
    ev$Species <- species_name
    
    ev$PA <- paste0(
      "PA",
      i)
    
    evaluation_list[[i]] <- ev
  }
  
  evaluations <- bind_rows(
    evaluation_list)
  
  write.csv(
    evaluations,
    file = file.path(
      validation_dir,
      "evaluations_spatial.csv"),
    row.names = FALSE)
  
  # VARIABLE IMPORTANCE
  importance_list <- list()
  
  for (
    i in seq_along(
      validation_object$results)) {
    
    vi <- get_variables_importance(
      validation_object$results[[i]])
    
    vi$Species <- species_name
    
    vi$PA <- paste0(
      "PA",
      i)

    importance_list[[i]] <- vi
    
  }
  
  variable_importance <- bind_rows(
    importance_list)
  
  write.csv(
    variable_importance,
    file = file.path(
      validation_dir,
      "variable_importance_spatial.csv"
    ),
    row.names = FALSE
  )
  
  # SUMMARY RF VS GBM
  summary_algorithm <- evaluations %>%
    
    filter(
      run != "allRun") %>%
    
    group_by(
      Species,
      PA,
      algo,
      metric.eval) %>%
    
    summarise(
      mean = mean(
        validation,
        na.rm = TRUE),
      
      sd = sd(
        validation,
        na.rm = TRUE),
      
      min = min(
        validation,
        na.rm = TRUE),
      
      max = max(
        validation,
        na.rm = TRUE),
      
      n = sum(
        !is.na(
          validation)),
      
      .groups = "drop")
  
  write.csv(
    summary_algorithm,
    file = file.path(
      validation_dir,
      "summary_RF_GBM.csv"),
    row.names = FALSE)
  
  invisible(
    list(
      evaluations = evaluations,
      variable_importance = variable_importance,
      summary = summary_algorithm))
}

##### 6. FINAL MODELS ##### functions for fitting final random forest models

run_final_models <- function(
  species_name,
  occurrence_data,
  predictors,
  species_dir,
  rep_id = 1,
  seed = SEED_FINAL + rep_id) {
  
  # Folder
  final_dir <- file.path(
    species_dir,
    "02_final_models",
    paste0("rep", rep_id))
  
  dir.create(
    final_dir,
    recursive = TRUE,
    showWarnings = FALSE)
  
  # Data
  species_data <- subset(
    occurrence_data,
    Name == species_name)
  
  # Format data (the pseudo-absences, the random split and the RF fit all depend on this seed)
  mod_final <- format_data(
    seed = seed,
    resp.var = species_data[,4],
    expl.var = predictors,
    resp.xy = species_data[,2:3],
    resp.name = species_name,
    PA.strategy = "random",
    PA.nb.rep = 1,
    PA.nb.absences = n_presence_cells(species_data, predictors),
    filter.raster = TRUE, # one presence per grid cell, as in the spatial validation
    dir.name = final_dir)
  
  # Modeling
  result_final <- run_modeling(
    seed = seed,
    bm.format = mod_final,
    models = "RF",
    CV.strategy = "random",
    CV.nb.rep = 1, # the spatial cross-validation was done above, so blockCV is not used for the final model
    CV.perc = 0.8,
    CV.do.full.models = TRUE,
    metric.eval = c(
      "TSS",
      "AUCroc"),
    var.import = 3)
  
  # Evaluation
  evaluations <- get_evaluations(
    result_final)
  
  write.csv(
    evaluations,
    file = file.path(
      final_dir,
      "evaluations_final.csv"),
    row.names = FALSE)
  
  # VARIABLE IMPORTANCE
  variable_importance <- get_variables_importance(
    result_final)
  
  write.csv(
    variable_importance,
    file = file.path(
      final_dir,
      "variable_importance_final.csv"),
    row.names = FALSE)
    
  # Save objects
  saveRDS(
    result_final,
    file = file.path(
      final_dir,
      "final_models.rds"))
  
  saveRDS(
    mod_final,
    file = file.path(
      final_dir,
      "formatted_final_data.rds"))
  
  return(
    list(
      model = result_final))
}

##### 7. RESPONSE CURVES ##### function for extracting final model response curves

run_response_curves <- function(
    species_name,
    result_final,
    species_dir) {
  
  # Folder
  response_dir <- file.path(
    species_dir,
    "02_final_models",
    "response_curves")
  
  dir.create(
    response_dir,
    recursive = TRUE,
    showWarnings = FALSE)
  
  # Load RF model
  RF_model <- BIOMOD_LoadModels(
    result_final)
  
  RF_model <- grep(
    "_allData_allRun_RF",
    RF_model,
    value = TRUE)

  if (length(RF_model) == 0) {
    
    stop(
      paste0(
        "RF allData_allRun not found for ",
        species_name))
  }

  # Response curves
  png(
    filename = file.path(
      response_dir,
      "response_curves_RF.png"),
    width = 2400,
    height = 1800,
    res = 300)
  on.exit(dev.off(), add = TRUE)   # the device is closed even if the plot fails
  
  curves <- bm_PlotResponseCurves(
    bm.out = result_final,
    models.chosen = RF_model,
    fixed.var.metric = "mean")

  # Values of the curves, so that Supporting Information S2 can redraw them with the
  # style and species names of the other figures. Never stops the analysis.
  tryCatch(
    write.csv(curves$tab, file.path(response_dir, "response_curves_RF.csv"), row.names = FALSE),
    error = function(e) message("Response curve values not saved: ", conditionMessage(e)))
  
  saveRDS(
    RF_model,
    file = file.path(
      response_dir,
      "RF_allData_allRun_model.rds"))
  
  invisible(TRUE)
}

##### 8. TSS THRESHOLD ##### function for deciding binary transformation threshold

get_TSS_threshold <- function(
  result_final,
  algorithm) {
  
  evaluations <- get_evaluations(
    result_final)
  if (!"full.name" %in% names(evaluations)) {
    stop("get_evaluations() has no 'full.name' column in this biomod2 version: ",
         "cannot identify the _allData_allRun_ model")
  }
  
  # Cutoff of the full model fitted with all the data ("_allData_allRun_"), the model
  # that is projected and binarised. Since CV.do.full.models = TRUE, there is also a
  # "_PA1_allRun_" model (same data, different random forest), whose cutoff must not be
  # used: both have run == "allRun", so the model name is used to tell them apart.
  threshold <- evaluations %>%
    filter(
      algo == algorithm,
      run == "allRun",
      grepl(paste0("_allData_allRun_", algorithm, "$"), full.name),
      metric.eval == "TSS") %>%
    
    pull(
      cutoff)
  
  if (
    length(threshold) == 0) {

    warning(
      
      paste0(
        "No threshold found for ",
        algorithm))
    
    return(
      NA_real_)
    
  }
  
  if (length(threshold) > 1) {
    stop("More than one full-model TSS cutoff found for ", algorithm)
  }

  threshold <- threshold / 1000
  
  return(
    threshold)
  
}

##### 9. PROJECTION ##### function for projecting model results to climatic scenarios

run_projections <- function(
  species_name,
  result_final,
  species_dir,
  predictor_list,
  algorithm,
  projection_subdir = NULL,
  continuous_dir = NULL) {
  
  # Folder
  projection_dir <- file.path(
    species_dir,
    "03_projections")
  if (!is.null(projection_subdir)) {
    projection_dir <- file.path(
      projection_dir,
      projection_subdir)
  }
  
  binary_dir <- file.path(
    projection_dir,
    "binary")
  
  if (is.null(continuous_dir)) {
    continuous_dir <- file.path(
      projection_dir,
      "continuous")
  }
  
  dir.create(
    projection_dir,
    recursive = TRUE,
    showWarnings = FALSE)
  
  dir.create(
    binary_dir,
    recursive = TRUE,
    showWarnings = FALSE)
  
  dir.create(
    continuous_dir,
    recursive = TRUE,
    showWarnings = FALSE)
  
  # Threshold
  threshold <- get_TSS_threshold(
    result_final,
    algorithm)
  
  cat("\n")
  cat("============================================================\n")
  cat("PROJECTIONS\n")
  cat("SPECIES:", species_name, "\n")
  cat("ALGORITHM:", algorithm, "\n")
  cat("THRESHOLD:", threshold, "\n")
  cat("============================================================\n")
  
  # Save threshold per each species
  writeLines(
    as.character(
      threshold),
    con = file.path(
      projection_dir,
      paste0(
        species_folder(species_name),
        "_TSS_threshold.txt")))
  
  # Models
  models_chosen <- get_built_models(
    result_final)
  
  # Only the full model (all data, all runs) is projected: it is the one binarised and
  # used in the analyses. The 80/20 split model and the PA1 full model are not needed,
  # and projecting them would triple the projection time and the disk space.
  models_chosen <- models_chosen[
    grepl(
      paste0(
        "_allData_allRun_",
        algorithm,
        "$"),
      models_chosen)]
  
  if (
    length(models_chosen) == 0
  ) {
    
    stop(
      
      paste0(
        "Cannot find ",
        algorithm,
        " models for ",
        species_name))
    
  }
  
  # Loop projections
  for (
    scenario_name in names(
      predictor_list)) {
    
    cat(
      "\nProjecting:",
      scenario_name,
      "\n")
    
    # Projection
    projection <- BIOMOD_Projection(
      bm.mod = result_final,
      proj.name = scenario_name,
      new.env = predictor_list[[scenario_name]],
      models.chosen = models_chosen,
      build.clamping.mask = TRUE)
    
    # Save projection
    saveRDS(
      projection,
      file = file.path(
        projection_dir,
        paste0(
          species_folder(species_name),
          "_",
          scenario_name,
          "_projection.rds")))
    
    # Predictions
    predictions <- get_predictions(projection)
    
    prediction_names <- names(predictions)
    
    selected <- grep(
      paste0(
        "allData_allRun_",
        algorithm),
      prediction_names,
      value = TRUE)
    
    if (
      length(selected) == 0) {
      
      warning(

        paste0(
          "Not found ",
          "allData_allRun_",
          algorithm,
          " for ",
          scenario_name))
      
      next
      
    }
    
    # Continuous raster
    pred <- predictions[[selected[1]]]
    
    pred <- pred / 1000
    
    writeRaster(
      pred,
      filename = file.path(
        continuous_dir,
        paste0(
          species_folder(species_name),
          "_",
          scenario_name,
          "_continuous.tif")),
      overwrite = TRUE)
    
    # Binary raster
    binary <- pred >= threshold
    
    writeRaster(
      binary,
      filename = file.path(
        binary_dir,
        paste0(
          species_folder(species_name),
          "_",
          scenario_name,
          "_binary.tif")),
      overwrite = TRUE)

  }
  
  invisible(TRUE)
  
}

##### 9b. CONSENSUS OF THE REPLICATES ##### function for combining the final model replicates
combine_replicates <- function(
  species_name,
  species_dir,
  scenarios) {

  sp <- species_folder(species_name)
  projection_dir <- file.path(species_dir, "03_projections")

  # All the replicates found on disk (including those of previous runs)
  rep_dirs <- list.dirs(projection_dir, recursive = FALSE, full.names = FALSE)
  rep_ids  <- sort(as.integer(sub("^rep", "", grep("^rep[0-9]+$", rep_dirs, value = TRUE))))
  if (length(rep_ids) == 0) stop("No replicates found in ", projection_dir)
  cat("Consensus of", length(rep_ids), "replicates:", paste(rep_ids, collapse = " "), "\n")

  for (d in c("binary", "agreement")) {
    dir.create(file.path(projection_dir, d), recursive = TRUE, showWarnings = FALSE)
  }

  for (scenario_name in scenarios) {

    file_name <- function(suffix) paste0(sp, "_", scenario_name, "_", suffix, ".tif")

    # Binary maps of each replicate (each one with the threshold of its own model)
    binary <- terra::rast(
      file.path(projection_dir, paste0("rep", rep_ids), "binary", file_name("binary")))

    # Fraction of replicates predicting presence in each cell
    agreement <- terra::mean(binary)

    # Consensus: predicted present if at least half of the replicates predict it
    terra::writeRaster(
      agreement >= 0.5,
      filename = file.path(projection_dir, "binary", file_name("binary")),
      overwrite = TRUE)

    terra::writeRaster(
      agreement,
      filename = file.path(projection_dir, "agreement", file_name("agreement")),
      overwrite = TRUE)
  }

  invisible(rep_ids)
}

##### 10. SPECIES FUNCTION ##### function for applying previous functions to each species

run_species <- function(
  species_name,
  occurrence_data,
  results_dir,
  predictors_present,
  predictors_ssp126,
  predictors_ssp370,
  predictors_ssp585,
  predictors_subtropical,
  n_pa = 5,
  k = 5,
  block_size = BLOCK_SIZE,
  models = c(
    "RF",
    "GBM"),
  seed = 100) {
  
  # Species folder
  sp_folder <- species_folder(species_name)
  
  species_dir <- file.path(
    results_dir,
    sp_folder)
  
  dir.create(
    species_dir,
    recursive = TRUE,
    showWarnings = FALSE)
  
  # Save parameters (spatial validation and final models)
  species_data <- subset(occurrence_data, Name == species_name)
  parameters <- list(
    species = species_name,
    n_presences = nrow(species_data),
    n_presence_cells = n_presence_cells(species_data, predictors_present),
    predictors = PREDICTOR_NAMES,
    n_pa = n_pa,
    k = k,
    block_size = block_size,
    models = models,
    seed = seed,
    n_final_reps = N_FINAL_REPS,
    seed_final = SEED_FINAL)

  saveRDS(
    parameters,
    file = file.path(
      species_dir,
      "parameters.rds"))

  # 10.1 SPATIAL VALIDATION and EXTRACTION OF ITS RESULTS
  if (RUN_SPATIAL_VALIDATION) {
    # remove the files of a previous spatial validation, so that old biomod2 objects
    # (e.g. fitted with other predictors) are not mixed with the new ones
    unlink(file.path(species_dir, "01_spatial_validation"), recursive = TRUE)
    spatial_validation <- run_spatial_validation(
      species_name = species_name,
      occurrence_data = occurrence_data,
      predictors = predictors_present,
      species_dir = species_dir,
      n_pa = n_pa,
      k = k,
      block_size = block_size,
      models = models,
      seed = seed)
    # the tables are written to disk; the biomod2 objects are then freed from memory
    # before fitting the final models
    extract_spatial_results(
      validation_object = spatial_validation,
      species_dir = species_dir,
      species_name = species_name)
    rm(spatial_validation)
  } else {
    cat("\nSpatial validation skipped (RUN_SPATIAL_VALIDATION = FALSE): reusing",
        file.path(species_dir, "01_spatial_validation"), "\n")
    if (!dir.exists(file.path(species_dir, "01_spatial_validation"))) {
      warning("No previous spatial validation found for ", species_name)
    }
  }

  # 10.2 PREDICTORS OF EACH SCENARIO
  predictor_list <- list(
    present = predictors_present,
    ssp126 = predictors_ssp126,
    ssp370 = predictors_ssp370,
    ssp585 = predictors_ssp585,
    subtropical = predictors_subtropical)

  # 10.3 FINAL MODELS: independent replicates (REPS_TO_RUN)
  if (CLEAN_PREVIOUS_FINAL) {
    unlink(
      file.path(species_dir, c("02_final_models", "03_projections")),
      recursive = TRUE)
  }

  # The continuous maps of the replicates are not used afterwards: they are written to a
  # temporary folder
  continuous_root <- file.path(tempdir(), sp_folder)

  for (r in REPS_TO_RUN) {

    cat("\n--- Final model: replicate", r, "of", N_FINAL_REPS, "---\n")

    final_r <- run_final_models(
      species_name = species_name,
      occurrence_data = occurrence_data,
      predictors = predictors_present,
      species_dir = species_dir,
      rep_id = r,
      seed = SEED_FINAL + r)

    # 10.4 RESPONSE CURVES (first replicate only)
    if (r == 1) {
      run_response_curves(
        species_name = species_name,
        result_final = final_r$model,
        species_dir = species_dir
      )
    }

    # 10.5 PROJECTIONS
    run_projections(
      species_name = species_name,
      result_final = final_r$model,
      species_dir = species_dir,
      predictor_list = predictor_list,
      algorithm = "RF",
      projection_subdir = paste0("rep", r),
      continuous_dir = file.path(continuous_root, paste0("rep", r))
    )
    rm(final_r)
  }

  # 10.6 CONSENSUS OF THE REPLICATES (all the replicates found on disk)
  rep_ids <- combine_replicates(
    species_name = species_name,
    species_dir = species_dir,
    scenarios = names(predictor_list))
  unlink(continuous_root, recursive = TRUE)

  # Tables of the final models (all replicates), from the files of each replicate
  final_dir <- file.path(species_dir, "02_final_models")
  for (table_name in c("evaluations_final", "variable_importance_final")) {
    write.csv(
      bind_rows(lapply(rep_ids, function(r) {
        x <- read.csv(file.path(final_dir, paste0("rep", r), paste0(table_name, ".csv")))
        x$replicate <- r
        x
      })),
      file = file.path(final_dir, paste0(table_name, ".csv")),
      row.names = FALSE)
  }

  # SUMMARY
  summary_species <- data.frame(
    species = species_name,
    n_presences = parameters$n_presences,
    n_presence_cells = parameters$n_presence_cells,
    n_PA_sets = n_pa,
    n_final_reps = N_FINAL_REPS,
    k = k,
    block_size = block_size,
    models =
      paste(
        models,
        collapse = ", "))
  
  write.csv(
    summary_species,
    file = file.path(
      species_dir,
      "summary.csv"),
    row.names = FALSE)
  
  # Only the summary is returned: everything else is saved on disk
  return(
    list(
      summary = summary_species))
}

##### 11. SPECIES LIST ##### listing species names while executing

species_list <- sort(
  unique(
    occurrence_data$Name))

if (!is.null(SPECIES_SUBSET)) {
  unknown <- setdiff(SPECIES_SUBSET, species_list)
  if (length(unknown) > 0) stop("Species not in the occurrence file: ", paste(unknown, collapse = ", "))
  species_list <- SPECIES_SUBSET
  cat("SPECIES_SUBSET in use: only", length(species_list), "species are run\n")
}

cat("\n")
cat("============================================================\n")
cat(
  "NUMBER OF SPECIES:",
  length(species_list),
  "\n"
)
cat("============================================================\n")

print(species_list)

##### 12. ALL SPECIES ##### run the workflow for every species

all_results <- vector(
  "list",
  length(
    species_list))

names(all_results) <- species_list

for (
  i in seq_along(
    species_list)) {
  
  species_name <- species_list[i]
  
  cat("\n\n")
  cat("############################################################\n")
  cat(
    "SPECIES",
    i,
    "/",
    length(species_list),
    "\n"
  )
  cat(
    species_name,
    "\n"
  )
  cat("############################################################\n")
  
  
  all_results[[i]] <- tryCatch(
    
    {
      
      run_species(
        species_name = species_name,
        occurrence_data = occurrence_data,
        results_dir = results_dir,
        predictors_present = predictors_present,
        predictors_ssp126 = predictors_ssp126,
        predictors_ssp370 = predictors_ssp370,
        predictors_ssp585 = predictors_ssp585,
        predictors_subtropical = predictors_subtropical,
        n_pa = 5,
        k = 5,
        block_size = BLOCK_SIZE,
        models = c(
          "RF",
          "GBM"
        ),
        seed = 100)
    },
    
    error = function(e) {
      
      cat("\n")
      cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      cat(
        "ERROR IN:",
        species_name,
        "\n"
      )
      cat(
        "MESSAGE:",
        conditionMessage(e),
        "\n"
      )
      cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      
      return(
        
        list(
          species = species_name,
          error = conditionMessage(e)))
      
    }
    
  )
  
}

# Species that failed (the loop goes on after an error, so they are listed here)
failed <- names(all_results)[vapply(all_results, function(x) !is.null(x$error), logical(1))]
if (length(failed) > 0) {
  warning("The workflow FAILED for ", length(failed), " species: ", paste(failed, collapse = ", "),
          ". Their results are missing or incomplete; fix the error and run them again (SPECIES_SUBSET).")
} else {
  cat("\nAll", length(all_results), "species finished without errors.\n")
}

##### 13. GLOBAL RESULTS #####
# The global tables are built from the per-species files saved on disk. This also
# works when the spatial validation was skipped (RUN_SPATIAL_VALIDATION = FALSE) or
# when the R session was restarted after the species had finished.
cat("\n")
cat("============================================================\n")
cat("Building the global result tables from the per-species files.\n")
cat("============================================================\n")

species_list <- sort(list.dirs(results_dir, recursive = FALSE, full.names = FALSE))

evaluation_all_species <- bind_rows(lapply(species_list, function(sp) {
  f <- file.path(results_dir, sp, "01_spatial_validation", "evaluations_spatial.csv")
  if (!file.exists(f)) {
    cat("WARNING: missing file", f, "\n")
    return(NULL)
  }
  read.csv(f)
}))

variable_importance_all_species <- bind_rows(lapply(species_list, function(sp) {
  f <- file.path(results_dir, sp, "01_spatial_validation", "variable_importance_spatial.csv")
  if (!file.exists(f)) {
    cat("WARNING: missing file", f, "\n")
    return(NULL)
  }
  read.csv(f)
}))

# TSS thresholds of the final models: one row per species and replicate
TSS_threshold_all_species <- bind_rows(lapply(species_list, function(sp) {
  files <- list.files(
    file.path(results_dir, sp, "03_projections"),
    pattern = "_TSS_threshold\\.txt$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    cat("WARNING: no threshold files for", sp, "\n")
    return(NULL)
  }
  data.frame(
    Species = sp,
    algo = "RF",
    replicate = as.integer(sub(".*rep([0-9]+).*", "\\1", files)),
    TSS_threshold = vapply(files, function(f) as.numeric(readLines(f)[1]), numeric(1)),
    row.names = NULL) %>%
    arrange(replicate)
}))

write.csv(evaluation_all_species,
          file = file.path(results_dir, "evaluation_all_species.csv"),
          row.names = FALSE)
write.csv(variable_importance_all_species,
          file = file.path(results_dir, "variable_importance_all_species.csv"),
          row.names = FALSE)
write.csv(TSS_threshold_all_species,
          file = file.path(results_dir, "TSS_threshold_all_species.csv"),
          row.names = FALSE)

# Summaries of the spatial validation (only when its files exist)
if (nrow(evaluation_all_species) > 0) {
  summary_evaluation <- evaluation_all_species %>%
    filter(run != "allRun") %>%
    group_by(Species, algo, metric.eval) %>%
    summarise(mean = mean(validation, na.rm = TRUE), sd = sd(validation, na.rm = TRUE),
              min = min(validation, na.rm = TRUE), max = max(validation, na.rm = TRUE),
              n = sum(!is.na(validation)), .groups = "drop")

  write.csv(summary_evaluation,
            file = file.path(results_dir, "summary_evaluation.csv"),
            row.names = FALSE)

  summary_vars <- variable_importance_all_species %>%
    filter(run != "allRun") %>%
    group_by(Species, algo, expl.var) %>%
    summarise(mean = mean(var.imp, na.rm = TRUE), sd = sd(var.imp, na.rm = TRUE),
              min = min(var.imp, na.rm = TRUE), max = max(var.imp, na.rm = TRUE),
              n = sum(!is.na(var.imp)), .groups = "drop")

  write.csv(summary_vars,
            file = file.path(results_dir, "summary_vars.csv"),
            row.names = FALSE)
} else {
  cat("No spatial validation files found: summary_evaluation.csv and summary_vars.csv not written.\n")
}

# R and package versions of this run (for the README and the Methods)
writeLines(capture.output(sessionInfo()), file.path("results", "sessionInfo_04_SDMs.txt"))

cat("\nGlobal results built from disk. DONE.\n")
