# =============================================================================
# variable_selection_common.R
# Shared definitions and functions of the variable selection, used by
# 02_variable_selection.R and by Supporting Information S1
# (results/supplementary/S1_variable_selection_supplementary.Rmd).
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
#
# Purpose
#   Keeps the variable selection in ONE place, so that the console script and
#   the supplementary document always report the same analysis and the same
#   reasons. All the correlation values quoted in the reasons for excluding a
#   variable are computed from the data, not written by hand.
#
# Usage
#   Not run on its own. Sourced after figure_common.R (which defines
#   PROJECT_ROOT); paths are relative to PROJECT_ROOT.
#
# Requires
#   terra, dplyr
# =============================================================================

if (!exists("PROJECT_ROOT")) PROJECT_ROOT <- "."

# ---- Candidate variables (26) ------------------------------------------------
VS_R_THRESHOLD    <- 0.7   # |r| above which two variables are considered collinear
VS_BIO_VARS       <- paste0("bio", sprintf("%02d", 1:19))
VS_SOIL_VARS      <- c("ph", "sand")
VS_BIOCLIM_PLUS   <- c("clt", "hurs", "vpd", "cmi", "fcf")   # CHELSA-BIOCLIM+ candidates
# CHELSA-BIOCLIM+ "time-series" variables: available only for 1980-2018 (Brun et al.
# 2022), with no future-scenario layers, so they cannot enter the projective models.
# fcf is a "projected" variable (future layers exist) and is excluded on other grounds.
VS_NO_FUTURE      <- c("clt", "hurs", "vpd", "cmi")
VS_CANDIDATE_VARS <- c(VS_BIO_VARS, VS_SOIL_VARS, VS_BIOCLIM_PLUS)

# Final predictor set (decision of the authors; justified in selection_decision())
VS_SELECTED <- c("bio06", "bio14", "ph")

# ---- Values of the candidate variables at the occurrence records --------------
# Returns a list with:
#   data          id, species, source and the 26 variables (complete records only)
#   values        the 26 variables only (input of the correlation matrix and PCA)
#   n_incomplete  records without a value in some variable (excluded)
#   min_cells   number of grid cells with presence of the species with the fewest
#                 (the models use one presence per cell)
load_candidate_values <- function(
    layers_dir       = file.path(PROJECT_ROOT, "data/raster/present"),
    occurrences_file = file.path(PROJECT_ROOT, "data/species-points.csv")) {

  layer_files   <- file.path(layers_dir, paste0(VS_CANDIDATE_VARS, "_present.tif"))
  missing_files <- VS_CANDIDATE_VARS[!file.exists(layer_files)]
  if (length(missing_files) > 0) {
    stop("Missing layers: ", paste(missing_files, collapse = ", "),
         " - check ", layers_dir, " (run 01_present_layers.R)")
  }

  layers <- terra::rast(layer_files)
  names(layers) <- VS_CANDIDATE_VARS

  occurrences <- read.csv(occurrences_file)
  points      <- terra::vect(occurrences, geom = c("longitude", "latitude"),
                             crs = terra::crs(layers))
  extracted   <- terra::extract(layers, points, ID = FALSE)

  data <- dplyr::bind_cols(
    dplyr::select(occurrences, id, species, source), extracted)

  complete <- stats::complete.cases(data)

  list(
    data         = data[complete, ],
    values       = data[complete, VS_CANDIDATE_VARS],
    n_incomplete = sum(!complete),
    min_cells  = min(tapply(
      terra::cellFromXY(layers, as.matrix(occurrences[complete, c("longitude", "latitude")])),
      data$species[complete], function(x) length(unique(x))))
  )
}

# ---- Extrapolation of a candidate variable in the future scenarios ------------
# Percentage of the region cells where the future value (2071-2100, GCM ensemble
# written by 03_future_layers.R) is above the present-day maximum in the region.
# Returns a list with the present-day maximum and a named vector of percentages
# (NA when the future layer is missing).
future_extrapolation <- function(v, ssps = c("ssp126", "ssp370", "ssp585")) {
  present <- terra::rast(file.path(PROJECT_ROOT, "data/raster/present", paste0(v, "_present.tif")))
  hi      <- as.numeric(terra::global(present, "max", na.rm = TRUE)[1, 1])
  n_cells <- as.numeric(terra::global(!is.na(present), "sum", na.rm = TRUE)[1, 1])
  pct <- vapply(ssps, function(ssp) {
    f <- file.path(PROJECT_ROOT, "data/raster/future", paste0(v, "_", ssp, "_2071-2100.tif"))
    if (!file.exists(f)) return(NA_real_)
    100 * as.numeric(terra::global(terra::rast(f) > hi, "sum", na.rm = TRUE)[1, 1]) / n_cells
  }, numeric(1))
  list(present_max = hi, pct_above = pct)
}

# ---- Pairs of variables with |r| > threshold -----------------------------------
correlation_pairs <- function(cor_matrix, threshold = VS_R_THRESHOLD) {
  idx <- which(abs(cor_matrix) > threshold & upper.tri(cor_matrix), arr.ind = TRUE)
  out <- data.frame(
    var1 = rownames(cor_matrix)[idx[, 1]],
    var2 = colnames(cor_matrix)[idx[, 2]],
    r    = cor_matrix[idx],
    stringsAsFactors = FALSE
  )
  out$includes_no_future <- out$var1 %in% VS_NO_FUTURE | out$var2 %in% VS_NO_FUTURE
  out <- out[order(-abs(out$r)), ]
  rownames(out) <- NULL
  out
}

# ---- Reasons for the final selection -------------------------------------------
# Builds the text of the final decision from the correlation matrix and the PCA.
# Returns a list with:
#   selected  the final predictors
#   retained  named list: one sentence per final predictor (why it is included)
#   excluded  named list: one sentence per excluded variable or group
#   flags     messages for the authors when the data no longer support a sentence
#             as written (printed by 02_variable_selection.R; not shown in S1)
selection_decision <- function(cor_matrix, pca, min_cells,
                               extrapolation_bio10 = future_extrapolation("bio10"),
                               selected = VS_SELECTED, threshold = VS_R_THRESHOLD) {

  flags <- character(0)
  rr    <- function(v1, v2) cor_matrix[v1, v2]
  fr    <- function(v1, v2) sprintf("%.3f", rr(v1, v2))

  # "a (r = x), b (r = y) and c (r = z)": strongest correlations of v
  strongest <- function(v, among = setdiff(colnames(cor_matrix), v), n = 3,
                        only_above = TRUE) {
    r <- cor_matrix[v, among]
    r <- r[order(-abs(r))]
    if (only_above) r <- r[abs(r) > threshold]
    r <- head(r, n)
    if (length(r) == 0) return(NA_character_)
    items <- sprintf("%s (r = %.3f)", names(r), r)
    if (length(items) == 1) items else
      paste(paste(items[-length(items)], collapse = ", "), "and", items[length(items)])
  }

  # Maximum |r| of v with the final predictors, as text
  max_with_selected <- function(v) {
    r <- cor_matrix[v, setdiff(selected, v)]
    k <- which.max(abs(r))
    sprintf("%s (r = %.3f)", names(r)[k], r[k])
  }

  no_future <- "In addition, CHELSA-BIOCLIM+ provides it only for the period 1980-2018, with no future-scenario layers, so it cannot be used in the projective models."

  # Check: the final predictors must not be collinear among themselves
  sel_r <- cor_matrix[selected, selected]
  if (any(abs(sel_r[upper.tri(sel_r)]) > threshold)) {
    flags <- c(flags, "Some final predictors are collinear among themselves (|r| > threshold).")
  }

  # --- Reasons for the final predictors ---------------------------------------
  retained <- list()
  # All the candidate variables collinear with v (not only the strongest ones)
  cluster <- function(v) {
    top <- strongest(v, n = Inf)
    if (is.na(top)) "It is not collinear with any other candidate variable." else
      sprintf("It represents the group of candidate variables collinear with it: %s.", top)
  }

  # Candidate with future layers (BIO variables) most correlated with vpd
  vpd_r     <- cor_matrix["vpd", VS_BIO_VARS]
  vpd_proxy <- names(vpd_r)[which.max(abs(vpd_r))]
  if (vpd_proxy != "bio10") {
    flags <- c(flags, sprintf(
      "The BIO variable most correlated with vpd is %s (r = %.3f), not bio10 (r = %.3f): revise the reason for evaluating bio10 as its proxy.",
      vpd_proxy, vpd_r[vpd_proxy], vpd_r["bio10"]))
  }

  retained$bio06 <- paste(
    "Minimum temperature of the coldest month: frost limit, the main constraint",
    "expected for subtropical relict ferns and for frost-sensitive invasive species.",
    cluster("bio06"))
  retained$bio14 <- paste(
    "Precipitation of the driest month: summer drought, the moisture constraint",
    "expected to limit the distributions in this oceanic region.",
    cluster("bio14"))
  retained$ph <- paste(
    "Topsoil pH: the edaphic factor expected to limit this group of ferns (acidic",
    "siliceous substrates).",
    cluster("ph"))

  excluded <- list()

  # --- bio07 ------------------------------------------------------------------
  pc2 <- pca$rotation[, "PC2"]
  opposite_pc2 <- sign(pc2["bio06"]) != sign(pc2["bio07"])
  excluded$bio07 <- paste0(
    sprintf("Collinear with bio06 (r = %s)", fr("bio06", "bio07")),
    if (opposite_pc2) sprintf(
      ", and both load on the second PCA axis with opposite signs (PC2 loadings: bio06 = %.3f, bio07 = %.3f)",
      pc2["bio06"], pc2["bio07"]) else "",
    ": they represent the same climatic dimension (oceanicity / winter cold), not two ",
    "independent axes. bio06 is retained because of its more direct physiological ",
    "interpretation (frost limit).")
  if (abs(rr("bio06", "bio07")) <= threshold) {
    flags <- c(flags, sprintf("bio07 is no longer collinear with bio06 (r = %s): revise its reason.", fr("bio06", "bio07")))
  }

  # --- bio10 ------------------------------------------------------------------
  ex <- extrapolation_bio10
  if (any(is.na(ex$pct_above))) {
    flags <- c(flags, "Future bio10 layers missing (run 03_future_layers.R): the extrapolation of bio10 cannot be reported.")
  }
  excluded$bio10 <- paste(
    "Mean temperature of the warmest quarter (summer heat). Of the candidate",
    "variables with future-scenario layers, it is the one most correlated with vpd",
    sprintf("(r = %s), so it was evaluated as a proxy of summer atmospheric moisture", fr("bio10", "vpd")),
    sprintf(paste(
      "demand. It was excluded because its future values (2071-2100, ensemble of the",
      "GCMs) exceed its present-day maximum in the study region (%.2f \u00b0C) in",
      "%.0f%%, %.0f%% and %.0f%% of the region cells under SSP1-2.6, SSP3-7.0 and",
      "SSP5-8.5, respectively: including it would force the models to extrapolate beyond",
      "the calibrated range over most of the study area in the higher-emission scenarios,",
      "where Random Forest would simply hold the response of the warmest present-day",
      "conditions. Summer warming is therefore not represented in the models, a",
      "limitation acknowledged in the main text."),
      ex$present_max, ex$pct_above[1], ex$pct_above[2], ex$pct_above[3]))

  # --- sand -------------------------------------------------------------------
  if (abs(rr("ph", "sand")) > threshold) {
    excluded$sand <- sprintf(
      "Collinear with ph (r = %s). ph is retained because of its more direct physiological relevance for this group of ferns (acidic siliceous substrates).",
      fr("ph", "sand"))
  } else {
    # Not collinear with ph: report what sand does correlate with
    sand_top <- strongest("sand")                       # candidates with |r| > threshold
    sand_sel <- cor_matrix["sand", setdiff(selected, "ph")]
    k        <- which.max(abs(sand_sel))
    near_sel <- abs(sand_sel[k]) > threshold - 0.1      # close to the threshold
    excluded$sand <- paste0(
      sprintf("Not collinear with ph (r = %s). ", fr("ph", "sand")),
      if (!is.na(sand_top)) sprintf(
        "At the occurrence points, sand content is collinear with %s%s, so it largely tracks the climatic gradient already represented by the final predictors rather than an independent edaphic dimension. ",
        sand_top,
        if (near_sel) sprintf(" and close to the threshold with %s (r = %.3f)", names(sand_sel)[k], sand_sel[k]) else "")
      else "",
      "ph is retained as the only edaphic predictor because it represents the soil ",
      "factor expected to limit this group of ferns (acidic siliceous substrates), and ",
      sprintf("the number of predictors is constrained by the smallest samples (%d grid cells with presence).",
              min_cells))
    if (is.na(sand_top)) {
      flags <- c(flags, sprintf(
        "sand is not collinear with ph (r = %s) nor with any other candidate: its exclusion rests only on ecological grounds and parsimony. Check that the authors agree, or include sand.",
        fr("ph", "sand")))
    }
  }

  # --- CHELSA-BIOCLIM+ variables ----------------------------------------------
  bioclim_plus_reason <- function(v, extra = "") {
    top <- strongest(v)
    first <- if (!is.na(top)) {
      best_sel <- max_with_selected(v)
      # the partner among the final predictors is omitted if already listed
      if (startsWith(top, sub(" .*", "", best_sel)) ||
          grepl(paste0(sub(" .*", "", best_sel), " \\("), top)) {
        sprintf("Collinear with %s.", top)
      } else {
        sprintf("Collinear with %s; maximum |r| with the final predictors: %s.", top, best_sel)
      }
    } else {
      sprintf("Not collinear with any candidate variable (strongest correlation: %s).",
              strongest(v, n = 1, only_above = FALSE))
    }
    paste(c(first, if (nzchar(extra)) extra, if (v %in% VS_NO_FUTURE) no_future),
          collapse = " ")
  }

  excluded$cmi  <- bioclim_plus_reason("cmi")
  excluded$vpd  <- bioclim_plus_reason(
    "vpd",
    sprintf("Its closest proxy with future-scenario layers, %s, was evaluated and also excluded (see %s).", vpd_proxy, vpd_proxy))
  excluded$clt  <- bioclim_plus_reason("clt")
  excluded$hurs <- bioclim_plus_reason(
    "hurs",
    "Atmospheric humidity is ecologically relevant for these ferns (in particular the filmy ferns), so the absence of relative humidity, fog and summer moisture demand from the models is acknowledged as a limitation in the main text.")
  excluded$fcf  <- bioclim_plus_reason(
    "fcf",
    "Future-scenario layers are available for it, but it lacks an ecological interpretation for this group: it measures the frequency of freeze-thaw events, relevant in periglacial and alpine systems, with no plausible mechanism for ferns of coastal ravines where frosts are infrequent; the frost limit is already represented by bio06.")

  # --- Other BIO variables ----------------------------------------------------
  other_bio <- setdiff(VS_BIO_VARS, c(selected, names(excluded)))
  clim_sel <- intersect(selected, VS_BIO_VARS)

  # Partner of v among `refs` (|r| above the threshold), as text; NA if none
  partner_of <- function(v, refs) {
    r <- cor_matrix[v, refs, drop = TRUE]
    names(r) <- refs
    k <- which.max(abs(r))
    if (abs(r[k]) > threshold) sprintf("%s (with %s, r = %.3f)", v, names(r)[k], r[k]) else NA_character_
  }

  # First the final climatic predictors; the remaining variables are compared with
  # bio10 (excluded above because of extrapolation)
  partner_sel   <- vapply(other_bio, partner_of, character(1), refs = clim_sel)
  partner_bio10 <- vapply(other_bio, function(v)
    if (is.na(partner_sel[v])) partner_of(v, "bio10") else NA_character_, character(1))

  redundant       <- other_bio[!is.na(partner_sel)]
  redundant_bio10 <- other_bio[!is.na(partner_bio10)]
  not_redundant   <- other_bio[is.na(partner_sel) & is.na(partner_bio10)]

  # Are the non-redundant variables collinear with some other (excluded) candidate?
  collinear_other <- vapply(not_redundant, function(v) {
    any(abs(cor_matrix[v, setdiff(colnames(cor_matrix), v)]) > threshold)
  }, logical(1))
  how_many <- if (all(collinear_other)) "all of them are" else
    if (mean(collinear_other) > 0.5) "most of them are" else "some of them are"

  join <- function(x) if (length(x) <= 1) x else
    paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])

  excluded$other_bio <- paste0(
    if (length(redundant) > 0) sprintf(
      "%s %s collinear with a final climatic predictor: %s. ",
      join(redundant), if (length(redundant) == 1) "is" else "are",
      join(partner_sel[!is.na(partner_sel)])) else "",
    if (length(redundant_bio10) > 0) sprintf(
      "%s %s collinear with bio10 (%s) and represent%s the same summer-heat dimension, so %s excluded for the same reason as bio10. ",
      join(redundant_bio10), if (length(redundant_bio10) == 1) "is" else "are",
      join(sprintf("%s: r = %.3f", redundant_bio10, cor_matrix[redundant_bio10, "bio10"])),
      if (length(redundant_bio10) == 1) "s" else "",
      if (length(redundant_bio10) == 1) "it is" else "they are") else "",
    if (length(not_redundant) > 0) sprintf(paste0(
      "%s %s not collinear with the final predictors (although %s collinear ",
      "with other excluded candidates; see Table 1). They were not included because (i) the ",
      "models are built around the two climatic limits hypothesised for this ",
      "assemblage of subtropical species, winter frost (bio06) and summer drought ",
      "(bio14), and these two dimensions must be separate predictors for the ",
      "'Subtropical' scenario (future winter temperature with present-day summer ",
      "precipitation) to be defined; (ii) wet-season and annual precipitation are high across this oceanic ",
      "region, so the precipitation of the driest month is the moisture variable ",
      "expected to limit the distributions; and (iii) the number of predictors is ",
      "constrained by the smallest samples (%d grid cells with presence)."),
      join(not_redundant), if (length(not_redundant) == 1) "is" else "are",
      how_many, min_cells) else ""
  )

  list(selected = selected, retained = retained, excluded = excluded, flags = flags)
}

# Label of each excluded entry in the printed / knitted decision
decision_label <- function(key) if (key == "other_bio") "Other BIO variables" else key
