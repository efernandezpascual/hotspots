# =============================================================================
# Step 2 - Selection of the predictor variables
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
# Script     : 02_variable_selection.R
#
# Purpose
#   Selects the predictor variables of the species distribution models among 26
#   candidates (19 BIO variables, pH, sand, and five CHELSA-BIOCLIM+ variables, four of
#   which have no future-scenario layers):
#   (1) extracts the candidate variables at the occurrence records;
#   (2) lists the pairs of variables with |r| > 0.7;
#   (3) runs a Principal Component Analysis (PCA) of the candidates;
#   (4) prints the final decision, the reason for including each final predictor
#       and the reason for excluding each other variable,
#       with the correlation values computed from the data, and warns if the
#       data no longer support one of the reasons as written.
#   The analysis and the reasons are defined in variable_selection_common.R and
#   are shared with Supporting Information S1
#   (results/supplementary/S1_variable_selection_supplementary.Rmd), so both
#   always report the same results.
#
# Inputs
#   data/raster/present/<variable>_present.tif   26 layers (01_present_layers.R)
#   data/species-points.csv                      columns: id, species, source,
#                                                longitude, latitude
#
# Output
#   None. The results are printed to the console and the biplot is shown in the
#   plot window (only in an interactive session); nothing is saved. The objects
#   `cor_matrix`, `pairs_table`, `pca` and `decision` stay in the R session.
#
# Requires
#   terra, dplyr, ggplot2 (only for the biplot); run from the project root.
# =============================================================================

source("src/figure_common.R")               # species lists, colours, theme
source("src/variable_selection_common.R")   # candidates, analysis and decision

# ---- 1. Values of the candidate variables at the occurrence records ---------
candidates   <- load_candidate_values()
pca_data     <- candidates$data
value_matrix <- candidates$values

cat("\n=== CANDIDATE VARIABLES AND RECORDS ===\n")
cat(length(VS_CANDIDATE_VARS), "candidate variables:", paste(VS_CANDIDATE_VARS, collapse = ", "), "\n")
cat(nrow(pca_data), "occurrence records,", length(unique(pca_data$species)), "species",
    "(smallest sample:", candidates$min_cells, "grid cells with presence)\n")
if (candidates$n_incomplete > 0) {
  cat(candidates$n_incomplete,
      "records without a value in some variable (outside the region mask) were excluded\n")
}

# ---- 2. Correlation matrix and pairs with |r| > threshold -------------------
cor_matrix  <- cor(value_matrix, use = "pairwise.complete.obs")
pairs_table <- correlation_pairs(cor_matrix)

cat("\n=== PAIRS OF VARIABLES WITH |r| >", VS_R_THRESHOLD, "===\n")
cat("(includes_no_future = TRUE: the pair involves a variable with no future layer)\n\n")
print(transform(pairs_table, r = round(r, 3)), row.names = FALSE)

# ---- 3. PCA ------------------------------------------------------------------
pca <- prcomp(value_matrix, scale. = TRUE, center = TRUE)

cat("\n=== PCA: VARIANCE EXPLAINED (first 5 components) ===\n")
print(round(summary(pca)$importance[, 1:5], 3))

cat("\n=== PCA: LOADINGS (first 3 components) ===\n")
print(round(pca$rotation[, 1:3], 3))

# ---- 4. Final decision -------------------------------------------------------
decision <- selection_decision(cor_matrix, pca, candidates$min_cells)

cat("\n=== FINAL DECISION ===\n")
cat("Selected predictors:", paste(sub("^PH$", "pH", toupper(decision$selected)), collapse = ", "), "\n")
cat("Correlations among the selected predictors:\n")
print(round(cor_matrix[decision$selected, decision$selected], 3))
cat("\nReasons for the selected predictors:\n\n")
for (v in names(decision$retained)) {
  cat("* ", v, ": ", sep = "")
  cat(strwrap(decision$retained[[v]], width = 90, exdent = 4), sep = "\n")
  cat("\n")
}
cat("\nThe remaining", length(VS_CANDIDATE_VARS) - length(decision$selected),
    "candidate variables are excluded for the following reasons:\n\n")
for (v in names(decision$excluded)) {
  cat("* ", decision_label(v), ": ", sep = "")
  cat(strwrap(decision$excluded[[v]], width = 90, exdent = 4), sep = "\n")
  cat("\n")
}

if (length(decision$flags) > 0) {
  cat("\n!!! REVIEW BEFORE ACCEPTING THE SELECTION !!!\n")
  for (f in decision$flags) cat(strwrap(paste("-", f), width = 90, exdent = 2), sep = "\n")
  cat("\n")
}

# ---- 5. Biplot (plot window only; not saved) --------------------------------
if (interactive()) {

  scores <- as.data.frame(pca$x[, 1:2])
  scores$group <- group_factor(pca_data$species %in% species_label(NATIVE_SPECIES))
  explained <- summary(pca)$importance[2, 1:2] * 100

  loadings <- as.data.frame(pca$rotation[, 1:2]) * max(abs(scores[, 1:2])) * 0.8
  loadings$variable <- rownames(loadings)
  loadings$type <- ifelse(loadings$variable %in% VS_NO_FUTURE,
                          "No future layer (not usable in projections)", "BIO / soil")

  print(
    ggplot() +
      geom_point(data = scores, aes(x = PC1, y = PC2, colour = group), alpha = 0.3, size = 1) +
      stat_ellipse(data = scores, aes(x = PC1, y = PC2, colour = group), linewidth = 0.8) +
      geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2, linetype = type),
                   arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
      geom_text(data = loadings, aes(x = PC1, y = PC2, label = variable),
                colour = "black", size = 3, vjust = -0.5) +
      scale_colour_manual(values = GROUP_COLOURS, name = NULL) +
      scale_linetype_manual(values = c("BIO / soil" = "solid",
                                       "No future layer (not usable in projections)" = "dashed"),
                            name = NULL) +
      labs(x = sprintf("PC1 (%.1f%%)", explained[1]), y = sprintf("PC2 (%.1f%%)", explained[2])) +
      guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2.5))) +
      theme_fig()
  )
}

cat("\nDone. Nothing was saved; see Supporting Information S1 for the tables and figure.\n")
