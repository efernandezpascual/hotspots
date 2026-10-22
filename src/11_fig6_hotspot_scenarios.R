# =============================================================================
# Figure 6 - Native hotspots under future scenarios
# =============================================================================
# Manuscript : Subtropical refugia in Atlantic Europe become invasion hotspots
#              under climate change
# Script     : 11_fig6_hotspot_scenarios.R
#
# Purpose
#   Three bar charts, side by side, of the native hotspots (cells with at least
#   HOTSPOT_THRESHOLD of the nine native species), one bar per future scenario:
#   (a) Change in the area of the native hotspots with respect to the present (%).
#   (b) % of the present-day native hotspot cells that are no longer hotspot.
#   (c) Change in the mean number of the nine invasive species in the PRESENT-DAY native
#       hotspot cells (the same cells in all scenarios), with respect to the present:
#       exposure of the current refugia to the invasive species. The same metric in the
#       hotspot cells of each scenario, which move between scenarios, is reported in
#       Supporting Information S3.
#   Panel (b) refers only to the present-day hotspot cells: cells that become native
#   hotspots in the future are not counted, because climatic suitability in new cells
#   does not imply colonisation.
#
# Replicates
#   The final model of each species was fitted several times (replicates, 04_SDMs.R).
#   Every metric is calculated separately with each replicate (replicate r of all species),
#   and the changes are calculated within each replicate (paired: the future minus the
#   present of the same replicate). Bars are the mean of the replicates and each open circle
#   is one replicate. Because the changes are paired, circles that are all on the same side
#   of zero show a change that does not depend on the randomness of the models, even if the
#   levels differ a lot between replicates (see Supporting Information S3 for the levels).
#
# Inputs
#   results/SDMs/<species>/03_projections/rep<r>/binary/   (04_SDMs.R)
#
# Output
#   results/figures/fig6_hotspot_scenarios.png   (170 x 120 mm, 600 dpi)
#   The mean values in the present are printed to the console (for the figure caption).
#
# Requires
#   terra, dplyr, ggplot2, patchwork; run from the project root.
# =============================================================================

source("src/figure_common.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(patchwork)
})

# ---- Settings ---------------------------------------------------------------
FIG_HEIGHT_MM <- 120

# ---- Metrics of each replicate; changes with respect to the present of the same replicate
m <- hotspot_metrics() %>%
  group_by(version) %>%
  mutate(area_change = 100 * (native_km2 / native_km2[scenario == "present"] - 1),
         fixed_change = inv_fixed - inv_fixed[scenario == "present"]) %>%
  ungroup()

present <- m %>% filter(scenario == "present")
cat("Present-day native hotspots (mean of the replicates): area =", round(mean(present$native_km2)),
    "km2; invasive species per hotspot cell =", round(mean(present$mean_inv), 2), "\n")

m <- m %>% filter(scenario %in% FUTURE_KEYS)
m$scenario <- scenario_factor(m$scenario, FUTURE_KEYS)

# ---- One bar chart per metric (bar = mean of the replicates, circle = one replicate) ----
bar_panel <- function(metric, fill, ylab, title, limits = NULL, zero_line = TRUE) {

  d <- m
  d$value <- d[[metric]]
  bars <- d %>% group_by(scenario) %>% summarise(value = mean(value), .groups = "drop")

  ggplot() +
    geom_col(data = bars, aes(x = scenario, y = value),
             width = 0.6, fill = fill, colour = "black", linewidth = LINE_WIDTH) +
    {if (zero_line) geom_hline(yintercept = 0, linewidth = LINE_WIDTH)} +
    geom_point(data = d, aes(x = scenario, y = value),
               shape = 21, fill = "white", colour = "black", size = 1.8, stroke = LINE_WIDTH + 0.1,
               position = position_jitter(width = 0.1, height = 0, seed = 1)) +
    scale_y_continuous(expand = expansion(mult = c(0.06, 0.06)), limits = limits) +
    labs(x = NULL, y = ylab) +
    ggtitle(title) +
    theme_fig() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
}

panel_a <- bar_panel("area_change", COL[["native_hotspot"]],
                     "Change in the area of the native hotspots (%)",
                     "(a)")

panel_b <- bar_panel("lost_pct", COL[["lost"]],
                     "Present-day native hotspot cells lost (%)",
                     "(b)", limits = c(0, 100), zero_line = FALSE)

panel_c <- bar_panel("fixed_change", COL[["invasive_hotspot"]],
                     "Change in invasive species\nin present-day native hotspot cells",
                     "(c)")

# ---- Combine and save -------------------------------------------------------
fig6 <- panel_a | panel_b | panel_c

save_figure(fig6, "fig6_hotspot_scenarios", height_mm = FIG_HEIGHT_MM)
