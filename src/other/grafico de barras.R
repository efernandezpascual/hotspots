# ===============================================================
# INDEPENDENT BAR CHARTS FOR TWO SCENARIOS (Subtropical & Mediterranean)
# ALL CATEGORIES INCLUDED EVEN IF ZERO
# ===============================================================

# --- 0️⃣ Load libraries ---
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# --- 1️⃣ Read Excel files ---
presente <- read_excel("C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_presente_helechos.xlsx")

# Subtropical
subtropical_helechos <- read_excel("C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_subtropical_helechos.xlsx")
subtropical_invasoras <- read_excel("C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_subtropical_invasoras.xlsx")

# Mediterranean
mediterranea_helechos <- read_excel("C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_mediterranea_helechos.xlsx")
mediterranea_invasoras <- read_excel("C:/Users/JorgeCV/Desktop/TFM/Resultados/hotspots_por_pixel_mediterranea_invasoras.xlsx")

# --- 2️⃣ Define all categories as factor levels ---
niveles <- c(
  "Lost hotspots",
  "Maintained (ferns only)",
  "Maintained + invasive species",
  "Hotspot expansion (ferns only)",
  "Hotspot expansion + invasive species"
)

# --- 3️⃣ Function to create categories and count pixels ---
categorize_pixels <- function(presente, futuro_helechos, futuro_invasoras) {
  
  df <- presente %>%
    filter(hotspots_total >= 7) %>%
    left_join(futuro_helechos %>% rename(helechos = hotspots_total), by = c("x","y")) %>%
    left_join(futuro_invasoras %>% rename(invasoras = hotspots_total), by = c("x","y")) %>%
    mutate(categoria = case_when(
      is.na(helechos) | helechos == 0 ~ "Lost hotspots",
      helechos > 0 & (is.na(invasoras) | invasoras == 0) ~ "Maintained (ferns only)",
      helechos > 0 & invasoras > 0 ~ "Maintained + invasive species"
    ))
  
  expansion <- presente %>%
    filter(hotspots_total < 7) %>%
    left_join(futuro_helechos %>% rename(helechos = hotspots_total), by = c("x","y")) %>%
    left_join(futuro_invasoras %>% rename(invasoras = hotspots_total), by = c("x","y")) %>%
    filter(!is.na(helechos) & helechos >= 7) %>%
    mutate(categoria = case_when(
      is.na(invasoras) | invasoras == 0 ~ "Hotspot expansion (ferns only)",
      invasoras > 0 ~ "Hotspot expansion + invasive species"
    ))
  
  df_total <- bind_rows(df, expansion)
  
  conteo <- df_total %>%
    mutate(categoria = factor(categoria, levels = niveles)) %>%
    group_by(categoria) %>%
    summarise(pixels = n()) %>%
    ungroup() %>%
    complete(categoria = niveles, fill = list(pixels = 0))
  
  return(conteo)
}

# --- 4️⃣ Create pixel counts per scenario ---
conteo_subtropical <- categorize_pixels(presente, subtropical_helechos, subtropical_invasoras)
conteo_mediterranea <- categorize_pixels(presente, mediterranea_helechos, mediterranea_invasoras)

# --- 5️⃣ Define color palette ---
colores <- c(
  "Lost hotspots" = "#FF3B30",
  "Maintained (ferns only)" = "#32CD32",
  "Maintained + invasive species" = "#FFF700",
  "Hotspot expansion (ferns only)" = "#1E90FF",
  "Hotspot expansion + invasive species" = "#FF00FF"
)

# --- 6️⃣ Create bar charts ---
plot_subtropical <- ggplot(conteo_subtropical, aes(x = categoria, y = pixels, fill = categoria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores) +
  labs(x = "Category", y = "Number of pixels", title = "Subtropical Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

plot_mediterranea <- ggplot(conteo_mediterranea, aes(x = categoria, y = pixels, fill = categoria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores) +
  labs(x = "Category", y = "Number of pixels", title = "Mediterranean Scenario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# --- 7️⃣ Save plots ---
ggsave("C:/Users/JorgeCV/Desktop/TFM/Resultados/bar_chart_subtropical.png", plot_subtropical, width = 10, height = 6)
ggsave("C:/Users/JorgeCV/Desktop/TFM/Resultados/bar_chart_mediterranea.png", plot_mediterranea, width = 10, height = 6)

# --- 8️⃣ Show plots ---
print(plot_subtropical)
print(plot_mediterranea)