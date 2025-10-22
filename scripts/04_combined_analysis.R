# ==============================================================================
# Script: Combined Analysis - Exports and FDI
# Countries: Philippines, Vietnam, Thailand
# Purpose: Analyze two dimensions of economic openness
# ==============================================================================

# Load setup
source(here::here("scripts", "00_setup.R"))

cat("Creating combined analysis of Economic Openness...\n")
cat("Dimensions: Exports and FDI\n")
cat("Countries: Philippines, Vietnam, Thailand\n")
cat(rep("-", 60), "\n", sep = "")

# ==============================================================================
# Load the processed data
# ==============================================================================

cat("\nLoading processed data...\n")

# Load exports data
exports_file <- here("data", "exports_wdi_comparison.csv")
if (!file.exists(exports_file)) {
  stop("❌ Exports data not found. Please run script 01b first.")
}
exports_data <- read_csv(exports_file, show_col_types = FALSE)
cat("✓ Exports data loaded:", nrow(exports_data), "observations\n")

# Load FDI data (with % GDP)
fdi_file <- here("data", "fdi_unctad_comparison_pct_gdp.csv")
if (!file.exists(fdi_file)) {
  # Try without % GDP version
  fdi_file <- here("data", "fdi_unctad_comparison.csv")
  if (!file.exists(fdi_file)) {
    stop("❌ FDI data not found. Please run script 03 first.")
  }
}
fdi_data <- read_csv(fdi_file, show_col_types = FALSE)
cat("✓ FDI data loaded:", nrow(fdi_data), "observations\n\n")

# ==============================================================================
# Combine datasets
# ==============================================================================

cat("Combining exports and FDI data...\n")

combined_data <- exports_data %>%
  full_join(fdi_data, by = c("country", "year")) %>%
  arrange(country, year)

cat("✓ Combined dataset created:", nrow(combined_data), "observations\n")
cat("Countries:", paste(unique(combined_data$country), collapse = ", "), "\n")
cat("Year range:", min(combined_data$year, na.rm = TRUE), "-", 
    max(combined_data$year, na.rm = TRUE), "\n\n")

# Save combined dataset
output_file <- here("data", "economic_openness_combined.csv")
write_csv(combined_data, output_file)
cat("✓ Combined data saved to:", output_file, "\n\n")

# Define consistent colors
country_colors <- c(
  "Philippines" = "#2E86AB",
  "Vietnam" = "#A23B72",
  "Thailand" = "#F18F01"
)

# ==============================================================================
# Visualization 1: Dual-axis plot (Exports % GDP + FDI Inward % GDP)
# ==============================================================================

cat("Creating Visualization 1: Dual indicators by country...\n")

# Prepare data for dual plot
dual_data <- combined_data %>%
  filter(!is.na(exports_pct_gdp) & !is.na(fdi_inward_pct_gdp)) %>%
  select(country, year, exports_pct_gdp, fdi_inward_pct_gdp) %>%
  pivot_longer(
    cols = c(exports_pct_gdp, fdi_inward_pct_gdp),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  mutate(
    indicator = recode(indicator,
                       "exports_pct_gdp" = "Exports (% GDP)",
                       "fdi_inward_pct_gdp" = "FDI Inflows (% GDP)")
  )

plot_dual_indicators <- ggplot(dual_data, 
                               aes(x = year, y = value, 
                                   color = indicator, 
                                   linetype = indicator)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~country, ncol = 1, scales = "free_y") +
  scale_color_manual(
    values = c("Exports (% GDP)" = "#2E86AB", 
               "FDI Inflows (% GDP)" = "#A23B72"),
    name = "Indicator"
  ) +
  scale_linetype_manual(
    values = c("Exports (% GDP)" = "solid", 
               "FDI Inflows (% GDP)" = "dashed"),
    name = "Indicator"
  ) +
  labs(
    title = "Economic Openness: Exports and FDI Inflows",
    subtitle = paste0(start_year, " - ", end_year),
    x = "Year",
    y = "% of GDP",
    caption = "Source: World Bank (Exports), UNCTAD (FDI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "economic_openness_dual_faceted.png"),
  plot_dual_indicators,
  width = 12,
  height = 10,
  dpi = 300
)

print(plot_dual_indicators)

# ==============================================================================
# Visualization 2: Scatter plot - Exports vs FDI
# ==============================================================================

cat("Creating Visualization 2: Scatter plot Exports vs FDI...\n")

scatter_data <- combined_data %>%
  filter(!is.na(exports_pct_gdp) & !is.na(fdi_inward_pct_gdp))

plot_scatter <- ggplot(scatter_data, 
                       aes(x = exports_pct_gdp, y = fdi_inward_pct_gdp, 
                           color = country)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = country_colors, name = "Country") +
  labs(
    title = "Relationship between Exports and FDI Inflows",
    subtitle = paste0(start_year, " - ", end_year),
    x = "Exports of Goods and Services (% of GDP)",
    y = "FDI Inflows (% of GDP)",
    caption = "Source: World Bank (Exports), UNCTAD (FDI)\nNote: Each point represents one year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "exports_fdi_scatter.png"),
  plot_scatter,
  width = 12,
  height = 8,
  dpi = 300
)

print(plot_scatter)

# ==============================================================================
# Visualization 3: Combined line chart (All countries, both indicators)
# ==============================================================================

cat("Creating Visualization 3: Combined comparison all countries...\n")

plot_combined_comparison <- ggplot(dual_data, 
                                   aes(x = year, y = value, 
                                       color = country, 
                                       linetype = indicator)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = country_colors, name = "Country") +
  scale_linetype_manual(
    values = c("Exports (% GDP)" = "solid", 
               "FDI Inflows (% GDP)" = "dashed"),
    name = "Indicator"
  ) +
  labs(
    title = "Economic Openness Indicators: Regional Comparison",
    subtitle = paste0(start_year, " - ", end_year),
    x = "Year",
    y = "% of GDP",
    caption = "Source: World Bank (Exports), UNCTAD (FDI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "economic_openness_combined_lines.png"),
  plot_combined_comparison,
  width = 12,
  height = 7,
  dpi = 300
)

print(plot_combined_comparison)

# ==============================================================================
# Visualization 4: Philippines Focus with Benchmarks
# ==============================================================================

cat("Creating Visualization 4: Philippines with benchmarks...\n")

plot_philippines_focus <- ggplot(dual_data, 
                                 aes(x = year, y = value, 
                                     color = country, 
                                     linetype = indicator)) +
  geom_line(aes(size = country == "Philippines", 
                alpha = country == "Philippines")) +
  scale_color_manual(values = country_colors, name = "Country") +
  scale_linetype_manual(
    values = c("Exports (% GDP)" = "solid", 
               "FDI Inflows (% GDP)" = "dashed"),
    name = "Indicator"
  ) +
  scale_size_manual(values = c("TRUE" = 1.3, "FALSE" = 0.7), guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5), guide = "none") +
  labs(
    title = "Philippines Economic Openness with Regional Benchmarks",
    subtitle = paste0(start_year, " - ", end_year),
    x = "Year",
    y = "% of GDP",
    caption = "Source: World Bank (Exports), UNCTAD (FDI)\nNote: Philippines shown in bold"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "philippines_openness_highlighted.png"),
  plot_philippines_focus,
  width = 12,
  height = 7,
  dpi = 300
)

print(plot_philippines_focus)

# ==============================================================================
# Visualization 5: Heatmap of Economic Openness Over Time
# ==============================================================================

cat("Creating Visualization 5: Heatmap by decade...\n")

# Calculate averages by decade
decade_data <- combined_data %>%
  filter(!is.na(exports_pct_gdp) | !is.na(fdi_inward_pct_gdp)) %>%
  mutate(decade = floor(year / 10) * 10) %>%
  group_by(country, decade) %>%
  summarise(
    avg_exports = mean(exports_pct_gdp, na.rm = TRUE),
    avg_fdi = mean(fdi_inward_pct_gdp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(avg_exports, avg_fdi),
    names_to = "indicator",
    values_to = "value"
  ) %>%
  mutate(
    indicator = recode(indicator,
                       "avg_exports" = "Exports",
                       "avg_fdi" = "FDI Inflows"),
    decade_label = paste0(decade, "s")
  )

plot_heatmap <- ggplot(decade_data, 
                       aes(x = decade_label, y = indicator, fill = value)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(value, 1)), color = "white", 
            fontface = "bold", size = 3) +
  facet_wrap(~country, ncol = 3) +
  scale_fill_gradient(low = "#3A506B", high = "#F18F01", 
                      name = "Avg % of GDP") +
  labs(
    title = "Economic Openness by Decade: Average Values",
    subtitle = "Exports and FDI Inflows as % of GDP",
    x = "Decade",
    y = "",
    caption = "Source: World Bank (Exports), UNCTAD (FDI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 9),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  here("figures", "economic_openness_heatmap.png"),
  plot_heatmap,
  width = 12,
  height = 6,
  dpi = 300
)

print(plot_heatmap)

# ==============================================================================
# Summary Statistics Table
# ==============================================================================

cat("\nGenerating summary statistics...\n")

summary_stats <- combined_data %>%
  group_by(country) %>%
  summarise(
    # Exports
    exports_mean = round(mean(exports_pct_gdp, na.rm = TRUE), 2),
    exports_sd = round(sd(exports_pct_gdp, na.rm = TRUE), 2),
    exports_min = round(min(exports_pct_gdp, na.rm = TRUE), 2),
    exports_max = round(max(exports_pct_gdp, na.rm = TRUE), 2),
    
    # FDI Inward
    fdi_inward_mean = round(mean(fdi_inward_pct_gdp, na.rm = TRUE), 2),
    fdi_inward_sd = round(sd(fdi_inward_pct_gdp, na.rm = TRUE), 2),
    fdi_inward_min = round(min(fdi_inward_pct_gdp, na.rm = TRUE), 2),
    fdi_inward_max = round(max(fdi_inward_pct_gdp, na.rm = TRUE), 2),
    
    .groups = "drop"
  )

cat("\n", rep("=", 80), "\n", sep = "")
cat("SUMMARY STATISTICS: ECONOMIC OPENNESS (% of GDP)\n")
cat(rep("=", 80), "\n", sep = "")
cat("\nExports of Goods and Services:\n")
print(summary_stats %>% select(country, exports_mean, exports_sd, exports_min, exports_max))

cat("\nFDI Inflows:\n")
print(summary_stats %>% select(country, fdi_inward_mean, fdi_inward_sd, fdi_inward_min, fdi_inward_max))

# Save summary table
summary_file <- here("output", "summary_statistics.csv")
write_csv(summary_stats, summary_file)
cat("\n✓ Summary statistics saved to:", summary_file, "\n")

# ==============================================================================
# Completion Message
# ==============================================================================

cat("\n", rep("=", 60), "\n", sep = "")
cat("COMBINED ANALYSIS COMPLETE!\n")
cat(rep("=", 60), "\n", sep = "")

cat("\n✓ Visualizations created:\n")
cat("  1. economic_openness_dual_faceted.png\n")
cat("  2. exports_fdi_scatter.png\n")
cat("  3. economic_openness_combined_lines.png\n")
cat("  4. philippines_openness_highlighted.png\n")
cat("  5. economic_openness_heatmap.png\n")

cat("\n✓ Data files created:\n")
cat("  - data/economic_openness_combined.csv\n")
cat("  - output/summary_statistics.csv\n")

cat("\n", rep("=", 60), "\n\n", sep = "")