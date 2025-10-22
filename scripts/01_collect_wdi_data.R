# ==============================================================================
# Script: Process Manually Downloaded WDI Exports Data (Multiple Countries)
# Countries: Philippines, Vietnam, Thailand
# ==============================================================================

# Load setup
source(here::here("scripts", "00_setup.R"))

# Define countries for comparison
countries <- c("Philippines", "Vietnam", "Thailand")
country_codes <- c("PHL", "VNM", "THA")

# Check for manual download file
manual_file <- here("data", "philippines_exports_wdi_manual.csv")

cat("Processing WDI Exports data for comparison...\n")
cat("Countries:", paste(countries, collapse = ", "), "\n")
cat(rep("-", 60), "\n", sep = "")

if (!file.exists(manual_file)) {
  cat("\n❌ File not found!\n\n")
  cat("Please download data manually from World Bank:\n")
  cat("1. Visit: https://data.worldbank.org/\n")
  cat("2. Search for: 'Exports of goods and services % GDP'\n")
  cat("3. Filter for: Philippines, Vietnam, AND Thailand\n")
  cat("4. Years: 1990-2023\n")
  cat("5. Download as CSV\n")
  cat("6. Save to 'data' folder as: philippines_exports_wdi_manual.csv\n")
  cat("\nIMPORTANT: Make sure to select ALL THREE countries!\n\n")
  stop("Manual data file not found")
}

cat("✓ File found! Processing...\n\n")

# Read the CSV file
exports_raw <- read_csv(manual_file, show_col_types = FALSE)

cat("File structure:\n")
cat("Rows:", nrow(exports_raw), "\n")
cat("Columns:", ncol(exports_raw), "\n\n")

# Check which countries are in the data
countries_in_data <- unique(exports_raw$`Country Code`)
cat("Countries found in data:\n")
print(countries_in_data)
cat("\n")

# Check if all target countries are present
missing_countries <- setdiff(country_codes, countries_in_data)
if (length(missing_countries) > 0) {
  cat("⚠️ WARNING: The following countries are missing from the data:\n")
  cat(paste(missing_countries, collapse = ", "), "\n\n")
  cat("Please re-download the data and make sure to select:\n")
  cat("- Philippines (PHL)\n")
  cat("- Vietnam (VNM)\n")
  cat("- Thailand (THA)\n\n")
  
  cat("Proceeding with available countries only...\n\n")
}

# Process and clean the data for all available countries
exports_clean <- exports_raw %>%
  # Filter for our target countries (only those present)
  filter(`Country Code` %in% country_codes) %>%
  # Select country info and year columns
  select(
    country = `Country Name`,
    iso2c = `Country Code`,
    starts_with("19"),
    starts_with("20")
  ) %>%
  # --- Normalize country names ---
  mutate(
    country = case_when(
      country == "Viet Nam" ~ "Vietnam",
      TRUE ~ country
    )
  ) %>%
  # CRITICAL: Convert ALL year columns to numeric
  mutate(across(matches("^(19|20)\\d{2}$"), ~as.numeric(as.character(.)))) %>%
  # Pivot from wide to long format
  pivot_longer(
    cols = -c(country, iso2c),
    names_to = "year",
    values_to = "exports_pct_gdp"
  ) %>%
  # Convert year to numeric
  mutate(year = as.numeric(year)) %>%
  # Filter for desired year range
  filter(year >= start_year & year <= end_year) %>%
  # Remove rows with missing values
  filter(!is.na(exports_pct_gdp)) %>%
  # Sort by country and year
  arrange(country, year)

# Display summary
cat("\n" , rep("=", 60), "\n", sep = "")
cat("SUMMARY OF EXPORTS DATA (ALL COUNTRIES)\n")
cat(rep("=", 60), "\n", sep = "")
cat("\nTotal observations:", nrow(exports_clean), "\n")
cat("Countries included:", length(unique(exports_clean$country)), "\n")
cat("Year range:", min(exports_clean$year, na.rm = TRUE), "-", 
    max(exports_clean$year, na.rm = TRUE), "\n\n")

# Summary by country
cat("Observations per country:\n")
print(exports_clean %>% 
        group_by(country) %>% 
        summarise(
          n_obs = n(),
          first_year = min(year),
          last_year = max(year),
          mean_exports = round(mean(exports_pct_gdp, na.rm = TRUE), 2),
          .groups = "drop"
        ))

cat("\nFirst 5 years (all countries):\n")
print(head(exports_clean, 15))

# Save cleaned data
output_file <- here("data", "exports_wdi_comparison.csv")
write_csv(exports_clean, output_file)
cat("\n✓ Cleaned data saved to:", output_file, "\n")

# ==============================================================================
# Create comparison visualizations
# ==============================================================================

cat("\nGenerating comparison plots...\n")

# Plot 1: All three countries comparison
plot_comparison <- ggplot(exports_clean, aes(x = year, y = exports_pct_gdp, 
                                             color = country, group = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "Philippines" = "#2E86AB",
      "Vietnam" = "#A23B72",
      "Thailand" = "#F18F01"
    ),
    name = "Country"
  ) +
  labs(
    title = "Exports of Goods and Services: Regional Comparison",
    subtitle = paste0(start_year, " - ", end_year),
    x = "Year",
    y = "Exports (% of GDP)",
    caption = "Source: World Bank WDI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "exports_comparison_all.png"),
  plot_comparison,
  width = 12,
  height = 7,
  dpi = 300
)

print(plot_comparison)

# Plot 2: Individual country plots (faceted)
plot_faceted <- ggplot(exports_clean, aes(x = year, y = exports_pct_gdp)) +
  geom_line(aes(color = country), linewidth = 1.2, show.legend = FALSE) +
  geom_point(aes(color = country), size = 2, show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "Philippines" = "#2E86AB",
      "Vietnam" = "#A23B72",
      "Thailand" = "#F18F01"
    )
  ) +
  facet_wrap(~country, ncol = 1, scales = "free_y") +
  labs(
    title = "Exports of Goods and Services by Country",
    subtitle = paste0(start_year, " - ", end_year),
    x = "Year",
    y = "Exports (% of GDP)",
    caption = "Source: World Bank WDI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "exports_comparison_faceted.png"),
  plot_faceted,
  width = 10,
  height = 9,
  dpi = 300
)

print(plot_faceted)

# Plot 3: Philippines highlighted with benchmarks
plot_philippines_focus <- ggplot(exports_clean, 
                                 aes(x = year, y = exports_pct_gdp, 
                                     color = country, group = country)) +
  geom_line(aes(size = country == "Philippines", alpha = country == "Philippines")) +
  geom_point(aes(size = country == "Philippines")) +
  scale_color_manual(
    values = c(
      "Philippines" = "#2E86AB",
      "Vietnam" = "#A23B72",
      "Thailand" = "#F18F01"
    ),
    name = "Country"
  ) +
  scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.8), guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.6), guide = "none") +
  labs(
    title = "Philippines Exports with Regional Benchmarks",
    subtitle = paste0(start_year, " - ", end_year),
    x = "Year",
    y = "Exports (% of GDP)",
    caption = "Source: World Bank WDI\nNote: Philippines shown in bold"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "exports_philippines_highlighted.png"),
  plot_philippines_focus,
  width = 12,
  height = 7,
  dpi = 300
)

print(plot_philippines_focus)

cat("\n✓ Plots saved:\n")
cat("  - figures/exports_comparison_all.png\n")
cat("  - figures/exports_comparison_faceted.png\n")
cat("  - figures/exports_philippines_highlighted.png\n")

cat("\n", rep("=", 60), "\n", sep = "")
cat("EXPORTS DATA PROCESSING COMPLETE!\n")
cat(rep("=", 60), "\n\n", sep = "")