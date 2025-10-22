# ==============================================================================
# Script: Process FDI Data from UNCTAD (Multiple Countries Combined)
# Countries: Philippines, Vietnam, Thailand
# ==============================================================================

# Load setup
source(here::here("scripts", "00_setup.R"))

# Define file paths for both FDI files (now with multiple countries)
fdi_inward_file <- here("data", "FDI_philippines_inward.csv")
fdi_outward_file <- here("data", "FDI_philippines_outward.csv")

cat("Processing UNCTAD FDI data for regional comparison...\n")
cat("Countries: Philippines, Vietnam, Thailand\n")
cat(rep("-", 60), "\n", sep = "")

# Check if both files exist
if (!file.exists(fdi_inward_file)) {
  stop("❌ ERROR: File not found: ", basename(fdi_inward_file))
}

if (!file.exists(fdi_outward_file)) {
  stop("❌ ERROR: File not found: ", basename(fdi_outward_file))
}

cat("✓ Both files found!\n")
cat("  - ", basename(fdi_inward_file), "\n")
cat("  - ", basename(fdi_outward_file), "\n\n")

# ==============================================================================
# Process FDI INWARD (entrada de investimento)
# ==============================================================================

cat("Processing FDI Inward data...\n")

fdi_inward_raw <- read_csv(fdi_inward_file, show_col_types = FALSE)

cat("Countries in inward file:\n")
print(unique(fdi_inward_raw$Economy_Label))
cat("\n")

fdi_inward_clean <- fdi_inward_raw %>%
  # Select economy and all year columns
  select(economy = Economy_Label, ends_with("_Value")) %>%
  # Filter for our target countries
  filter(economy %in% c("Philippines", "Viet Nam", "Thailand")) %>%
  # Standardize country names
  mutate(economy = case_when(
    economy == "Viet Nam" ~ "Vietnam",
    TRUE ~ economy
  )) %>%
  # Pivot to long format
  pivot_longer(
    cols = -economy,
    names_to = "year_full",
    values_to = "fdi_inward"
  ) %>%
  # Extract year from column name
  mutate(year = as.numeric(str_extract(year_full, "^\\d{4}"))) %>%
  # Remove the full year column name
  select(country = economy, year, fdi_inward) %>%
  # Filter for desired year range
  filter(year >= start_year & year <= end_year) %>%
  # Remove NAs
  filter(!is.na(fdi_inward)) %>%
  # Sort
  arrange(country, year)

cat("✓ FDI Inward processed\n")
cat("Countries found:", paste(unique(fdi_inward_clean$country), collapse = ", "), "\n")
cat("Total observations:", nrow(fdi_inward_clean), "\n\n")

# ==============================================================================
# Process FDI OUTWARD (saída de investimento)
# ==============================================================================

cat("Processing FDI Outward data...\n")

fdi_outward_raw <- read_csv(fdi_outward_file, show_col_types = FALSE)

cat("Countries in outward file:\n")
print(unique(fdi_outward_raw$Economy_Label))
cat("\n")

fdi_outward_clean <- fdi_outward_raw %>%
  # Select economy and all year columns
  select(economy = Economy_Label, ends_with("_Value")) %>%
  # Filter for our target countries
  filter(economy %in% c("Philippines", "Viet Nam", "Thailand")) %>%
  # Standardize country names
  mutate(economy = case_when(
    economy == "Viet Nam" ~ "Vietnam",
    TRUE ~ economy
  )) %>%
  # Pivot to long format
  pivot_longer(
    cols = -economy,
    names_to = "year_full",
    values_to = "fdi_outward"
  ) %>%
  # Extract year from column name
  mutate(year = as.numeric(str_extract(year_full, "^\\d{4}"))) %>%
  # Remove the full year column name
  select(country = economy, year, fdi_outward) %>%
  # Filter for desired year range
  filter(year >= start_year & year <= end_year) %>%
  # Remove NAs
  filter(!is.na(fdi_outward)) %>%
  # Sort
  arrange(country, year)

cat("✓ FDI Outward processed\n")
cat("Countries found:", paste(unique(fdi_outward_clean$country), collapse = ", "), "\n")
cat("Total observations:", nrow(fdi_outward_clean), "\n\n")

# ==============================================================================
# Combine both datasets
# ==============================================================================

cat("Combining FDI Inward and Outward data...\n")

fdi_all_countries <- fdi_inward_clean %>%
  full_join(fdi_outward_clean, by = c("country", "year")) %>%
  # Calculate net FDI (inward - outward)
  mutate(
    fdi_outward = replace_na(fdi_outward, 0),
    fdi_net = fdi_inward - fdi_outward
  ) %>%
  arrange(country, year)

# Display summary
cat("\n" , rep("=", 60), "\n", sep = "")
cat("SUMMARY OF FDI DATA (ALL COUNTRIES)\n")
cat(rep("=", 60), "\n", sep = "")
cat("\nTotal observations:", nrow(fdi_all_countries), "\n")
cat("Countries included:", length(unique(fdi_all_countries$country)), "\n")
cat("Year range:", min(fdi_all_countries$year, na.rm = TRUE), "-", 
    max(fdi_all_countries$year, na.rm = TRUE), "\n\n")

# Summary by country
cat("Summary by country:\n")
summary_table <- fdi_all_countries %>% 
  group_by(country) %>% 
  summarise(
    n_obs = n(),
    first_year = min(year),
    last_year = max(year),
    mean_inward = round(mean(fdi_inward, na.rm = TRUE), 2),
    mean_outward = round(mean(fdi_outward, na.rm = TRUE), 2),
    mean_net = round(mean(fdi_net, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(summary_table)

cat("\nFirst 5 years (each country):\n")
print(fdi_all_countries %>% group_by(country) %>% slice_head(n = 5))

# Save combined data
output_file <- here("data", "fdi_unctad_comparison.csv")
write_csv(fdi_all_countries, output_file)
cat("\n✓ Combined FDI data saved to:", output_file, "\n")

# ==============================================================================
# Load GDP data and calculate FDI as % of GDP
# ==============================================================================

cat("\nLoading GDP data to calculate FDI as % of GDP...\n")

# Try to load GDP data (check both manual and API downloaded versions)
gdp_file_manual <- here("data", "gdp_current_usd.csv")

if (!file.exists(gdp_file_manual)) {
  cat("⚠️  WARNING: GDP data not found at:", gdp_file_manual, "\n")
  cat("FDI will be shown in absolute values only.\n")
  cat("\nTo add % of GDP calculations:\n")
  cat("1. Download GDP data from World Bank\n")
  cat("2. Save as: data/gdp_current_usd.csv\n\n")
  
  # Continue without GDP data
  fdi_with_gdp <- fdi_all_countries %>%
    mutate(
      fdi_inward_pct_gdp = NA_real_,
      fdi_outward_pct_gdp = NA_real_,
      fdi_net_pct_gdp = NA_real_
    )
  
} else {
  
  cat("✓ GDP data found! Loading...\n")
  
  # Load GDP data
  gdp_data_raw <- read_csv(gdp_file_manual, show_col_types = FALSE)
  
  # Check if it's in wide format (years as columns) or long format
  if ("year" %in% names(gdp_data_raw)) {
    # Already in long format
    gdp_data <- gdp_data_raw %>%
      select(country = matches("Country.*Name"), 
             year, 
             gdp_current_usd = matches("GDP|gdp")) %>%
      mutate(
        country = case_when(
          str_detect(country, "Viet") ~ "Vietnam",
          TRUE ~ country
        ),
        gdp_current_usd = as.numeric(gdp_current_usd)
      )
  } else {
    # Wide format - need to pivot
    gdp_data <- gdp_data_raw %>%
      filter(`Country Name` %in% c("Philippines", "Vietnam", "Viet Nam", "Thailand")) %>%
      mutate(`Country Name` = case_when(
        `Country Name` == "Viet Nam" ~ "Vietnam",
        TRUE ~ `Country Name`
      )) %>%
      select(country = `Country Name`, 
             starts_with("19"), 
             starts_with("20")) %>%
      mutate(across(matches("^(19|20)\\d{2}$"), ~as.numeric(as.character(.)))) %>%
      pivot_longer(
        cols = -country,
        names_to = "year",
        values_to = "gdp_current_usd"
      ) %>%
      mutate(year = as.numeric(year)) %>%
      filter(!is.na(gdp_current_usd))
  }
  
  cat("✓ GDP data processed\n")
  cat("Countries:", paste(unique(gdp_data$country), collapse = ", "), "\n\n")
  
  # Merge FDI with GDP data
  fdi_with_gdp <- fdi_all_countries %>%
    left_join(gdp_data, by = c("country", "year")) %>%
    mutate(
      # Convert FDI from millions to same unit as GDP
      # FDI is in million USD, GDP is in current USD
      # So multiply FDI by 1,000,000 to get USD
      fdi_inward_usd = fdi_inward * 1000000,
      fdi_outward_usd = fdi_outward * 1000000,
      fdi_net_usd = fdi_net * 1000000,
      
      # Calculate as % of GDP
      fdi_inward_pct_gdp = (fdi_inward_usd / gdp_current_usd) * 100,
      fdi_outward_pct_gdp = (fdi_outward_usd / gdp_current_usd) * 100,
      fdi_net_pct_gdp = (fdi_net_usd / gdp_current_usd) * 100
    ) %>%
    select(-fdi_inward_usd, -fdi_outward_usd, -fdi_net_usd)  # Remove intermediate columns
  
  cat("✓ FDI as % of GDP calculated\n\n")
  
  # Show summary
  cat("Summary of FDI as % of GDP:\n")
  summary_pct <- fdi_with_gdp %>%
    filter(!is.na(fdi_inward_pct_gdp)) %>%
    group_by(country) %>%
    summarise(
      mean_inward_pct = round(mean(fdi_inward_pct_gdp, na.rm = TRUE), 2),
      mean_outward_pct = round(mean(fdi_outward_pct_gdp, na.rm = TRUE), 2),
      mean_net_pct = round(mean(fdi_net_pct_gdp, na.rm = TRUE), 2),
      .groups = "drop"
    )
  print(summary_pct)
  cat("\n")
  
  # Save updated data with % GDP
  output_file_pct <- here("data", "fdi_unctad_comparison_pct_gdp.csv")
  write_csv(fdi_with_gdp, output_file_pct)
  cat("✓ FDI data with % GDP saved to:", output_file_pct, "\n\n")
}

# Replace the original dataset with the one including GDP calculations
fdi_all_countries <- fdi_with_gdp

# ==============================================================================
# Create comparison visualizations
# ==============================================================================

cat("\nGenerating comparison plots...\n")

# Define consistent colors (same as exports)
country_colors <- c(
  "Philippines" = "#2E86AB",
  "Vietnam" = "#A23B72",
  "Thailand" = "#F18F01"
)

# Plot 1: FDI Inward comparison
plot_inward_comparison <- ggplot(fdi_all_countries, 
                                 aes(x = year, y = fdi_inward, 
                                     color = country, group = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = country_colors, name = "Country") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "FDI Inflows: Regional Comparison",
    subtitle = paste0(start_year, " - ", end_year),
    x = "Year",
    y = "FDI Inflows (million USD, current prices)",
    caption = "Source: UNCTAD"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "fdi_inward_comparison.png"),
  plot_inward_comparison,
  width = 12,
  height = 7,
  dpi = 300
)

print(plot_inward_comparison)

# Plot 2: FDI Outward comparison
plot_outward_comparison <- ggplot(fdi_all_countries, 
                                  aes(x = year, y = fdi_outward, 
                                      color = country, group = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = country_colors, name = "Country") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "FDI Outflows: Regional Comparison",
    subtitle = paste0(start_year, " - ", end_year),
    x = "Year",
    y = "FDI Outflows (million USD, current prices)",
    caption = "Source: UNCTAD"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "fdi_outward_comparison.png"),
  plot_outward_comparison,
  width = 12,
  height = 7,
  dpi = 300
)

print(plot_outward_comparison)

# Plot 3: Net FDI comparison
plot_net_comparison <- ggplot(fdi_all_countries, 
                              aes(x = year, y = fdi_net, 
                                  color = country, group = country)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = country_colors, name = "Country") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Net FDI: Regional Comparison",
    subtitle = paste0(start_year, " - ", end_year, " (Inward - Outward)"),
    x = "Year",
    y = "Net FDI (million USD, current prices)",
    caption = "Source: UNCTAD"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "fdi_net_comparison.png"),
  plot_net_comparison,
  width = 12,
  height = 7,
  dpi = 300
)

print(plot_net_comparison)

# Plot 4: Faceted view by country
plot_faceted <- fdi_all_countries %>%
  select(country, year, fdi_inward, fdi_outward) %>%
  pivot_longer(cols = c(fdi_inward, fdi_outward), 
               names_to = "flow_type", 
               values_to = "value") %>%
  mutate(flow_type = recode(flow_type, 
                            "fdi_inward" = "Inward",
                            "fdi_outward" = "Outward")) %>%
  ggplot(aes(x = year, y = value, color = flow_type, group = flow_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~country, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Inward" = "#2E86AB", "Outward" = "#A23B72"),
                     name = "FDI Flow") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "FDI Flows by Country (Inward vs Outward)",
    subtitle = paste0(start_year, " - ", end_year),
    x = "Year",
    y = "FDI (million USD, current prices)",
    caption = "Source: UNCTAD"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "fdi_faceted_comparison.png"),
  plot_faceted,
  width = 10,
  height = 10,
  dpi = 300
)

print(plot_faceted)

# Plot 5: Philippines highlighted with benchmarks
plot_philippines_focus <- ggplot(fdi_all_countries, 
                                 aes(x = year, y = fdi_inward, 
                                     color = country, group = country)) +
  geom_line(aes(size = country == "Philippines", alpha = country == "Philippines")) +
  geom_point(aes(size = country == "Philippines")) +
  scale_color_manual(values = country_colors, name = "Country") +
  scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.8), guide = "none") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.6), guide = "none") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Philippines FDI Inflows with Regional Benchmarks",
    subtitle = paste0(start_year, " - ", end_year),
    x = "Year",
    y = "FDI Inflows (million USD, current prices)",
    caption = "Source: UNCTAD\nNote: Philippines shown in bold"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  here("figures", "fdi_philippines_highlighted.png"),
  plot_philippines_focus,
  width = 12,
  height = 7,
  dpi = 300
)

print(plot_philippines_focus)

# ==============================================================================
# Additional Plots: FDI as % of GDP
# ==============================================================================

if (any(!is.na(fdi_all_countries$fdi_inward_pct_gdp))) {
  
  cat("\nGenerating FDI as % of GDP plots...\n")
  
  # Plot 6: FDI Inward as % of GDP
  plot_inward_pct_gdp <- ggplot(
    fdi_all_countries %>% filter(!is.na(fdi_inward_pct_gdp)), 
    aes(x = year, y = fdi_inward_pct_gdp, color = country, group = country)
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = country_colors, name = "Country") +
    labs(
      title = "FDI Inflows as % of GDP: Regional Comparison",
      subtitle = paste0(start_year, " - ", end_year),
      x = "Year",
      y = "FDI Inflows (% of GDP)",
      caption = "Source: UNCTAD (FDI), World Bank (GDP)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    here("figures", "fdi_inward_pct_gdp_comparison.png"),
    plot_inward_pct_gdp,
    width = 12,
    height = 7,
    dpi = 300
  )
  
  print(plot_inward_pct_gdp)
  
  # Plot 7: FDI Outward as % of GDP
  plot_outward_pct_gdp <- ggplot(
    fdi_all_countries %>% filter(!is.na(fdi_outward_pct_gdp)), 
    aes(x = year, y = fdi_outward_pct_gdp, color = country, group = country)
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = country_colors, name = "Country") +
    labs(
      title = "FDI Outflows as % of GDP: Regional Comparison",
      subtitle = paste0(start_year, " - ", end_year),
      x = "Year",
      y = "FDI Outflows (% of GDP)",
      caption = "Source: UNCTAD (FDI), World Bank (GDP)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    here("figures", "fdi_outward_pct_gdp_comparison.png"),
    plot_outward_pct_gdp,
    width = 12,
    height = 7,
    dpi = 300
  )
  
  print(plot_outward_pct_gdp)
  
  # Plot 8: Net FDI as % of GDP
  plot_net_pct_gdp <- ggplot(
    fdi_all_countries %>% filter(!is.na(fdi_net_pct_gdp)), 
    aes(x = year, y = fdi_net_pct_gdp, color = country, group = country)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = country_colors, name = "Country") +
    labs(
      title = "Net FDI as % of GDP: Regional Comparison",
      subtitle = paste0(start_year, " - ", end_year, " (Inward - Outward)"),
      x = "Year",
      y = "Net FDI (% of GDP)",
      caption = "Source: UNCTAD (FDI), World Bank (GDP)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    here("figures", "fdi_net_pct_gdp_comparison.png"),
    plot_net_pct_gdp,
    width = 12,
    height = 7,
    dpi = 300
  )
  
  print(plot_net_pct_gdp)
  
  cat("\n✓ Additional plots saved:\n")
  cat("  - figures/fdi_inward_pct_gdp_comparison.png\n")
  cat("  - figures/fdi_outward_pct_gdp_comparison.png\n")
  cat("  - figures/fdi_net_pct_gdp_comparison.png\n")
}

cat("\n✓ Plots saved:\n")
cat("  - figures/fdi_inward_comparison.png\n")
cat("  - figures/fdi_outward_comparison.png\n")
cat("  - figures/fdi_net_comparison.png\n")
cat("  - figures/fdi_faceted_comparison.png\n")
cat("  - figures/fdi_philippines_highlighted.png\n")

cat("\n", rep("=", 60), "\n", sep = "")
cat("FDI DATA PROCESSING COMPLETE!\n")
cat(rep("=", 60), "\n\n", sep = "")