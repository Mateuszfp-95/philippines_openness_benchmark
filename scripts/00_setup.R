# ==============================================================================
# Setup Script for Philippines Economic Openness Project
# ==============================================================================

# Load required packages
library(WDI)        # World Bank data
library(tidyverse)  # Data manipulation and visualization
library(readxl)     # Read Excel files
library(writexl)    # Write Excel files
library(lubridate)  # Date handling
library(scales)     # Formatting
library(here)       # File paths

# Set project parameters
country_code <- "PHL"  # Philippines ISO code
country_name <- "Philippines"
start_year <- 1990
end_year <- 2023

# Create a theme for consistent plotting
theme_set(theme_minimal(base_size = 12))

# Print confirmation
cat("\n=====================================\n")
cat("Project: Philippines Economic Openness\n")
cat("Country:", country_name, "\n")
cat("Period:", start_year, "-", end_year, "\n")
cat("=====================================\n\n")
cat("✓ Environment setup complete!\n\n")

# Display project structure
cat("Project folders:\n")
list.files() %>% paste("-", .) %>% cat(sep = "\n")