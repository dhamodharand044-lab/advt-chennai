# ==============================================================================
# AtmosVue: Comparative Air Quality Analytics and Visualization for Chennai
# Script: atmosvue_analysis.R
# Purpose: Standalone reproducible exploratory analysis and publication-figure generation
# ==============================================================================

# 1. Source modular pipeline functions
source("R/data_import.R")
source("R/data_cleaning.R")
source("R/data_validation.R")
source("R/data_analysis.R")
source("R/visualization_functions.R")

cat("\n=======================================================\n")
cat(" AtmosVue: Chennai Air Quality Analytical Pipeline\n")
cat("=======================================================\n\n")

# 2. Ingest Data
data_load <- load_default_data(".")
if (is.null(data_load$combined_data)) {
  stop("No input data found. Please ensure CSV data files exist in root or data/ directory.")
}

cat(">> Data Source:", if (data_load$is_synthetic) "Demonstration Sample" else "Default CSV Files", "\n")
cat(">> Total Raw Rows Imported:", nrow(data_load$combined_data), "\n")

# 3. Clean and Validate
cleaned_res <- clean_air_quality_data(data_load$combined_data)
cleaned_df <- cleaned_res$cleaned_data
validation_res <- validate_air_quality_data(cleaned_df, data_load$combined_data)

cat(">> Valid AQI Observations:", sum(cleaned_df$Is_Valid_AQI_Obs, na.rm = TRUE), "\n")
cat(">> Overall Dataset Health Rating:", validation_res$overall_health_score, "%\n\n")

# 4. Generate Core Analytical Summaries
kpis <- compute_overview_kpis(cleaned_df)
station_summary <- compute_station_summary(cleaned_df)
aqi_dist <- compute_aqi_distribution(cleaned_df)
monthly_aqi <- compute_monthly_aqi(cleaned_df)

cat("---------------- STATION SUMMARY ----------------\n")
print(station_summary)

cat("\n------------- AQI CATEGORY SUMMARY --------------\n")
print(aqi_dist)

cat("\n------------- ANALYTICAL INTERPRETATION ---------\n")
cat(generate_dynamic_interpretation(kpis, station_summary, aqi_dist), "\n\n")

# 5. Export Standard Visualizations
cat(">> Generating and saving standard AtmosVue publication charts...\n")

# Chart 1: Average AQI by Station
p_station <- plot_station_avg_aqi(station_summary)
ggplot2::ggsave(
  filename = "AtmosVue_Average_AQI_by_Station.png",
  plot = p_station,
  width = 8.5, height = 5.2, dpi = 300
)
cat("   [+] Saved AtmosVue_Average_AQI_by_Station.png\n")

# Also save alias AtmosVue_Average_AQI.png for backwards compatibility
ggplot2::ggsave(
  filename = "AtmosVue_Average_AQI.png",
  plot = p_station,
  width = 8.5, height = 5.2, dpi = 300
)
cat("   [+] Saved AtmosVue_Average_AQI.png\n")

# Chart 2: AQI Category Distribution
p_category <- plot_aqi_category_distribution(aqi_dist)
ggplot2::ggsave(
  filename = "AtmosVue_AQI_Category_Distribution.png",
  plot = p_category,
  width = 8.5, height = 5.2, dpi = 300
)
cat("   [+] Saved AtmosVue_AQI_Category_Distribution.png\n")

# Chart 3: Monthly Average AQI
p_monthly <- plot_monthly_aqi_trends(monthly_aqi)
ggplot2::ggsave(
  filename = "AtmosVue_Monthly_Average_AQI.png",
  plot = p_monthly,
  width = 8.5, height = 5.2, dpi = 300
)
cat("   [+] Saved AtmosVue_Monthly_Average_AQI.png\n")

cat("\nPipeline execution complete. Visualizations successfully exported to workspace.\n")
