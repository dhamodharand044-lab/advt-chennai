# ==============================================================================
# AtmosVue: Comparative Air Quality Analytics and Visualization for Chennai
# Unit & Integration Tests: tests/test_data_processing.R
# ==============================================================================

cat("\n====================================================================\n")
cat(" AtmosVue Automated Test Suite: Data Processing & Analytics\n")
cat("====================================================================\n\n")

# Source pipeline modules
source("R/data_import.R")
source("R/data_cleaning.R")
source("R/data_validation.R")
source("R/data_analysis.R")
source("R/visualization_functions.R")

test_results <- list()
run_test <- function(name, expr) {
  res <- tryCatch({
    expr
    list(name = name, status = "PASS", error = NULL)
  }, error = function(e) {
    list(name = name, status = "FAIL", error = e$message)
  })
  test_results[[length(test_results) + 1]] <<- res
  cat(sprintf("[%s] %s\n", res$status, res$name))
  if (res$status == "FAIL") {
    cat(sprintf("       Error: %s\n", res$error))
  }
}

# ------------------------------------------------------------------------------
# Test Group 1: Data Import & Provenance
# ------------------------------------------------------------------------------
run_test("Import demo CSV and verify provenance column", {
  demo_file <- "data/synthetic_demonstration_data.csv"
  stopifnot(file.exists(demo_file))
  res <- import_single_csv(demo_file, "synthetic_demonstration_data.csv")
  stopifnot(res$status == "success" || res$status == "warning")
  stopifnot(nrow(res$data) > 0)
  stopifnot("source_file" %in% names(res$data))
  stopifnot(res$data$source_file[1] == "synthetic_demonstration_data.csv")
})

run_test("Import non-existent CSV returns safe error status without crashing", {
  res <- import_single_csv("non_existent_file.csv")
  stopifnot(res$status == "error")
  stopifnot(is.null(res$data))
})

# ------------------------------------------------------------------------------
# Test Group 2: Conservative Date Parsing
# ------------------------------------------------------------------------------
run_test("Date parser handles standard ISO and DMY dates with full 4-digit years", {
  dates <- c("2023-05-01", "01-05-2023", "25-10-2023", "20-10-2023")
  res <- parse_dates_conservatively(dates)
  stopifnot(all(res$Date_Valid_Full))
  stopifnot(all(res$Year == 2023))
  stopifnot(res$Month[1] == "May" && res$Month[2] == "May")
  stopifnot(res$Month[3] == "October" && res$Month[4] == "October")
  stopifnot(res$Day[1] == 1 && res$Day[2] == 1)
  stopifnot(res$Day[3] == 25 && res$Day[4] == 20)
  # Verify that year 1925 is NEVER generated!
  stopifnot(!any(res$Year == 1925))
})

run_test("Date parser handles text month with year like '16JULY 2023'", {
  dates <- c("16JULY 2023", "16 JULY 2023")
  res <- parse_dates_conservatively(dates)
  stopifnot(all(res$Date_Valid_Full))
  stopifnot(all(res$Year == 2023))
  stopifnot(all(res$Month == "July"))
  stopifnot(all(res$Day == 16))
})

run_test("Date parser flags month-day without year e.g. '02MAY' without inventing a year", {
  dates <- c("02MAY")
  res <- parse_dates_conservatively(dates)
  stopifnot(!res$Date_Valid_Full[1])
  stopifnot(is.na(res$Year[1]))
  stopifnot(res$Month[1] == "May")
  stopifnot(res$Day[1] == 2)
  stopifnot(res$Date_Quality_Flag[1] == "Month & Day Only (Missing Year)")
})

run_test("Date parser handles missing and invalid date strings safely", {
  dates <- c("", "NA", "invalid_string", NA)
  res <- parse_dates_conservatively(dates)
  stopifnot(all(!res$Date_Valid_Full))
  stopifnot(all(is.na(res$Date_Parsed)))
})

# ------------------------------------------------------------------------------
# Test Group 3: Cleaning & Validation
# ------------------------------------------------------------------------------
run_test("Cleaning preserves raw values, normalizes stations and categories", {
  test_raw <- data.frame(
    Date = c("01-05-2023", "02MAY", "invalid"),
    Station = c("kodungaiyur", "MANALI", "UnknownZone"),
    SO2 = c("12.5", "invalid_num", "15.0"),
    NO2 = c("25.0", "30.0", "35.0"),
    CO = c("0.8", "0.9", "1.0"),
    `PM2.5` = c("45.0", "55.0", "65.0"),
    PM10 = c("90.0", "110.0", "130.0"),
    `AQI.Index` = c("moderate", "VERY POOR", "random_cat"),
    `AQI.Value` = c("115", "310", "420"),
    `Prominent.Pollutant` = c("PM10", "PM2.5", "PM10"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  cleaned_res <- clean_air_quality_data(test_raw)
  df <- cleaned_res$cleaned_data

  stopifnot(df$Station[1] == "Kodungaiyur")
  stopifnot(df$Station[2] == "Manali")
  stopifnot(df$Station_Recognized[1] == TRUE)
  stopifnot(df$Station_Recognized[3] == FALSE)
  stopifnot(df$AQI.Index[1] == "Moderate")
  stopifnot(df$AQI.Index[2] == "Very Poor")
  stopifnot(is.na(df$AQI.Index[3]))
  stopifnot(!is.na(df$AQI.Index_Raw[3]))
  stopifnot(is.na(df$SO2[2]))
  stopifnot(nrow(cleaned_res$conversion_failures) == 1)
})

run_test("Validation checks detect missing values, unrecognized stations, and duplicate rows", {
  demo_file <- "data/synthetic_demonstration_data.csv"
  raw_df <- import_single_csv(demo_file)$data
  cleaned_df <- clean_air_quality_data(raw_df)$cleaned_data

  val <- validate_air_quality_data(cleaned_df, raw_df)
  stopifnot(!is.null(val$dq_summary))
  stopifnot(nrow(val$dq_summary) >= 6)
  stopifnot(!is.null(val$missing_counts))
  stopifnot(!is.null(val$flagged_records))
  stopifnot(val$overall_health_score >= 0 && val$overall_health_score <= 100)
})

# ------------------------------------------------------------------------------
# Test Group 4: Analytics Computations & Empty States
# ------------------------------------------------------------------------------
run_test("Overview KPIs compute correctly and handle empty data without crashing", {
  demo_file <- "data/synthetic_demonstration_data.csv"
  cleaned_df <- clean_air_quality_data(import_single_csv(demo_file)$data)$cleaned_data

  kpis <- compute_overview_kpis(cleaned_df)
  stopifnot(kpis$total_obs > 0)
  stopifnot(kpis$valid_aqi_obs > 0)
  stopifnot(kpis$station_count == 5)
  stopifnot(!is.na(kpis$overall_avg_aqi))

  empty_kpis <- compute_overview_kpis(data.frame())
  stopifnot(empty_kpis$total_obs == 0)
  stopifnot(empty_kpis$valid_aqi_obs == 0)
  stopifnot(is.na(empty_kpis$overall_avg_aqi))
})

run_test("Station summary ranks stations by average AQI", {
  demo_file <- "data/synthetic_demonstration_data.csv"
  cleaned_df <- clean_air_quality_data(import_single_csv(demo_file)$data)$cleaned_data

  st_sum <- compute_station_summary(cleaned_df)
  stopifnot(nrow(st_sum) == 5)
  stopifnot(all(c("Station", "Mean_AQI", "Rank") %in% names(st_sum)))
  stopifnot(st_sum$Mean_AQI[1] >= st_sum$Mean_AQI[nrow(st_sum)])
})

run_test("Pollutant calculations and correlation matrix compute safely", {
  demo_file <- "data/synthetic_demonstration_data.csv"
  cleaned_df <- clean_air_quality_data(import_single_csv(demo_file)$data)$cleaned_data

  pm25_stats <- compute_pollutant_stats(cleaned_df, "PM2.5")
  stopifnot(!is.na(pm25_stats$overall$mean))
  stopifnot(pm25_stats$overall$valid > 0)
  stopifnot(nrow(pm25_stats$station_table) == 5)

  cmat <- compute_pollutant_correlations(cleaned_df)
  stopifnot(!is.null(cmat))
  stopifnot(nrow(cmat) >= 2)
})

# ------------------------------------------------------------------------------
# Test Group 5: Visualizations & Interactive Highlighting
# ------------------------------------------------------------------------------
run_test("Visualizations render without external package crashes", {
  demo_file <- "data/synthetic_demonstration_data.csv"
  cleaned_df <- clean_air_quality_data(import_single_csv(demo_file)$data)$cleaned_data
  st_sum <- compute_station_summary(cleaned_df)
  aqi_dist <- compute_aqi_distribution(cleaned_df)
  monthly_aqi <- compute_monthly_aqi(cleaned_df)

  p1 <- plot_station_avg_aqi(st_sum, highlight_station = "Manali")
  stopifnot(inherits(p1, "ggplot"))

  p2 <- plot_aqi_category_distribution(aqi_dist, highlight_category = "Moderate")
  stopifnot(inherits(p2, "ggplot"))

  p3 <- plot_pollutant_boxplot(cleaned_df, "PM2.5", "µg/m³")
  stopifnot(inherits(p3, "ggplot"))

  p4 <- plot_pollutant_histogram(cleaned_df, "PM2.5", "µg/m³")
  stopifnot(inherits(p4, "ggplot"))

  p5 <- plot_station_category_breakdown(cleaned_df)
  stopifnot(inherits(p5, "ggplot"))

  p6 <- plot_monthly_aqi_trends(monthly_aqi, view_mode = "chrono")
  stopifnot(inherits(p6, "ggplot"))

  p7 <- plot_station_monthly_comparison(monthly_aqi, view_mode = "chrono")
  stopifnot(inherits(p7, "ggplot"))
})

run_test("Multi-year 2023–2026 dates parse accurately across all 4 years", {
  raw_df <- import_single_csv("data/chennai_air_quality_realtime.csv")$data
  cleaned_df <- clean_air_quality_data(raw_df)$cleaned_data

  years_present <- sort(unique(na.omit(cleaned_df$Year)))
  stopifnot(identical(years_present, c(2023L, 2024L, 2025L, 2026L)))

  # Check latest date is October 2026
  max_date <- max(cleaned_df$Date_Parsed, na.rm = TRUE)
  stopifnot(format(max_date, "%Y-%m") == "2026-10")

  # Verify chronological aggregation covers all months
  m_res <- compute_monthly_aqi(cleaned_df)
  stopifnot(!is.null(m_res$chronological_summary))
  stopifnot(nrow(m_res$chronological_summary) >= 40)
  stopifnot(m_res$chronological_summary$Year_Month[1] == "2023-01")
  stopifnot(tail(m_res$chronological_summary$Year_Month, 1) == "2026-10")
})

# ------------------------------------------------------------------------------
# Test Summary
# ------------------------------------------------------------------------------
cat("\n====================================================================\n")
total_tests <- length(test_results)
passed_tests <- sum(sapply(test_results, function(r) r$status == "PASS"))
cat(sprintf(" Tests Completed: %d / %d Passed\n", passed_tests, total_tests))
if (passed_tests == total_tests) {
  cat(" ALL TESTS PASSED SUCCESSFULLY! Real-time pipeline is verified.\n")
} else {
  cat(" SOME TESTS FAILED. Review output above.\n")
}
cat("====================================================================\n\n")
