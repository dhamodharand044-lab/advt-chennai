# ==============================================================================
# AtmosVue: Comparative Air Quality Analytics and Visualization for Chennai
# Module: R/data_validation.R
# Purpose: Comprehensive data quality auditing, validation checks, and diagnostic tables
# ==============================================================================

#' Validate cleaned air quality dataset
#'
#' @param cleaned_df Data frame produced by clean_air_quality_data()
#' @param raw_df Optional original raw data frame
#' @return A list containing:
#'   - dq_summary: Summary data frame of all checks with status and counts
#'   - missing_counts: Column-wise missing value counts and percentages
#'   - flagged_records: Detailed records flagged for analyst review
#'   - overall_health_score: Percentage of records passing core quality criteria
validate_air_quality_data <- function(cleaned_df, raw_df = NULL) {
  if (is.null(cleaned_df) || nrow(cleaned_df) == 0) {
    return(list(
      dq_summary = data.frame(
        Check = "Record Count",
        Status = "FAIL",
        Details = "No records available for validation",
        Flagged_Count = 0,
        stringsAsFactors = FALSE
      ),
      missing_counts = data.frame(),
      flagged_records = data.frame(),
      overall_health_score = 0
    ))
  }

  n_total <- nrow(cleaned_df)
  checks <- list()
  flagged_rows_list <- list()

  # Check 1: Total records & Valid AQI records
  valid_aqi_count <- sum(cleaned_df$Is_Valid_AQI_Obs, na.rm = TRUE)
  checks[[length(checks) + 1]] <- data.frame(
    Check = "Valid AQI Observations",
    Status = if (valid_aqi_count > 0) "PASS" else "FAIL",
    Details = paste0(valid_aqi_count, " of ", n_total, " rows have valid AQI value & station."),
    Flagged_Count = n_total - valid_aqi_count,
    stringsAsFactors = FALSE
  )

  # Check 2: Missing AQI Values
  missing_aqi <- sum(is.na(cleaned_df$AQI.Value))
  checks[[length(checks) + 1]] <- data.frame(
    Check = "Missing AQI Values",
    Status = if (missing_aqi == 0) "PASS" else if (missing_aqi < 0.2 * n_total) "WARN" else "FAIL",
    Details = paste0(missing_aqi, " rows missing AQI.Value (", round(100 * missing_aqi / n_total, 1), "%)."),
    Flagged_Count = missing_aqi,
    stringsAsFactors = FALSE
  )
  if (missing_aqi > 0) {
    bad_idx <- which(is.na(cleaned_df$AQI.Value))
    flagged_rows_list[[length(flagged_rows_list) + 1]] <- data.frame(
      Row_ID = bad_idx,
      Station = cleaned_df$Station[bad_idx],
      Date = cleaned_df$Date_Raw[bad_idx],
      Issue_Type = "Missing AQI Value",
      Severity = "High",
      Value = "NA",
      stringsAsFactors = FALSE
    )
  }

  # Check 3: Duplicate rows
  dup_count <- sum(cleaned_df$Is_Duplicate, na.rm = TRUE)
  checks[[length(checks) + 1]] <- data.frame(
    Check = "Duplicate Rows",
    Status = if (dup_count == 0) "PASS" else "WARN",
    Details = paste0(dup_count, " duplicate rows detected."),
    Flagged_Count = dup_count,
    stringsAsFactors = FALSE
  )
  if (dup_count > 0) {
    bad_idx <- which(cleaned_df$Is_Duplicate)
    flagged_rows_list[[length(flagged_rows_list) + 1]] <- data.frame(
      Row_ID = bad_idx,
      Station = cleaned_df$Station[bad_idx],
      Date = cleaned_df$Date_Raw[bad_idx],
      Issue_Type = "Duplicate Observation",
      Severity = "Medium",
      Value = "Exact key duplicate",
      stringsAsFactors = FALSE
    )
  }

  # Check 4: Unrecognized Station Names
  unrec_stations <- sum(!cleaned_df$Station_Recognized, na.rm = TRUE)
  checks[[length(checks) + 1]] <- data.frame(
    Check = "Station Identification",
    Status = if (unrec_stations == 0) "PASS" else "WARN",
    Details = paste0(unrec_stations, " rows have missing or unrecognized station names."),
    Flagged_Count = unrec_stations,
    stringsAsFactors = FALSE
  )
  if (unrec_stations > 0) {
    bad_idx <- which(!cleaned_df$Station_Recognized)
    flagged_rows_list[[length(flagged_rows_list) + 1]] <- data.frame(
      Row_ID = bad_idx,
      Station = as.character(cleaned_df$Station_Raw[bad_idx]),
      Date = cleaned_df$Date_Raw[bad_idx],
      Issue_Type = "Unrecognized Station",
      Severity = "High",
      Value = as.character(cleaned_df$Station_Raw[bad_idx]),
      stringsAsFactors = FALSE
    )
  }

  # Check 5: Date Quality & Completeness
  date_failures <- sum(!cleaned_df$Date_Valid_Full, na.rm = TRUE)
  checks[[length(checks) + 1]] <- data.frame(
    Check = "Date Completeness (Year/Month/Day)",
    Status = if (date_failures == 0) "PASS" else "WARN",
    Details = paste0(date_failures, " rows lack full calendar dates (e.g. missing year or malformed)."),
    Flagged_Count = date_failures,
    stringsAsFactors = FALSE
  )
  if (date_failures > 0) {
    bad_idx <- which(!cleaned_df$Date_Valid_Full)
    flagged_rows_list[[length(flagged_rows_list) + 1]] <- data.frame(
      Row_ID = bad_idx,
      Station = cleaned_df$Station[bad_idx],
      Date = cleaned_df$Date_Raw[bad_idx],
      Issue_Type = cleaned_df$Date_Quality_Flag[bad_idx],
      Severity = "Medium",
      Value = as.character(cleaned_df$Date_Raw[bad_idx]),
      stringsAsFactors = FALSE
    )
  }

  # Check 6: AQI Category Validity
  invalid_cat_count <- sum(!cleaned_df$AQI.Index_Valid & !is.na(cleaned_df$AQI.Index_Raw), na.rm = TRUE)
  checks[[length(checks) + 1]] <- data.frame(
    Check = "AQI Category Labels",
    Status = if (invalid_cat_count == 0) "PASS" else "WARN",
    Details = paste0(invalid_cat_count, " rows have non-standard AQI category labels."),
    Flagged_Count = invalid_cat_count,
    stringsAsFactors = FALSE
  )
  if (invalid_cat_count > 0) {
    bad_idx <- which(!cleaned_df$AQI.Index_Valid & !is.na(cleaned_df$AQI.Index_Raw))
    flagged_rows_list[[length(flagged_rows_list) + 1]] <- data.frame(
      Row_ID = bad_idx,
      Station = cleaned_df$Station[bad_idx],
      Date = cleaned_df$Date_Raw[bad_idx],
      Issue_Type = "Non-standard AQI Category",
      Severity = "Low",
      Value = as.character(cleaned_df$AQI.Index_Raw[bad_idx]),
      stringsAsFactors = FALSE
    )
  }

  # Check 7: Numeric Range Checks (Suspicious Values)
  # Negative values check
  num_cols <- intersect(c("SO2", "NO2", "CO", "PM2.5", "PM10", "AQI.Value"), names(cleaned_df))
  neg_count <- 0
  for (nc in num_cols) {
    negs <- which(cleaned_df[[nc]] < 0)
    if (length(negs) > 0) {
      neg_count <- neg_count + length(negs)
      flagged_rows_list[[length(flagged_rows_list) + 1]] <- data.frame(
        Row_ID = negs,
        Station = cleaned_df$Station[negs],
        Date = cleaned_df$Date_Raw[negs],
        Issue_Type = paste0("Negative Value in ", nc),
        Severity = "High",
        Value = as.character(cleaned_df[[nc]][negs]),
        stringsAsFactors = FALSE
      )
    }
  }
  checks[[length(checks) + 1]] <- data.frame(
    Check = "Negative Numeric Values",
    Status = if (neg_count == 0) "PASS" else "FAIL",
    Details = paste0(neg_count, " instances of negative pollutant or AQI values detected."),
    Flagged_Count = neg_count,
    stringsAsFactors = FALSE
  )

  # Check 8: Extreme Outliers / Unusual Values (AQI > 500, PM2.5 > 500, PM10 > 800)
  extreme_idx <- which(cleaned_df$AQI.Value > 500 | cleaned_df$PM2.5 > 500 | cleaned_df$PM10 > 800)
  checks[[length(checks) + 1]] <- data.frame(
    Check = "Extreme Numeric Outliers",
    Status = if (length(extreme_idx) == 0) "PASS" else "WARN",
    Details = paste0(length(extreme_idx), " rows with extreme values (e.g. AQI > 500 or PM2.5 > 500). Flagged for review."),
    Flagged_Count = length(extreme_idx),
    stringsAsFactors = FALSE
  )
  if (length(extreme_idx) > 0) {
    flagged_rows_list[[length(flagged_rows_list) + 1]] <- data.frame(
      Row_ID = extreme_idx,
      Station = cleaned_df$Station[extreme_idx],
      Date = cleaned_df$Date_Raw[extreme_idx],
      Issue_Type = "Unusual High Value (Flagged for Review)",
      Severity = "Low",
      Value = paste0("AQI: ", cleaned_df$AQI.Value[extreme_idx], ", PM2.5: ", cleaned_df$PM2.5[extreme_idx]),
      stringsAsFactors = FALSE
    )
  }

  # Missing values breakdown by column
  inspect_cols <- c("Date", "Station", "SO2", "NO2", "CO", "PM2.5", "PM10", "AQI.Index", "AQI.Value", "Prominent.Pollutant")
  existing_inspect_cols <- intersect(inspect_cols, names(cleaned_df))

  missing_df <- data.frame(
    Column = existing_inspect_cols,
    Missing_Count = sapply(existing_inspect_cols, function(col) sum(is.na(cleaned_df[[col]]))),
    Total_Count = n_total,
    stringsAsFactors = FALSE
  )
  missing_df$Missing_Percent <- round(100 * missing_df$Missing_Count / missing_df$Total_Count, 1)

  flagged_records_df <- if (length(flagged_rows_list) > 0) {
    dplyr::bind_rows(flagged_rows_list)
  } else {
    data.frame(
      Row_ID = integer(0),
      Station = character(0),
      Date = character(0),
      Issue_Type = character(0),
      Severity = character(0),
      Value = character(0),
      stringsAsFactors = FALSE
    )
  }

  summary_df <- do.call(rbind, checks)

  # Compute overall health score: % of records with valid AQI, recognized station, non-negative
  healthy_recs <- sum(cleaned_df$Is_Valid_AQI_Obs & !cleaned_df$Is_Duplicate, na.rm = TRUE)
  health_score <- round(100 * healthy_recs / n_total, 1)

  list(
    dq_summary = summary_df,
    missing_counts = missing_df,
    flagged_records = flagged_records_df,
    overall_health_score = health_score
  )
}
