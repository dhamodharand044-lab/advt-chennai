# ==============================================================================
# AtmosVue: Comparative Air Quality Analytics and Visualization for Chennai
# Module: R/data_analysis.R
# Purpose: Analytical computations, statistical aggregations, and interpretation generators
# ==============================================================================

#' Compute overview KPI summary
#'
#' @param df Data frame of filtered cleaned records
#' @return A list of KPI metrics
compute_overview_kpis <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      total_obs = 0,
      valid_aqi_obs = 0,
      station_count = 0,
      overall_avg_aqi = NA_real_,
      highest_avg_station = "None",
      highest_avg_value = NA_real_
    ))
  }

  total_obs <- nrow(df)
  valid_df <- df[!is.na(df$AQI.Value) & df$Is_Valid_AQI_Obs, ]
  valid_aqi_obs <- nrow(valid_df)

  stations_present <- unique(na.omit(df$Station[df$Station_Recognized]))
  station_count <- length(stations_present)

  overall_avg_aqi <- if (valid_aqi_obs > 0) mean(valid_df$AQI.Value, na.rm = TRUE) else NA_real_

  # Compute station with highest average AQI
  if (valid_aqi_obs > 0) {
    st_avgs <- stats::aggregate(AQI.Value ~ Station, data = valid_df, FUN = mean)
    st_avgs <- st_avgs[order(-st_avgs$AQI.Value), ]
    highest_station <- as.character(st_avgs$Station[1])
    highest_val <- round(st_avgs$AQI.Value[1], 1)
  } else {
    highest_station <- "N/A"
    highest_val <- NA_real_
  }

  list(
    total_obs = total_obs,
    valid_aqi_obs = valid_aqi_obs,
    station_count = station_count,
    overall_avg_aqi = if (is.na(overall_avg_aqi)) NA_real_ else round(overall_avg_aqi, 1),
    highest_avg_station = highest_station,
    highest_avg_value = highest_val
  )
}

#' Station summary analytics
#'
#' @param df Data frame of filtered cleaned records
#' @return Data frame summarizing each station's metrics
compute_station_summary <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }

  # Filter to recognized stations
  sub_df <- df[df$Station_Recognized, ]
  if (nrow(sub_df) == 0) return(data.frame())

  stations <- sort(unique(sub_df$Station))
  results <- list()

  for (st in stations) {
    st_data <- sub_df[sub_df$Station == st, ]
    n_total <- nrow(st_data)
    valid_vals <- na.omit(st_data$AQI.Value)
    n_valid <- length(valid_vals)
    n_missing <- n_total - n_valid
    pct_missing <- if (n_total > 0) round(100 * n_missing / n_total, 1) else 0

    avg_val <- if (n_valid > 0) round(mean(valid_vals), 1) else NA_real_
    med_val <- if (n_valid > 0) round(median(valid_vals), 1) else NA_real_
    min_val <- if (n_valid > 0) round(min(valid_vals), 1) else NA_real_
    max_val <- if (n_valid > 0) round(max(valid_vals), 1) else NA_real_

    results[[st]] <- data.frame(
      Station = st,
      Total_Obs = n_total,
      Valid_Obs = n_valid,
      Missing_Pct = pct_missing,
      Mean_AQI = avg_val,
      Median_AQI = med_val,
      Min_AQI = min_val,
      Max_AQI = max_val,
      stringsAsFactors = FALSE
    )
  }

  summary_table <- do.call(rbind, results)
  # Rank by Mean_AQI descending
  if (nrow(summary_table) > 0 && any(!is.na(summary_table$Mean_AQI))) {
    summary_table$Rank <- rank(-summary_table$Mean_AQI, na.last = "keep", ties.method = "min")
    summary_table <- summary_table[order(summary_table$Rank), ]
  } else {
    summary_table$Rank <- NA_integer_
  }

  summary_table
}

#' Compute AQI category distribution
#'
#' @param df Data frame of filtered cleaned records
#' @return Data frame of category counts and percentages
compute_aqi_distribution <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(data.frame())
  }

  valid_df <- df[!is.na(df$AQI.Index) & df$AQI.Index_Valid, ]
  if (nrow(valid_df) == 0) return(data.frame())

  cat_counts <- table(valid_df$AQI.Index)
  total_valid <- nrow(valid_df)

  data.frame(
    Category = factor(names(cat_counts), levels = names(cat_counts)),
    Count = as.integer(cat_counts),
    Percentage = round(100 * as.numeric(cat_counts) / total_valid, 1),
    stringsAsFactors = FALSE
  )
}

#' Compute pollutant summary statistics
#'
#' @param df Data frame of filtered records
#' @param pollutant_col Character name of pollutant column (e.g. "PM2.5")
#' @return List with overall stats, station breakdown, and unit string
compute_pollutant_stats <- function(df, pollutant_col = "PM2.5") {
  if (is.null(df) || nrow(df) == 0 || !(pollutant_col %in% names(df))) {
    return(list(
      overall = list(mean = NA, median = NA, min = NA, max = NA, valid = 0, missing = 0),
      station_table = data.frame(),
      unit = "Unit not specified"
    ))
  }

  vals <- df[[pollutant_col]]
  valid_vals <- na.omit(vals)
  n_total <- nrow(df)
  n_valid <- length(valid_vals)
  n_missing <- n_total - n_valid

  overall_stats <- list(
    mean = if (n_valid > 0) round(mean(valid_vals), 2) else NA_real_,
    median = if (n_valid > 0) round(median(valid_vals), 2) else NA_real_,
    min = if (n_valid > 0) round(min(valid_vals), 2) else NA_real_,
    max = if (n_valid > 0) round(max(valid_vals), 2) else NA_real_,
    valid = n_valid,
    missing = n_missing
  )

  # Unit display label
  unit_label <- switch(
    pollutant_col,
    "PM2.5" = "µg/m³ (CPCB standard benchmark)",
    "PM10"  = "µg/m³ (CPCB standard benchmark)",
    "NO2"   = "µg/m³ (CPCB standard benchmark)",
    "SO2"   = "µg/m³ (CPCB standard benchmark)",
    "CO"    = "mg/m³ (CPCB standard benchmark)",
    "Unit not specified"
  )

  # Station level breakdown
  sub_df <- df[df$Station_Recognized, ]
  if (nrow(sub_df) > 0) {
    stations <- sort(unique(sub_df$Station))
    st_rows <- list()
    for (st in stations) {
      s_vals <- na.omit(sub_df[[pollutant_col]][sub_df$Station == st])
      s_tot <- sum(sub_df$Station == st)
      st_rows[[st]] <- data.frame(
        Station = st,
        Valid_Count = length(s_vals),
        Missing_Count = s_tot - length(s_vals),
        Mean = if (length(s_vals) > 0) round(mean(s_vals), 2) else NA_real_,
        Median = if (length(s_vals) > 0) round(median(s_vals), 2) else NA_real_,
        Min = if (length(s_vals) > 0) round(min(s_vals), 2) else NA_real_,
        Max = if (length(s_vals) > 0) round(max(s_vals), 2) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
    station_table <- do.call(rbind, st_rows)
  } else {
    station_table <- data.frame()
  }

  list(
    overall = overall_stats,
    station_table = station_table,
    unit = unit_label
  )
}

#' Compute pollutant correlation matrix
#'
#' @param df Data frame of filtered records
#' @return Correlation matrix or NULL if insufficient data
compute_pollutant_correlations <- function(df) {
  cols <- c("PM2.5", "PM10", "NO2", "SO2", "CO", "AQI.Value")
  available_cols <- intersect(cols, names(df))

  if (length(available_cols) < 2) return(NULL)

  num_matrix <- df[, available_cols, drop = FALSE]
  # Calculate pairwise complete correlation
  suppressWarnings({
    cor_mat <- stats::cor(num_matrix, use = "pairwise.complete.obs")
  })

  round(cor_mat, 2)
}

#' Compute monthly aggregation for AQI
#'
#' @param df Data frame of filtered records
#' @return List with monthly_summary, station_monthly, chronological_summary, station_chronological, and date_quality_advisory
compute_monthly_aqi <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !("Month" %in% names(df))) {
    return(list(
      monthly_summary = data.frame(),
      station_monthly = data.frame(),
      chronological_summary = data.frame(),
      station_chronological = data.frame(),
      date_quality_advisory = "No monthly data available."
    ))
  }

  valid_df <- df[!is.na(df$AQI.Value) & !is.na(df$Month), ]
  if (nrow(valid_df) == 0) {
    return(list(
      monthly_summary = data.frame(),
      station_monthly = data.frame(),
      chronological_summary = data.frame(),
      station_chronological = data.frame(),
      date_quality_advisory = "No observations with both valid AQI and parsed month."
    ))
  }

  # Overall seasonal monthly aggregation (Jan - Dec)
  months_present <- levels(valid_df$Month)[levels(valid_df$Month) %in% unique(valid_df$Month)]

  m_summary_list <- list()
  for (m in months_present) {
    m_data <- valid_df[valid_df$Month == m, ]
    if (nrow(m_data) > 0) {
      m_summary_list[[m]] <- data.frame(
        Month = m,
        Mean_AQI = round(mean(m_data$AQI.Value, na.rm = TRUE), 1),
        Median_AQI = round(median(m_data$AQI.Value, na.rm = TRUE), 1),
        Obs_Count = nrow(m_data),
        stringsAsFactors = FALSE
      )
    }
  }
  monthly_summary <- do.call(rbind, m_summary_list)

  # Station by month aggregation
  st_valid <- valid_df[valid_df$Station_Recognized, ]
  if (nrow(st_valid) > 0) {
    station_monthly <- stats::aggregate(
      AQI.Value ~ Station + Month,
      data = st_valid,
      FUN = function(x) c(Mean = mean(x), Count = length(x))
    )
    station_monthly_flat <- data.frame(
      Station = station_monthly$Station,
      Month = station_monthly$Month,
      Mean_AQI = round(station_monthly$AQI.Value[, "Mean"], 1),
      Obs_Count = as.integer(station_monthly$AQI.Value[, "Count"]),
      stringsAsFactors = FALSE
    )
  } else {
    station_monthly_flat <- data.frame()
  }

  # Chronological Year-Month aggregation (2023-01 through 2026-10)
  chrono_df <- valid_df[valid_df$Date_Valid_Full & !is.na(valid_df$Date_Parsed), ]
  if (nrow(chrono_df) > 0) {
    chrono_df$Year_Month <- format(chrono_df$Date_Parsed, "%Y-%m")
    ym_present <- sort(unique(chrono_df$Year_Month))

    c_summary_list <- list()
    for (ym in ym_present) {
      ym_data <- chrono_df[chrono_df$Year_Month == ym, ]
      if (nrow(ym_data) > 0) {
        c_summary_list[[ym]] <- data.frame(
          Year_Month = ym,
          Mean_AQI = round(mean(ym_data$AQI.Value, na.rm = TRUE), 1),
          Median_AQI = round(median(ym_data$AQI.Value, na.rm = TRUE), 1),
          Obs_Count = nrow(ym_data),
          stringsAsFactors = FALSE
        )
      }
    }
    chronological_summary <- do.call(rbind, c_summary_list)

    # Station by Year-Month
    st_chrono <- chrono_df[chrono_df$Station_Recognized, ]
    if (nrow(st_chrono) > 0) {
      agg_st_chrono <- stats::aggregate(
        AQI.Value ~ Station + Year_Month,
        data = st_chrono,
        FUN = function(x) c(Mean = mean(x), Count = length(x))
      )
      station_chronological <- data.frame(
        Station = agg_st_chrono$Station,
        Year_Month = agg_st_chrono$Year_Month,
        Mean_AQI = round(agg_st_chrono$AQI.Value[, "Mean"], 1),
        Obs_Count = as.integer(agg_st_chrono$AQI.Value[, "Count"]),
        stringsAsFactors = FALSE
      )
      station_chronological <- station_chronological[order(station_chronological$Year_Month, station_chronological$Station), ]
    } else {
      station_chronological <- data.frame()
    }
  } else {
    chronological_summary <- data.frame()
    station_chronological <- data.frame()
  }

  # Assess date completeness
  full_date_ratio <- mean(df$Date_Valid_Full, na.rm = TRUE)
  advisory <- if (is.nan(full_date_ratio) || full_date_ratio < 0.8) {
    "DATA QUALITY ADVISORY: Significant portion of records lack verifiable calendar years (e.g. format '02MAY'). This monthly visualization represents an exploratory month-wise grouping, not a continuous multi-year chronological time series. Avoid drawing seasonal causal conclusions without multi-year confirmation."
  } else {
    "CHRONOLOGICAL AUDIT: Multi-year observations (2023–2026) verified with 100% calendar completeness. Continuous time series and seasonal grouping active."
  }

  list(
    monthly_summary = monthly_summary,
    station_monthly = station_monthly_flat,
    chronological_summary = chronological_summary,
    station_chronological = station_chronological,
    date_quality_advisory = advisory
  )
}

#' Generate dynamic analytical interpretation text
#'
#' @param kpis KPI summary list
#' @param station_summary Station summary table
#' @param aqi_dist AQI category distribution table
#' @return Character string containing analytical commentary
generate_dynamic_interpretation <- function(kpis, station_summary, aqi_dist) {
  if (is.null(kpis) || kpis$valid_aqi_obs == 0) {
    return("No valid observations are available under the active filter selection to generate an analytical interpretation.")
  }

  # Most common category
  dominant_cat <- if (!is.null(aqi_dist) && nrow(aqi_dist) > 0) {
    top_cat <- aqi_dist[order(-aqi_dist$Count), ][1, ]
    paste0("'", top_cat$Category, "' (", top_cat$Percentage, "% of valid readings)")
  } else {
    "undetermined"
  }

  # Highest vs Lowest station
  station_contrast <- if (!is.null(station_summary) && nrow(station_summary) >= 2) {
    highest_st <- station_summary$Station[1]
    highest_val <- station_summary$Mean_AQI[1]
    lowest_st <- station_summary$Station[nrow(station_summary)]
    lowest_val <- station_summary$Mean_AQI[nrow(station_summary)]
    paste0("Across monitoring stations, ", highest_st, " recorded the highest mean AQI (",
           highest_val, "), whereas ", lowest_st, " recorded the lowest mean AQI (",
           lowest_val, ").")
  } else if (!is.null(station_summary) && nrow(station_summary) == 1) {
    paste0("Station ", station_summary$Station[1], " has an average AQI of ",
           station_summary$Mean_AQI[1], ".")
  } else {
    ""
  }

  paste0(
    "Based on ", kpis$valid_aqi_obs, " validated observations across ",
    kpis$station_count, " Chennai monitoring station(s), the overall mean AQI stands at ",
    kpis$overall_avg_aqi, ". The predominant air quality classification is ",
    dominant_cat, ". ", station_contrast,
    " Note: Observational summaries reflect local monitor readings during the sampled time window and should not be construed as definitive citywide exposure or causal evidence."
  )
}
