# ==============================================================================
# AtmosVue: Comparative Air Quality Analytics and Visualization for Chennai
# Module: R/visualization_functions.R
# Purpose: High-quality, publication-grade ggplot2 visualizations with real-time highlighting
# ==============================================================================

# Palette definitions
ATMOS_NAVY <- "#0F2537"
ATMOS_TEAL <- "#0D9488"
ATMOS_SLATE <- "#475569"
ATMOS_LIGHT <- "#F8FAFC"
ATMOS_BORDER <- "#CBD5E1"
ATMOS_HIGHLIGHT <- "#F59E0B" # Warm amber highlight

# Standard CPCB AQI Category Colors
AQI_CATEGORY_COLORS <- c(
  "Good"         = "#16A34A",  # Emerald Green
  "Satisfactory" = "#65A30D",  # Olive / Lime Green
  "Moderate"     = "#D97706",  # Amber / Warm Ochre
  "Poor"         = "#EA580C",  # Orange
  "Very Poor"    = "#DC2626",  # Crimson Red
  "Severe"       = "#7F1D1D"   # Maroon / Deep Burgundy
)

# Station Colors
STATION_COLORS <- c(
  "Kodungaiyur" = "#0F4C81",
  "Koyambedu"   = "#0D9488",
  "Manali"      = "#D97706",
  "Perungudi"   = "#2563EB",
  "Royapuram"   = "#7C3AED"
)

#' Base AtmosVue ggplot theme
atmosvue_theme <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, color = ATMOS_NAVY, margin = ggplot2::margin(b = 6)),
      plot.subtitle = ggplot2::element_text(size = 11, color = ATMOS_SLATE, margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(size = 9, color = "#64748B", margin = ggplot2::margin(t = 8)),
      axis.title.x = ggplot2::element_text(face = "bold", size = 11, color = ATMOS_NAVY, margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(face = "bold", size = 11, color = ATMOS_NAVY, margin = ggplot2::margin(r = 8)),
      axis.text = ggplot2::element_text(color = ATMOS_SLATE, size = 10),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#F1F5F9", linewidth = 0.6),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.title = ggplot2::element_text(face = "bold", size = 10, color = ATMOS_NAVY),
      legend.text = ggplot2::element_text(size = 10, color = ATMOS_SLATE),
      legend.position = "right",
      strip.background = ggplot2::element_rect(fill = "#F1F5F9", color = NA),
      strip.text = ggplot2::element_text(face = "bold", size = 11, color = ATMOS_NAVY)
    )
}

#' Helper to generate an informative empty state plot
empty_state_plot <- function(message = "No matching records found for the current filter criteria.") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message,
                      color = "#94A3B8", size = 4.5, fontface = "italic") +
    ggplot2::theme_void() +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
}

#' Plot Average AQI by Station
#'
#' @param station_df Summary data frame from compute_station_summary()
#' @param highlight_station Optional character string of station to highlight
#' @return ggplot object
plot_station_avg_aqi <- function(station_df, highlight_station = NULL) {
  if (is.null(station_df) || nrow(station_df) == 0 || all(is.na(station_df$Mean_AQI))) {
    return(empty_state_plot("No valid station AQI observations available under active filters."))
  }

  df <- station_df[!is.na(station_df$Mean_AQI), ]
  # Reorder stations by Mean_AQI ascending for coord_flip
  df$Station <- factor(df$Station, levels = df$Station[order(df$Mean_AQI)])

  # Fill colors: highlight selected station if provided
  if (!is.null(highlight_station) && length(highlight_station) > 0 && any(df$Station %in% highlight_station)) {
    df$Fill_Color <- ifelse(df$Station %in% highlight_station, ATMOS_TEAL, "#94A3B8")
  } else {
    df$Fill_Color <- ATMOS_NAVY
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = Station, y = Mean_AQI, fill = I(Fill_Color))) +
    ggplot2::geom_col(width = 0.55, alpha = 0.9) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f", Mean_AQI)),
      hjust = -0.2, size = 3.8, fontface = "bold", color = ATMOS_NAVY
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.18)),
      limits = c(0, max(df$Mean_AQI, na.rm = TRUE) * 1.18)
    ) +
    atmosvue_theme() +
    ggplot2::labs(
      title = "Average Air Quality Index (AQI) by Monitoring Station",
      subtitle = if (!is.null(highlight_station) && length(highlight_station) > 0)
        paste0("Real-time view | Highlighting: ", paste(highlight_station, collapse = ", "))
      else "Zero-baseline magnitude comparison across Chennai monitoring locations (Click bar to filter)",
      x = "Monitoring Station",
      y = "Mean AQI (CPCB Scale)"
    )

  p
}

#' Plot AQI Category Distribution
#'
#' @param aqi_dist_df Summary data frame from compute_aqi_distribution()
#' @param highlight_category Optional category string to highlight
#' @return ggplot object
plot_aqi_category_distribution <- function(aqi_dist_df, highlight_category = NULL) {
  if (is.null(aqi_dist_df) || nrow(aqi_dist_df) == 0 || all(aqi_dist_df$Count == 0)) {
    return(empty_state_plot("No valid AQI categories available."))
  }

  all_cats <- names(AQI_CATEGORY_COLORS)
  aqi_dist_df$Category <- factor(aqi_dist_df$Category, levels = all_cats)

  # Alpha transparency based on highlight
  if (!is.null(highlight_category) && length(highlight_category) > 0 && any(aqi_dist_df$Category %in% highlight_category)) {
    aqi_dist_df$Alpha <- ifelse(aqi_dist_df$Category %in% highlight_category, 1.0, 0.35)
  } else {
    aqi_dist_df$Alpha <- 0.92
  }

  ggplot2::ggplot(aqi_dist_df, ggplot2::aes(x = Category, y = Count, fill = Category, alpha = I(Alpha))) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(Count, " (", Percentage, "%)")),
      vjust = -0.5, size = 3.6, fontface = "bold", color = ATMOS_NAVY
    ) +
    ggplot2::scale_fill_manual(values = AQI_CATEGORY_COLORS, drop = FALSE) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.18)),
      limits = c(0, max(aqi_dist_df$Count, na.rm = TRUE) * 1.25)
    ) +
    atmosvue_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15, hjust = 1)) +
    ggplot2::labs(
      title = "Distribution of AQI Classifications",
      subtitle = if (!is.null(highlight_category) && length(highlight_category) > 0)
        paste0("Real-time view | Highlighting: ", paste(highlight_category, collapse = ", "))
      else "Categorization based on standard CPCB air quality bands (Click bar to filter)",
      x = "AQI Classification Band",
      y = "Observation Count"
    )
}

#' Plot Monthly AQI Trends
#'
#' @param monthly_list Result from compute_monthly_aqi()
#' @param view_mode Character: "chrono" for continuous multi-year or "seasonal" for Jan-Dec aggregate
#' @return ggplot object
plot_monthly_aqi_trends <- function(monthly_list, view_mode = "chrono") {
  if (view_mode == "chrono" && !is.null(monthly_list$chronological_summary) && nrow(monthly_list$chronological_summary) > 0) {
    df <- monthly_list$chronological_summary
    df$Year_Month <- factor(df$Year_Month, levels = df$Year_Month)

    return(
      ggplot2::ggplot(df, ggplot2::aes(x = Year_Month, y = Mean_AQI, group = 1)) +
        ggplot2::geom_line(color = ATMOS_TEAL, linewidth = 1.3) +
        ggplot2::geom_point(color = ATMOS_NAVY, size = 3, shape = 21, fill = "white", stroke = 2) +
        ggplot2::geom_text(
          ggplot2::aes(label = sprintf("%.0f", Mean_AQI)),
          vjust = -1.2, size = 3.2, fontface = "bold", color = ATMOS_NAVY
        ) +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(mult = c(0.1, 0.22))
        ) +
        atmosvue_theme() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 8.5)
        ) +
        ggplot2::labs(
          title = "Continuous Multi-Year Monthly AQI Timeline (2023–2026)",
          subtitle = "Chronological monthly trajectory across all active Chennai observations",
          caption = "Verified calendar observations spanning January 2023 to October 2026.",
          x = "Year - Month",
          y = "Mean AQI"
        )
    )
  }

  df <- monthly_list$monthly_summary
  if (is.null(df) || nrow(df) == 0) {
    return(empty_state_plot("No monthly AQI data available."))
  }

  ggplot2::ggplot(df, ggplot2::aes(x = Month, y = Mean_AQI, group = 1)) +
    ggplot2::geom_line(color = ATMOS_TEAL, linewidth = 1.3) +
    ggplot2::geom_point(color = ATMOS_NAVY, size = 3.5, shape = 21, fill = "white", stroke = 2) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f", Mean_AQI)),
      vjust = -1.2, size = 3.6, fontface = "bold", color = ATMOS_NAVY
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0.1, 0.2))
    ) +
    atmosvue_theme() +
    ggplot2::labs(
      title = "Seasonal Average Air Quality Index (Jan–Dec Aggregate)",
      subtitle = "Aggregated mean AQI by calendar month across all recorded years",
      caption = "Aggregated monthly pattern reflecting typical Chennai seasonal variations.",
      x = "Month",
      y = "Mean AQI"
    )
}

#' Plot Station-wise Monthly Comparison
#'
#' @param monthly_list Result from compute_monthly_aqi()
#' @param selected_stations Optional vector of stations to display
#' @param view_mode Character: "chrono" for continuous multi-year or "seasonal" for Jan-Dec aggregate
#' @return ggplot object
plot_station_monthly_comparison <- function(monthly_list, selected_stations = NULL, view_mode = "chrono") {
  use_chrono <- (view_mode == "chrono" && !is.null(monthly_list$station_chronological) && nrow(monthly_list$station_chronological) > 0)
  df <- if (use_chrono) monthly_list$station_chronological else monthly_list$station_monthly

  if (is.null(df) || nrow(df) == 0) {
    return(empty_state_plot("No station-wise monthly data available."))
  }

  if (!is.null(selected_stations) && length(selected_stations) > 0) {
    df <- df[df$Station %in% selected_stations, ]
    if (nrow(df) == 0) {
      return(empty_state_plot("No monthly records for selected station(s)."))
    }
  }

  x_var <- if (use_chrono) "Year_Month" else "Month"
  if (use_chrono) {
    df$Year_Month <- factor(df$Year_Month, levels = sort(unique(df$Year_Month)))
  }
  df$Plot_X <- df[[x_var]]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = Plot_X, y = Mean_AQI, color = Station, group = Station)) +
    ggplot2::geom_line(linewidth = 1.2, alpha = 0.88) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::scale_color_manual(values = STATION_COLORS) +
    atmosvue_theme() +
    ggplot2::labs(
      title = if (use_chrono) "Station-wise Multi-Year AQI Trajectory (2023–2026)" else "Station-wise Monthly Average AQI (Seasonal)",
      subtitle = if (use_chrono) "Continuous chronological comparison across Chennai monitoring stations" else "Comparative month-by-month trajectory across Chennai stations",
      x = if (use_chrono) "Year - Month" else "Month",
      y = "Mean AQI",
      color = "Station"
    )

  if (use_chrono) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = 8.5))
  }
  p
}

#' Plot Station-wise AQI Category Proportional Breakdown
#'
#' @param df Cleaned data frame
#' @return ggplot object
plot_station_category_breakdown <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(empty_state_plot("No station data available."))
  }

  sub_df <- df[df$Station_Recognized & df$AQI.Index_Valid, ]
  if (nrow(sub_df) == 0) {
    return(empty_state_plot("No categorized station observations available."))
  }

  sub_df$AQI.Index <- factor(sub_df$AQI.Index, levels = names(AQI_CATEGORY_COLORS))

  ggplot2::ggplot(sub_df, ggplot2::aes(x = Station, fill = AQI.Index)) +
    ggplot2::geom_bar(position = "fill", alpha = 0.9) +
    ggplot2::scale_fill_manual(values = AQI_CATEGORY_COLORS, drop = FALSE, name = "AQI Category") +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(x * 100), "%"),
      expand = c(0, 0)
    ) +
    atmosvue_theme() +
    ggplot2::labs(
      title = "Proportional AQI Category Breakdown Across Stations",
      subtitle = "100% stacked bar comparison of air quality classifications",
      x = "Monitoring Station",
      y = "Proportion of Observations"
    )
}

#' Plot Pollutant Distribution (Boxplot across stations)
#'
#' @param df Cleaned data frame
#' @param pollutant_col Character name of pollutant (e.g. "PM2.5")
#' @param unit_label Unit string
#' @return ggplot object
plot_pollutant_boxplot <- function(df, pollutant_col = "PM2.5", unit_label = "µg/m³") {
  if (is.null(df) || nrow(df) == 0 || !(pollutant_col %in% names(df))) {
    return(empty_state_plot("No pollutant data available."))
  }

  sub_df <- df[df$Station_Recognized & !is.na(df[[pollutant_col]]), ]
  if (nrow(sub_df) == 0) {
    return(empty_state_plot(paste0("No valid observations for ", pollutant_col)))
  }

  # Map column safely to an explicit variable without relying on .data
  sub_df$Pol_Value <- as.numeric(sub_df[[pollutant_col]])

  ggplot2::ggplot(sub_df, ggplot2::aes(x = Station, y = Pol_Value, fill = Station)) +
    ggplot2::geom_boxplot(alpha = 0.78, outlier.shape = 21, outlier.fill = "#DC2626", outlier.size = 2, width = 0.5) +
    ggplot2::scale_fill_manual(values = STATION_COLORS) +
    atmosvue_theme() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      title = paste0("Station Distribution for ", pollutant_col),
      subtitle = paste0("Boxplots showing median, interquartile range (IQR), and extreme values (", unit_label, ")"),
      x = "Station",
      y = paste0(pollutant_col, " (", unit_label, ")")
    )
}

#' Plot Pollutant Distribution (Histogram)
#'
#' @param df Cleaned data frame
#' @param pollutant_col Character name of pollutant
#' @param unit_label Unit string
#' @return ggplot object
plot_pollutant_histogram <- function(df, pollutant_col = "PM2.5", unit_label = "µg/m³") {
  if (is.null(df) || nrow(df) == 0 || !(pollutant_col %in% names(df))) {
    return(empty_state_plot("No pollutant data available."))
  }

  vals <- na.omit(df[[pollutant_col]])
  if (length(vals) == 0) {
    return(empty_state_plot(paste0("No valid numeric measurements for ", pollutant_col)))
  }

  p_df <- data.frame(Value = as.numeric(vals))
  mean_val <- mean(p_df$Value, na.rm = TRUE)
  med_val <- median(p_df$Value, na.rm = TRUE)

  ggplot2::ggplot(p_df, ggplot2::aes(x = Value)) +
    ggplot2::geom_histogram(fill = ATMOS_TEAL, color = "white", bins = 25, alpha = 0.85) +
    ggplot2::geom_vline(xintercept = mean_val, color = "#DC2626", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_vline(xintercept = med_val, color = "#2563EB", linetype = "dotted", linewidth = 1) +
    atmosvue_theme() +
    ggplot2::labs(
      title = paste0("Overall Frequency Histogram: ", pollutant_col),
      subtitle = paste0("Red dashed = Mean (", round(mean_val, 1), "), Blue dotted = Median (", round(med_val, 1), ") (", unit_label, ")"),
      x = paste0(pollutant_col, " (", unit_label, ")"),
      y = "Frequency Count"
    )
}

#' Plot Correlation Heatmap
#'
#' @param cor_mat Correlation matrix from compute_pollutant_correlations()
#' @return ggplot object
plot_correlation_heatmap <- function(cor_mat) {
  if (is.null(cor_mat) || length(cor_mat) == 0 || all(is.na(cor_mat))) {
    return(empty_state_plot("Insufficient pairwise data to compute correlation matrix."))
  }

  cor_df <- as.data.frame(as.table(cor_mat))
  names(cor_df) <- c("Var1", "Var2", "Correlation")

  ggplot2::ggplot(cor_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Correlation)), color = ATMOS_NAVY, size = 4, fontface = "bold") +
    ggplot2::scale_fill_gradient2(
      low = "#2563EB", mid = "#F8FAFC", high = "#DC2626",
      midpoint = 0, limit = c(-1, 1), name = "Pearson (r)"
    ) +
    atmosvue_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
    ggplot2::labs(
      title = "Pairwise Pollutant & AQI Correlation Heatmap",
      subtitle = "Pearson correlation coefficients (Note: correlation indicates association, not causation)",
      x = "",
      y = ""
    )
}
