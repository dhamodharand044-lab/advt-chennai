# ==============================================================================
# AtmosVue: Comparative Air Quality Analytics and Visualization for Chennai
# Module: R/dashboard_modules.R
# Purpose: UI components, KPI metric cards, advisory banners, and page views
# ==============================================================================

#' Render a professional KPI card
#'
#' @param title Metric title
#' @param value Metric value
#' @param subtitle Small explanatory text
#' @param badge_label Optional badge text
#' @param badge_color Badge background color
#' @return Shiny HTML div
kpi_card <- function(title, value, subtitle = NULL, badge_label = NULL, badge_color = "#0D9488") {
  shiny::tags$div(
    class = "atmos-kpi-card",
    shiny::tags$div(
      class = "atmos-kpi-header",
      shiny::tags$span(class = "atmos-kpi-title", title),
      if (!is.null(badge_label)) {
        shiny::tags$span(
          class = "atmos-kpi-badge",
          style = paste0("background-color: ", badge_color, ";"),
          badge_label
        )
      }
    ),
    shiny::tags$div(class = "atmos-kpi-value", as.character(value)),
    if (!is.null(subtitle)) {
      shiny::tags$div(class = "atmos-kpi-subtitle", subtitle)
    }
  )
}

#' Render an advisory notice box
#'
#' @param title Notice title
#' @param text Explanatory details
#' @param type "info", "warning", or "danger"
#' @return Shiny HTML div
advisory_banner <- function(title, text, type = "info") {
  cls <- paste0("atmos-advisory atmos-advisory-", type)
  icon_char <- switch(type, "warning" = "⚠️ ", "danger" = "🛑 ", "ℹ️ ")

  shiny::tags$div(
    class = cls,
    shiny::tags$div(class = "atmos-advisory-title", paste0(icon_char, title)),
    shiny::tags$div(class = "atmos-advisory-text", text)
  )
}

# ==============================================================================
# Page UI Layouts
# ==============================================================================

#' Page 1: Overview Dashboard UI
page_overview_ui <- function() {
  shiny::tagList(
    shiny::uiOutput("ui_overview_advisory"),

    # Prominent Interactive Quick-Station Button Bar
    shiny::tags$div(
      style = "margin-bottom: 14px; background: #FFFFFF; border: 1px solid #CBD5E1; border-left: 5px solid #0D9488; border-radius: 8px; padding: 12px 16px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
      shiny::tags$div(
        style = "display: flex; flex-wrap: wrap; justify-content: space-between; align-items: center; gap: 10px;",
        shiny::tags$div(
          style = "display: flex; align-items: center; gap: 6px; flex-wrap: wrap;",
          shiny::tags$strong("⚡ Filter Station (1-Click): ", style = "font-size: 0.88rem; color: #0F2537; margin-right: 4px;"),
          shiny::uiOutput("ui_quick_station_buttons", inline = TRUE)
        ),
        shiny::tags$div(
          shiny::uiOutput("ui_active_filter_status")
        )
      )
    ),

    # KPI Metric Cards
    shiny::uiOutput("ui_overview_kpis"),
    shiny::tags$div(style = "height: 18px;"),

    # Charts Row 1
    bslib::layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            shiny::tags$strong("AQI Classification Distribution"),
            shiny::tags$span(style = "font-size: 0.74rem; color: #0D9488; font-weight: 600;", "🖱️ Click bar to filter category")
          )
        ),
        shiny::plotOutput("plot_overview_aqi_dist", height = "360px", click = "click_overview_aqi_dist"),
        bslib::card_footer("Classified according to official Central Pollution Control Board (CPCB) India AQI categories.")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            shiny::tags$strong("Station Average AQI Comparison"),
            shiny::tags$span(style = "font-size: 0.74rem; color: #0D9488; font-weight: 600;", "🖱️ Click bar to isolate station")
          )
        ),
        shiny::plotOutput("plot_overview_station_avg", height = "360px", click = "click_overview_station_avg"),
        bslib::card_footer("Zero-baseline comparative mean values across Chennai monitoring stations.")
      )
    ),
    shiny::tags$div(style = "height: 18px;"),

    # Charts Row 2
    bslib::layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::tags$strong("Monthly Average AQI Pattern")),
        shiny::plotOutput("plot_overview_monthly", height = "340px"),
        bslib::card_footer("Aggregated monthly mean readings for sampled months.")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            shiny::tags$strong("Monitoring Station Summary Table"),
            shiny::tags$span(style = "font-size: 0.74rem; color: #0D9488; font-weight: 600;", "🖱️ Click row to isolate station")
          )
        ),
        DT::DTOutput("table_overview_stations"),
        bslib::card_footer("Clicking any station row isolates that station across all dashboard pages.")
      )
    ),
    shiny::tags$div(style = "height: 18px;"),

    # Automated Analytical Synthesis
    bslib::card(
      bslib::card_header(shiny::tags$strong("Automated Analytical Synthesis")),
      shiny::tags$div(
        style = "padding: 10px 16px; font-size: 0.95rem; line-height: 1.6; color: #1E293B;",
        shiny::textOutput("text_overview_interpretation")
      )
    )
  )
}

#' Page 2: Station Comparison UI
page_station_ui <- function() {
  shiny::tagList(
    # In-page Real-time Controls
    bslib::card(
      style = "margin-bottom: 16px; background: #FFFFFF; border-left: 4px solid #0F2537;",
      shiny::tags$div(
        style = "display: flex; flex-wrap: wrap; align-items: center; justify-content: space-between; gap: 14px;",
        shiny::tags$div(
          style = "flex: 2; min-width: 280px;",
          shiny::selectInput(
            "page_station_select",
            "Select Station to Isolate (or Compare All):",
            choices = c("All Stations (Combined)" = "ALL", EXPECTED_STATIONS),
            selected = "ALL"
          )
        ),
        shiny::tags$div(
          style = "flex: 1; min-width: 200px;",
          shiny::radioButtons(
            "page_station_sort",
            "Sort Stations By:",
            choices = c("Mean AQI" = "Mean_AQI", "Median AQI" = "Median_AQI", "Max AQI" = "Max_AQI"),
            selected = "Mean_AQI",
            inline = TRUE
          )
        )
      )
    ),

    bslib::layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::tags$strong("Station-wise Mean AQI Ranking")),
        shiny::plotOutput("plot_station_comp_bar", height = "380px", click = "click_station_comp_bar")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          shiny::tags$div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            shiny::tags$strong("Station AQI Metrics & Data Completeness"),
            shiny::tags$span(style = "font-size: 0.72rem; color: #0D9488;", "Click row to select")
          )
        ),
        DT::DTOutput("table_station_detail")
      )
    ),
    shiny::tags$div(style = "height: 18px;"),
    bslib::card(
      bslib::card_header(shiny::tags$strong("Station-wise AQI Category Breakdown")),
      shiny::plotOutput("plot_station_cat_breakdown", height = "360px"),
      bslib::card_footer("Proportion of air quality categories observed at each station.")
    )
  )
}

#' Page 3: Pollutant Analysis UI
page_pollutant_ui <- function() {
  shiny::tagList(
    # In-page Real-time Pollutant Selector Bar
    bslib::card(
      style = "margin-bottom: 16px; background: #FFFFFF; border-left: 4px solid #0D9488;",
      shiny::tags$div(
        style = "display: flex; flex-wrap: wrap; align-items: center; justify-content: space-between; gap: 14px;",
        shiny::tags$div(
          shiny::tags$strong("Select Pollutant to Analyze: ", style = "font-size: 0.92rem; color: #0F2537; margin-right: 10px;"),
          shiny::radioButtons(
            "page_pollutant_choice",
            label = NULL,
            choices = c("PM2.5", "PM10", "NO2", "SO2", "CO"),
            selected = "PM2.5",
            inline = TRUE
          )
        ),
        shiny::tags$div(
          shiny::selectInput(
            "page_pollutant_station_select",
            label = "Filter Station for Pollutant View:",
            choices = c("All Stations" = "ALL", EXPECTED_STATIONS),
            selected = "ALL",
            width = "240px"
          )
        )
      )
    ),

    # Pollutant KPI Cards
    bslib::card(
      bslib::card_header(shiny::tags$strong("Pollutant Summary Metric Cards")),
      shiny::uiOutput("ui_pollutant_kpis")
    ),
    shiny::tags$div(style = "height: 18px;"),

    # Charts
    bslib::layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::tags$strong("Station Distribution (Boxplot)")),
        shiny::plotOutput("plot_pollutant_box", height = "360px"),
        bslib::card_footer("Boxplot illustrates median, IQR (25th-75th percentile), and outliers.")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::tags$strong("Concentration Frequency (Histogram)")),
        shiny::plotOutput("plot_pollutant_hist", height = "360px"),
        bslib::card_footer("Empirical distribution across all filtered readings with Mean & Median lines.")
      )
    ),
    shiny::tags$div(style = "height: 18px;"),
    bslib::layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::tags$strong("Pollutant & AQI Correlation Heatmap")),
        shiny::plotOutput("plot_pollutant_corr", height = "360px"),
        bslib::card_footer("Note: Pearson r assesses linear association, not direct causation.")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::tags$strong("Station-Level Pollutant Statistics")),
        DT::DTOutput("table_pollutant_station")
      )
    )
  )
}

#' Page 4: Monthly Trends UI
page_monthly_ui <- function() {
  shiny::tagList(
    shiny::uiOutput("ui_monthly_advisory"),

    # In-page Real-time Controls
    bslib::card(
      style = "margin-bottom: 14px;",
      shiny::tags$div(
        style = "display: flex; flex-wrap: wrap; align-items: center; gap: 14px;",
        shiny::tags$div(
          style = "flex: 1; min-width: 250px;",
          shiny::selectInput(
            "page_monthly_station_select",
            "Station to Display in Monthly Trajectory:",
            choices = c("All Stations (Comparison)" = "ALL", EXPECTED_STATIONS),
            selected = "ALL"
          )
        ),
        shiny::tags$div(
          style = "flex: 1; min-width: 280px;",
          shiny::radioButtons(
            "page_monthly_view_mode",
            "Timeline Display Mode:",
            choices = c("Multi-Year Timeline (2023–2026)" = "chrono", "Seasonal Cycle (Jan–Dec Average)" = "seasonal"),
            selected = "chrono",
            inline = TRUE
          )
        )
      )
    ),

    bslib::layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::tags$strong("Overall Monthly Average AQI")),
        shiny::plotOutput("plot_monthly_trend_main", height = "380px")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::tags$strong("Station-wise Monthly Trajectory")),
        shiny::plotOutput("plot_monthly_station_trend", height = "380px")
      )
    ),
    shiny::tags$div(style = "height: 18px;"),
    bslib::card(
      bslib::card_header(shiny::tags$strong("Monthly Observation Aggregations")),
      DT::DTOutput("table_monthly_summary")
    )
  )
}

#' Page 5: Data Quality UI
page_data_quality_ui <- function() {
  shiny::tagList(
    shiny::uiOutput("ui_dq_health_banner"),
    shiny::tags$div(style = "height: 18px;"),
    bslib::layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::tags$strong("Data Integrity & Validation Audit Rules")),
        DT::DTOutput("table_dq_audit")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::tags$strong("Column-wise Missing Value Analysis")),
        DT::DTOutput("table_dq_missing")
      )
    ),
    shiny::tags$div(style = "height: 18px;"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        shiny::tags$div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          shiny::tags$strong("Flagged Records Requiring Review"),
          shiny::downloadButton("download_flagged_csv", "Export Flagged Records (CSV)", class = "btn-sm btn-outline-primary")
        )
      ),
      DT::DTOutput("table_dq_flagged"),
      bslib::card_footer("All flagged anomalies are retained for transparent auditing; no record is silently discarded.")
    ),
    shiny::tags$div(style = "height: 18px;"),
    bslib::card(
      bslib::card_header(shiny::tags$strong("Data Pipeline Transformation Log")),
      DT::DTOutput("table_dq_cleaning_log")
    )
  )
}

#' Page 6: Data Explorer UI
page_explorer_ui <- function() {
  shiny::tagList(
    bslib::card(
      bslib::card_header(
        shiny::tags$div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          shiny::tags$span(shiny::tags$strong("Dataset Explorer: "), "Search, Filter & Inspect Individual Records"),
          shiny::tags$div(
            shiny::downloadButton("download_filtered_csv", "Download Filtered (CSV)", class = "btn-sm btn-outline-primary"),
            shiny::tags$span(style = "width: 8px; display: inline-block;"),
            shiny::downloadButton("download_clean_all_csv", "Download All Cleaned (CSV)", class = "btn-sm btn-primary")
          )
        )
      ),
      shiny::tags$div(
        style = "margin-bottom: 12px;",
        shiny::radioButtons(
          "explorer_data_view",
          "Select Data Layer to Inspect:",
          choices = c("Cleaned & Standardized Dataset" = "cleaned", "Raw Imported Records" = "raw"),
          selected = "cleaned",
          inline = TRUE
        )
      ),
      DT::DTOutput("table_data_explorer")
    )
  )
}

#' Page 7: About Project UI
page_about_ui <- function() {
  shiny::tagList(
    bslib::card(
      bslib::card_header(shiny::tags$h4("About AtmosVue: Comparative Air Quality Analytics for Chennai", style = "margin: 0; color: #0F2537;")),
      shiny::tags$div(
        style = "padding: 12px; font-size: 0.95rem; line-height: 1.7; color: #334155;",
        shiny::tags$p(
          shiny::tags$strong("AtmosVue"), " is an academic-quality, comparative air quality analytics dashboard engineered in R Shiny. It provides transparent data ingestion, strict validation auditing, exploratory analysis, and clear visual interpretations of atmospheric monitoring records from five prominent monitoring stations across Chennai, India."
        ),
        shiny::tags$hr(),
        shiny::tags$h5(shiny::tags$strong("Core Research Questions")),
        shiny::tags$ol(
          shiny::tags$li(shiny::tags$strong("Station Disparities: "), "How does average AQI and pollutant concentration vary among Chennai's distinct geographical/industrial hubs (Kodungaiyur, Koyambedu, Manali, Perungudi, and Royapuram)?"),
          shiny::tags$li(shiny::tags$strong("Seasonal / Monthly Variation: "), "How do AQI observations fluctuate across calendar months in the available sampled records?"),
          shiny::tags$li(shiny::tags$strong("Pollutant Drivers: "), "Which primary pollutants (PM2.5, PM10, NO2, SO2, CO) predominate across different urban zones?"),
          shiny::tags$li(shiny::tags$strong("Data Integrity & Uncertainty: "), "How do missing records, inconsistent date notations, and irregular categorical entries affect statistical reliability and policy conclusions?")
        ),
        shiny::tags$hr(),
        shiny::tags$h5(shiny::tags$strong("Methodology")),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$strong("Data Ingestion & Provenance: "), "Loads CSV files with provenance tracking. Supports multi-file ingestion and user file uploads without silent overwrites."),
          shiny::tags$li(shiny::tags$strong("Conservative Date Parsing: "), "Preserves original date strings. Extracts day, month, and year strictly where defensible. Unverified years are classified as month-only exploratory groupings rather than misleading continuous time series."),
          shiny::tags$li(shiny::tags$strong("No Zero-Imputation: "), "Missing pollutant values remain NA; missing readings are never converted to zero, avoiding artificial suppression of pollution averages."),
          shiny::tags$li(shiny::tags$strong("No Automated Outlier Truncation: "), "Unusual extreme readings are flagged for analyst inspection rather than deleted silently."),
          shiny::tags$li(shiny::tags$strong("Standardized Category Alignment: "), "AQI categories are matched against official CPCB India bands: Good (0-50), Satisfactory (51-100), Moderate (101-200), Poor (201-300), Very Poor (301-400), and Severe (401-500).")
        ),
        shiny::tags$hr(),
        shiny::tags$h5(shiny::tags$strong("Scientific Limitations")),
        shiny::tags$ul(
          shiny::tags$li("The dashboard reflects retrospective observational monitor data and does not provide real-time or live predictive environmental forecasts."),
          shiny::tags$li("Correlation analyses quantify linear statistical co-occurrence and must not be interpreted as establishing causality or atmospheric chemical formation mechanisms."),
          shiny::tags$li("Incomplete date timestamps in some source records prevent uninterrupted multi-year trend modeling.")
        ),
        shiny::tags$hr(),
        shiny::tags$h5(shiny::tags$strong("Software & Technology Architecture")),
        shiny::tags$p("Engineered completely in R using modern responsive frameworks:"),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$code("R Shiny"), " for reactive dashboard server architecture"),
          shiny::tags$li(shiny::tags$code("bslib"), " for modern theme layout and responsive typography"),
          shiny::tags$li(shiny::tags$code("ggplot2"), " for publication-grade data visualizations"),
          shiny::tags$li(shiny::tags$code("dplyr & tidyr"), " for tidy data transformation"),
          shiny::tags$li(shiny::tags$code("readr"), " for safe and performant CSV ingestion"),
          shiny::tags$li(shiny::tags$code("DT"), " for searchable, paginated, and interactive data tables")
        )
      )
    )
  )
}
