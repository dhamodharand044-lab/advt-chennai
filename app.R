# ==============================================================================
# AtmosVue: Comparative Air Quality Analytics and Visualization for Chennai
# Main Shiny Application (app.R) - Real-Time Interactive Engine
#
# Launch:
#   shiny::runApp()
#   or click "Run App" in RStudio
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Package Verification & Loading
# ------------------------------------------------------------------------------
required_packages <- c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "readr", "DT")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(paste0(
    "The following required packages are not installed: ",
    paste(missing_packages, collapse = ", "), "\n",
    "Please install them using:\n",
    "install.packages(c(", paste(sprintf("'%s'", missing_packages), collapse = ", "), "))"
  ))
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(DT)
})

# ------------------------------------------------------------------------------
# 2. Source Modular Components
# ------------------------------------------------------------------------------
source("R/data_import.R", local = TRUE)
source("R/data_cleaning.R", local = TRUE)
source("R/data_validation.R", local = TRUE)
source("R/data_analysis.R", local = TRUE)
source("R/visualization_functions.R", local = TRUE)
source("R/dashboard_modules.R", local = TRUE)

# ------------------------------------------------------------------------------
# 3. Application Theme & Aesthetics
# ------------------------------------------------------------------------------
atmos_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "litera",
  primary = "#0F2537",
  secondary = "#0D9488",
  success = "#16A34A",
  warning = "#D97706",
  danger = "#DC2626",
  info = "#0284C7",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter")
)

# ------------------------------------------------------------------------------
# 4. User Interface (UI)
# ------------------------------------------------------------------------------
ui <- bslib::page_navbar(
  id = "nav_tabs",
  title = shiny::tags$div(
    style = "display: flex; align-items: center; gap: 8px;",
    shiny::tags$span(style = "font-weight: 800; font-size: 1.25rem; letter-spacing: -0.02em; color: #FFFFFF;", "AtmosVue"),
    shiny::tags$span(style = "font-size: 0.75rem; background: #0D9488; color: #FFFFFF; padding: 2px 8px; border-radius: 9999px; font-weight: 600;", "Chennai AQI Real-Time Analytics")
  ),
  theme = atmos_theme,
  header = shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  sidebar = bslib::sidebar(
    width = 320,
    open = "desktop",
    shiny::tags$div(
      class = "sidebar-title",
      "Control & Filter Center"
    ),
    shiny::tags$div(
      class = "sidebar-tagline",
      "Explore Air. Understand Impact."
    ),
    shiny::hr(style = "margin-top: 0; margin-bottom: 12px;"),

    # Data Source & Ingestion Section
    shiny::tags$strong("Dataset Source", style = "font-size: 0.82rem; color: #0F2537; text-transform: uppercase; letter-spacing: 0.03em;"),
    shiny::uiOutput("ui_source_badge"),
    shiny::fileInput(
      "upload_csv",
      "Upload CSV File(s)",
      multiple = TRUE,
      accept = c(".csv", "text/csv", "text/comma-separated-values"),
      buttonLabel = "Browse...",
      placeholder = "Select air quality CSV(s)"
    ),
    shiny::actionButton(
      "btn_restore_sample",
      "Restore Demonstration Dataset",
      class = "btn-sm btn-outline-secondary",
      style = "width: 100%; margin-bottom: 14px;"
    ),

    shiny::hr(style = "margin: 8px 0 14px 0;"),

    # Interactive Reactive Filters
    shiny::tags$strong("Interactive Real-Time Filters", style = "font-size: 0.82rem; color: #0F2537; text-transform: uppercase; letter-spacing: 0.03em;"),
    shiny::tags$div(style = "height: 8px;"),

    # 1-Click Station Selection Dropdown
    shiny::selectInput(
      "filter_station",
      "Monitoring Station:",
      choices = c("All Stations (Combined)" = "ALL", EXPECTED_STATIONS),
      selected = "ALL"
    ),

    # Optional Multi-Station Checkbox Mode
    shiny::checkboxInput(
      "enable_multi_station",
      "Compare multiple specific stations",
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = "input.enable_multi_station == true",
      shiny::checkboxGroupInput(
        "filter_stations_multi",
        "Select Stations to Compare:",
        choices = EXPECTED_STATIONS,
        selected = EXPECTED_STATIONS
      )
    ),

    # 1-Click AQI Category Filter
    shiny::selectInput(
      "filter_category",
      "AQI Classification Band:",
      choices = c("All Category Bands" = "ALL", STANDARD_AQI_CATEGORIES),
      selected = "ALL"
    ),

    # Focus Pollutant Selector
    shiny::selectInput(
      "filter_pollutant",
      "Primary Focus Pollutant:",
      choices = c("PM2.5", "PM10", "NO2", "SO2", "CO"),
      selected = "PM2.5"
    ),

    # Calendar Month Filter
    shiny::selectInput(
      "filter_month",
      "Calendar Month:",
      choices = c("All Available Months" = "ALL"),
      selected = "ALL"
    ),

    shiny::uiOutput("ui_date_range_picker"),

    shiny::tags$div(style = "height: 8px;"),
    shiny::actionButton(
      "btn_reset_filters",
      "Reset All Filters (Show All Data)",
      icon = shiny::icon("rotate-left"),
      class = "btn-sm btn-outline-danger",
      style = "width: 100%; font-weight: 600;"
    ),

    shiny::tags$div(style = "height: 14px;"),
    shiny::uiOutput("ui_sidebar_filter_summary")
  ),

  # Navigation Pages
  bslib::nav_panel("Overview", page_overview_ui()),
  bslib::nav_panel("Station Comparison", page_station_ui()),
  bslib::nav_panel("Pollutant Analysis", page_pollutant_ui()),
  bslib::nav_panel("Monthly Trends", page_monthly_ui()),
  bslib::nav_panel("Data Quality", page_data_quality_ui()),
  bslib::nav_panel("Data Explorer", page_explorer_ui()),
  bslib::nav_panel("About Project", page_about_ui())
)

# ------------------------------------------------------------------------------
# 5. Server Logic - Reactive Real-Time Engine
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive storage for active dataset and UI click state
  app_state <- shiny::reactiveValues(
    raw_df = NULL,
    cleaned_df = NULL,
    cleaning_log = NULL,
    conversion_failures = NULL,
    is_synthetic = FALSE,
    source_label = "Initializing...",
    sort_station_by = "Mean_AQI"
  )

  # Initial data loader
  shiny::observe({
    def_load <- load_default_data(".")
    if (!is.null(def_load$combined_data)) {
      app_state$raw_df <- def_load$combined_data
      app_state$is_synthetic <- def_load$is_synthetic
      app_state$source_label <- if (def_load$is_synthetic) {
        "Demonstration Sample (data/synthetic_demonstration_data.csv)"
      } else {
        paste0("Loaded default data (", nrow(def_load$combined_data), " rows)")
      }

      cleaned_res <- clean_air_quality_data(app_state$raw_df)
      app_state$cleaned_df <- cleaned_res$cleaned_data
      app_state$cleaning_log <- cleaned_res$cleaning_log
      app_state$conversion_failures <- cleaned_res$conversion_failures

      # Populate initial choices
      df <- app_state$cleaned_df
      avail_stations <- sort(unique(na.omit(df$Station[df$Station_Recognized])))
      if (length(avail_stations) == 0) avail_stations <- EXPECTED_STATIONS
      avail_months <- levels(df$Month)[levels(df$Month) %in% unique(df$Month[!is.na(df$Month)])]

      shiny::updateSelectInput(session, "filter_station", choices = c("All Stations (Combined)" = "ALL", avail_stations), selected = "ALL")
      shiny::updateCheckboxGroupInput(session, "filter_stations_multi", choices = avail_stations, selected = avail_stations)
      shiny::updateSelectInput(session, "page_station_select", choices = c("All Stations (Combined)" = "ALL", avail_stations), selected = "ALL")
      shiny::updateSelectInput(session, "page_pollutant_station_select", choices = c("All Stations" = "ALL", avail_stations), selected = "ALL")
      shiny::updateSelectInput(session, "page_monthly_station_select", choices = c("All Stations (Comparison)" = "ALL", avail_stations), selected = "ALL")
      shiny::updateSelectInput(session, "filter_month", choices = c("All Available Months" = "ALL", avail_months), selected = "ALL")
    } else {
      app_state$source_label <- "No default data found. Please upload CSV."
    }
  })

  # Handle User Uploads
  shiny::observeEvent(input$upload_csv, {
    shiny::req(input$upload_csv)
    uploaded_paths <- input$upload_csv$datapath
    uploaded_names <- input$upload_csv$name

    res <- import_multiple_csvs(uploaded_paths, uploaded_names)
    if (!is.null(res$combined_data) && nrow(res$combined_data) > 0) {
      app_state$raw_df <- res$combined_data
      app_state$is_synthetic <- FALSE
      app_state$source_label <- paste0("User Uploaded (", length(uploaded_names), " file(s), ", nrow(res$combined_data), " rows)")

      cleaned_res <- clean_air_quality_data(app_state$raw_df)
      app_state$cleaned_df <- cleaned_res$cleaned_data
      app_state$cleaning_log <- cleaned_res$cleaning_log
      app_state$conversion_failures <- cleaned_res$conversion_failures

      df <- app_state$cleaned_df
      avail_stations <- sort(unique(na.omit(df$Station[df$Station_Recognized])))
      if (length(avail_stations) == 0) avail_stations <- EXPECTED_STATIONS
      avail_months <- levels(df$Month)[levels(df$Month) %in% unique(df$Month[!is.na(df$Month)])]

      shiny::updateSelectInput(session, "filter_station", choices = c("All Stations (Combined)" = "ALL", avail_stations), selected = "ALL")
      shiny::updateCheckboxGroupInput(session, "filter_stations_multi", choices = avail_stations, selected = avail_stations)
      shiny::updateSelectInput(session, "page_station_select", choices = c("All Stations (Combined)" = "ALL", avail_stations), selected = "ALL")
      shiny::updateSelectInput(session, "page_pollutant_station_select", choices = c("All Stations" = "ALL", avail_stations), selected = "ALL")
      shiny::updateSelectInput(session, "page_monthly_station_select", choices = c("All Stations (Comparison)" = "ALL", avail_stations), selected = "ALL")
      shiny::updateSelectInput(session, "filter_month", choices = c("All Available Months" = "ALL", avail_months), selected = "ALL")
      shiny::updateSelectInput(session, "filter_category", selected = "ALL")

      shiny::showNotification(
        paste0("Successfully ingested ", nrow(res$combined_data), " records from ", length(uploaded_names), " file(s)."),
        type = "message",
        duration = 5
      )
    } else {
      shiny::showNotification(paste0("Upload failed: ", res$message), type = "error", duration = 8)
    }
  })

  # Handle Restore Demo Data
  shiny::observeEvent(input$btn_restore_sample, {
    demo_path <- file.path("data", "synthetic_demonstration_data.csv")
    if (file.exists(demo_path)) {
      res <- import_single_csv(demo_path, "synthetic_demonstration_data.csv")
      app_state$raw_df <- res$data
      app_state$is_synthetic <- TRUE
      app_state$source_label <- "Demonstration Sample (data/synthetic_demonstration_data.csv)"

      cleaned_res <- clean_air_quality_data(app_state$raw_df)
      app_state$cleaned_df <- cleaned_res$cleaned_data
      app_state$cleaning_log <- cleaned_res$cleaning_log
      app_state$conversion_failures <- cleaned_res$conversion_failures

      df <- app_state$cleaned_df
      avail_stations <- sort(unique(na.omit(df$Station[df$Station_Recognized])))
      if (length(avail_stations) == 0) avail_stations <- EXPECTED_STATIONS
      avail_months <- levels(df$Month)[levels(df$Month) %in% unique(df$Month[!is.na(df$Month)])]

      shiny::updateSelectInput(session, "filter_station", choices = c("All Stations (Combined)" = "ALL", avail_stations), selected = "ALL")
      shiny::updateCheckboxGroupInput(session, "filter_stations_multi", choices = avail_stations, selected = avail_stations)
      shiny::updateSelectInput(session, "page_station_select", choices = c("All Stations (Combined)" = "ALL", avail_stations), selected = "ALL")
      shiny::updateSelectInput(session, "page_pollutant_station_select", choices = c("All Stations" = "ALL", avail_stations), selected = "ALL")
      shiny::updateSelectInput(session, "page_monthly_station_select", choices = c("All Stations (Comparison)" = "ALL", avail_stations), selected = "ALL")
      shiny::updateSelectInput(session, "filter_month", choices = c("All Available Months" = "ALL", avail_months), selected = "ALL")
      shiny::updateSelectInput(session, "filter_category", selected = "ALL")
      shiny::updateSelectInput(session, "filter_pollutant", selected = "PM2.5")

      shiny::showNotification("Restored demonstration dataset.", type = "message", duration = 4)
    } else {
      shiny::showNotification("Demonstration dataset file not found.", type = "warning", duration = 4)
    }
  })

  # ============================================================================
  # REAL-TIME QUICK-STATION BUTTONS (Overview Page)
  # ============================================================================
  output$ui_quick_station_buttons <- shiny::renderUI({
    current_sel <- if (isTRUE(input$enable_multi_station)) "MULTI" else input$filter_station
    if (is.null(current_sel)) current_sel <- "ALL"

    stations <- c("ALL" = "All Stations (5)",
                  "Kodungaiyur" = "Kodungaiyur",
                  "Koyambedu"   = "Koyambedu",
                  "Manali"      = "Manali",
                  "Perungudi"   = "Perungudi",
                  "Royapuram"   = "Royapuram")

    btns <- lapply(names(stations), function(st_key) {
      st_label <- stations[[st_key]]
      is_active <- (current_sel == st_key)
      btn_class <- if (is_active) "btn-sm btn-quick-station btn-quick-station-active" else "btn-sm btn-quick-station btn-outline-secondary"

      shiny::actionButton(
        inputId = paste0("btn_st_", tolower(gsub("[^A-Za-z0-9]", "", st_key))),
        label = st_label,
        class = btn_class
      )
    })

    shiny::tags$div(style = "display: flex; gap: 6px; flex-wrap: wrap;", btns)
  })

  # Quick Station Button Click Handlers
  shiny::observeEvent(input$btn_st_all, {
    shiny::updateCheckboxInput(session, "enable_multi_station", value = FALSE)
    shiny::updateSelectInput(session, "filter_station", selected = "ALL")
    shiny::updateSelectInput(session, "page_station_select", selected = "ALL")
    shiny::showNotification("Showing all monitoring stations.", type = "message", duration = 2)
  })

  set_quick_station <- function(st_name) {
    shiny::updateCheckboxInput(session, "enable_multi_station", value = FALSE)
    shiny::updateSelectInput(session, "filter_station", selected = st_name)
    shiny::updateSelectInput(session, "page_station_select", selected = st_name)
    shiny::showNotification(paste0("Real-time station filter applied: ", st_name), type = "message", duration = 2)
  }

  shiny::observeEvent(input$btn_st_kodungaiyur, { set_quick_station("Kodungaiyur") })
  shiny::observeEvent(input$btn_st_koyambedu,   { set_quick_station("Koyambedu") })
  shiny::observeEvent(input$btn_st_manali,      { set_quick_station("Manali") })
  shiny::observeEvent(input$btn_st_perungudi,   { set_quick_station("Perungudi") })
  shiny::observeEvent(input$btn_st_royapuram,   { set_quick_station("Royapuram") })

  # Active Filter Status Badge & Reset Link
  output$ui_active_filter_status <- shiny::renderUI({
    df_tot <- app_state$cleaned_df
    df_sub <- filtered_data()
    if (is.null(df_tot)) return(NULL)

    n_tot <- nrow(df_tot)
    n_sub <- nrow(df_sub)

    cur_st <- if (isTRUE(input$enable_multi_station)) "Multiple Stations" else input$filter_station
    cur_cat <- input$filter_category
    cur_mo <- input$filter_month

    has_filter <- (cur_st != "ALL") || (cur_cat != "ALL") || (cur_mo != "ALL") || isTRUE(input$enable_multi_station)

    if (has_filter) {
      shiny::tags$div(
        style = "display: flex; align-items: center; gap: 8px;",
        shiny::tags$span(
          style = "background: #0F2537; color: #FFFFFF; padding: 4px 10px; border-radius: 4px; font-size: 0.8rem; font-weight: 700;",
          paste0("FILTERED: ", n_sub, " of ", n_tot, " records (", round(100 * n_sub / max(1, n_tot), 1), "%)")
        ),
        shiny::actionLink(
          "link_reset_top",
          "✕ Clear Filter",
          style = "font-size: 0.8rem; color: #DC2626; font-weight: 700; text-decoration: none;"
        )
      )
    } else {
      shiny::tags$span(
        style = "color: #0D9488; font-size: 0.82rem; font-weight: 600;",
        paste0("Displaying all ", n_tot, " observations across all stations")
      )
    }
  })

  shiny::observeEvent(input$link_reset_top, {
    shiny::updateCheckboxInput(session, "enable_multi_station", value = FALSE)
    shiny::updateSelectInput(session, "filter_station", selected = "ALL")
    shiny::updateSelectInput(session, "filter_category", selected = "ALL")
    shiny::updateSelectInput(session, "filter_month", selected = "ALL")
    shiny::updateSelectInput(session, "page_station_select", selected = "ALL")
    shiny::showNotification("Filters cleared: viewing full dataset.", type = "message", duration = 2)
  })

  # Sync in-page Station Comparison selector with sidebar
  shiny::observeEvent(input$page_station_select, {
    if (input$page_station_select != input$filter_station) {
      shiny::updateCheckboxInput(session, "enable_multi_station", value = FALSE)
      shiny::updateSelectInput(session, "filter_station", selected = input$page_station_select)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$filter_station, {
    if (input$filter_station != input$page_station_select) {
      shiny::updateSelectInput(session, "page_station_select", selected = input$filter_station)
    }
  }, ignoreInit = TRUE)

  # Sync in-page pollutant choice with global filter
  shiny::observeEvent(input$page_pollutant_choice, {
    if (input$page_pollutant_choice != input$filter_pollutant) {
      shiny::updateSelectInput(session, "filter_pollutant", selected = input$page_pollutant_choice)
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$filter_pollutant, {
    if (input$filter_pollutant != input$page_pollutant_choice) {
      shiny::updateRadioButtons(session, "page_pollutant_choice", selected = input$filter_pollutant)
    }
  }, ignoreInit = TRUE)

  # Reset Filters Button
  shiny::observeEvent(input$btn_reset_filters, {
    df <- app_state$cleaned_df
    shiny::req(df)

    avail_stations <- sort(unique(na.omit(df$Station[df$Station_Recognized])))
    avail_months <- levels(df$Month)[levels(df$Month) %in% unique(df$Month[!is.na(df$Month)])]

    shiny::updateCheckboxInput(session, "enable_multi_station", value = FALSE)
    shiny::updateSelectInput(session, "filter_station", selected = "ALL")
    shiny::updateCheckboxGroupInput(session, "filter_stations_multi", selected = avail_stations)
    shiny::updateSelectInput(session, "page_station_select", selected = "ALL")
    shiny::updateSelectInput(session, "page_pollutant_station_select", selected = "ALL")
    shiny::updateSelectInput(session, "page_monthly_station_select", selected = "ALL")
    shiny::updateSelectInput(session, "filter_category", selected = "ALL")
    shiny::updateSelectInput(session, "filter_pollutant", selected = "PM2.5")
    shiny::updateRadioButtons(session, "page_pollutant_choice", selected = "PM2.5")
    shiny::updateSelectInput(session, "filter_month", selected = "ALL")

    if (any(df$Date_Valid_Full, na.rm = TRUE)) {
      valid_dates <- na.omit(df$Date_Parsed)
      valid_dates <- valid_dates[as.integer(format(valid_dates, "%Y")) >= 2000 & as.integer(format(valid_dates, "%Y")) <= 2030]
      if (length(valid_dates) > 0) {
        shiny::updateDateRangeInput(session, "filter_date_range", start = min(valid_dates), end = max(valid_dates))
      }
    }

    shiny::showNotification("All dashboard filters reset to default full view.", type = "message", duration = 3)
  })

  # ============================================================================
  # INTERACTIVE PLOT CLICKS (Real-time Cross-Filtering)
  # ============================================================================
  # Click on Station Average Bar Chart -> Isolates clicked station
  shiny::observeEvent(input$click_overview_station_avg, {
    df <- filtered_data()
    st_sum <- compute_station_summary(df)
    if (nrow(st_sum) == 0 || all(is.na(st_sum$Mean_AQI))) return()

    df_plot <- st_sum[!is.na(st_sum$Mean_AQI), ]
    df_plot <- df_plot[order(df_plot$Mean_AQI), ]

    clicked_y <- round(input$click_overview_station_avg$y)
    if (!is.na(clicked_y) && clicked_y >= 1 && clicked_y <= nrow(df_plot)) {
      selected_st <- df_plot$Station[clicked_y]
      set_quick_station(selected_st)
    }
  })

  shiny::observeEvent(input$click_station_comp_bar, {
    df <- filtered_data()
    st_sum <- compute_station_summary(df)
    if (nrow(st_sum) == 0 || all(is.na(st_sum$Mean_AQI))) return()

    df_plot <- st_sum[!is.na(st_sum$Mean_AQI), ]
    df_plot <- df_plot[order(df_plot$Mean_AQI), ]

    clicked_y <- round(input$click_station_comp_bar$y)
    if (!is.na(clicked_y) && clicked_y >= 1 && clicked_y <= nrow(df_plot)) {
      selected_st <- df_plot$Station[clicked_y]
      set_quick_station(selected_st)
    }
  })

  # Click on AQI Category Distribution Chart -> Filters by Category
  shiny::observeEvent(input$click_overview_aqi_dist, {
    clicked_x <- round(input$click_overview_aqi_dist$x)
    all_cats <- names(AQI_CATEGORY_COLORS)
    if (!is.na(clicked_x) && clicked_x >= 1 && clicked_x <= length(all_cats)) {
      selected_cat <- all_cats[clicked_x]
      shiny::updateSelectInput(session, "filter_category", selected = selected_cat)
      shiny::showNotification(paste0("Real-time category filter applied: ", selected_cat), type = "message", duration = 3)
    }
  })

  # Interactive Click on DT Table Rows
  shiny::observeEvent(input$table_overview_stations_rows_selected, {
    selected_row_idx <- input$table_overview_stations_rows_selected
    if (length(selected_row_idx) == 1) {
      df <- filtered_data()
      st_sum <- compute_station_summary(df)
      if (nrow(st_sum) >= selected_row_idx) {
        selected_st <- st_sum$Station[selected_row_idx]
        set_quick_station(selected_st)
      }
    }
  })

  shiny::observeEvent(input$table_station_detail_rows_selected, {
    selected_row_idx <- input$table_station_detail_rows_selected
    if (length(selected_row_idx) == 1) {
      df <- filtered_data()
      st_sum <- compute_station_summary(df)
      if (nrow(st_sum) >= selected_row_idx) {
        selected_st <- st_sum$Station[selected_row_idx]
        set_quick_station(selected_st)
      }
    }
  })

  # In-page Station Page Sort Metric
  shiny::observeEvent(input$page_station_sort, {
    app_state$sort_station_by <- input$page_station_sort
  })

  # Date Range Picker UI with validated 2023 dates and quick presets
  output$ui_date_range_picker <- shiny::renderUI({
    df <- app_state$cleaned_df
    if (is.null(df) || !any(df$Date_Valid_Full, na.rm = TRUE)) {
      return(shiny::tags$div(
        style = "font-size: 0.75rem; color: #64748B; margin-top: 4px; font-style: italic;",
        "Date-range picker disabled: observations lack verifiable calendar years."
      ))
    }

    valid_dates <- na.omit(df$Date_Parsed)
    valid_dates <- valid_dates[as.integer(format(valid_dates, "%Y")) >= 2000 & as.integer(format(valid_dates, "%Y")) <= 2030]

    if (length(valid_dates) == 0) {
      return(shiny::tags$div(
        style = "font-size: 0.75rem; color: #64748B; margin-top: 4px; font-style: italic;",
        "No modern calendar dates (2000-2030) found in dataset."
      ))
    }

    min_d <- min(valid_dates)
    max_d <- max(valid_dates)

    shiny::tagList(
      shiny::dateRangeInput(
        "filter_date_range",
        "Calendar Date Range:",
        start = min_d,
        end = max_d,
        min = min_d,
        max = max_d,
        format = "yyyy-mm-dd",
        separator = " to "
      ),
      shiny::tags$div(
        style = "margin-top: -6px; margin-bottom: 10px; font-size: 0.74rem; display: flex; flex-wrap: wrap; gap: 4px; align-items: center;",
        shiny::tags$span(style = "color: #64748B; font-weight: 600; margin-right: 2px;", "Quick Years:"),
        shiny::actionLink("btn_range_all", "All (2023–2026)", style = "font-weight: 700; text-decoration: none; padding: 1px 6px; background: #E2E8F0; color: #0F2537; border-radius: 4px;"),
        shiny::actionLink("btn_range_2026", "2026", style = "font-weight: 600; text-decoration: none; padding: 1px 5px; background: #E2E8F0; color: #0F2537; border-radius: 4px;"),
        shiny::actionLink("btn_range_2025", "2025", style = "font-weight: 600; text-decoration: none; padding: 1px 5px; background: #E2E8F0; color: #0F2537; border-radius: 4px;"),
        shiny::actionLink("btn_range_2024", "2024", style = "font-weight: 600; text-decoration: none; padding: 1px 5px; background: #E2E8F0; color: #0F2537; border-radius: 4px;"),
        shiny::actionLink("btn_range_2023", "2023", style = "font-weight: 600; text-decoration: none; padding: 1px 5px; background: #E2E8F0; color: #0F2537; border-radius: 4px;"),
        shiny::actionLink("btn_range_oct2026", "Oct 2026 (Latest)", style = "font-weight: 700; text-decoration: none; padding: 1px 6px; background: #CCFBF1; color: #0F766E; border-radius: 4px;")
      )
    )
  })

  # Preset Range Observers
  shiny::observeEvent(input$btn_range_all, {
    df <- app_state$cleaned_df
    shiny::req(df)
    valid_dates <- na.omit(df$Date_Parsed)
    valid_dates <- valid_dates[as.integer(format(valid_dates, "%Y")) >= 2000 & as.integer(format(valid_dates, "%Y")) <= 2030]
    if (length(valid_dates) > 0) {
      shiny::updateDateRangeInput(session, "filter_date_range", start = min(valid_dates), end = max(valid_dates))
      shiny::showNotification("Filter set to full 2023–2026 date range.", type = "message", duration = 2)
    }
  })

  shiny::observeEvent(input$btn_range_2026, {
    df <- app_state$cleaned_df
    shiny::req(df)
    valid_dates <- na.omit(df$Date_Parsed)
    y_dates <- valid_dates[as.integer(format(valid_dates, "%Y")) == 2026]
    if (length(y_dates) > 0) {
      shiny::updateDateRangeInput(session, "filter_date_range", start = min(y_dates), end = max(y_dates))
      shiny::showNotification("Filter set to Year 2026.", type = "message", duration = 2)
    }
  })

  shiny::observeEvent(input$btn_range_2025, {
    df <- app_state$cleaned_df
    shiny::req(df)
    valid_dates <- na.omit(df$Date_Parsed)
    y_dates <- valid_dates[as.integer(format(valid_dates, "%Y")) == 2025]
    if (length(y_dates) > 0) {
      shiny::updateDateRangeInput(session, "filter_date_range", start = min(y_dates), end = max(y_dates))
      shiny::showNotification("Filter set to Year 2025.", type = "message", duration = 2)
    }
  })

  shiny::observeEvent(input$btn_range_2024, {
    df <- app_state$cleaned_df
    shiny::req(df)
    valid_dates <- na.omit(df$Date_Parsed)
    y_dates <- valid_dates[as.integer(format(valid_dates, "%Y")) == 2024]
    if (length(y_dates) > 0) {
      shiny::updateDateRangeInput(session, "filter_date_range", start = min(y_dates), end = max(y_dates))
      shiny::showNotification("Filter set to Year 2024.", type = "message", duration = 2)
    }
  })

  shiny::observeEvent(input$btn_range_2023, {
    df <- app_state$cleaned_df
    shiny::req(df)
    valid_dates <- na.omit(df$Date_Parsed)
    y_dates <- valid_dates[as.integer(format(valid_dates, "%Y")) == 2023]
    if (length(y_dates) > 0) {
      shiny::updateDateRangeInput(session, "filter_date_range", start = min(y_dates), end = max(y_dates))
      shiny::showNotification("Filter set to Year 2023.", type = "message", duration = 2)
    }
  })

  shiny::observeEvent(input$btn_range_oct2026, {
    df <- app_state$cleaned_df
    shiny::req(df)
    valid_dates <- na.omit(df$Date_Parsed)
    m_dates <- valid_dates[as.integer(format(valid_dates, "%Y")) == 2026 & as.integer(format(valid_dates, "%m")) == 10]
    if (length(m_dates) > 0) {
      shiny::updateDateRangeInput(session, "filter_date_range", start = min(m_dates), end = max(m_dates))
      shiny::showNotification("Filter set to Latest Month (October 2026).", type = "message", duration = 2)
    }
  })

  shiny::observeEvent(input$btn_range_recent, {
    df <- app_state$cleaned_df
    shiny::req(df)
    valid_dates <- na.omit(df$Date_Parsed)
    m_dates <- valid_dates[as.integer(format(valid_dates, "%Y")) == 2026 & as.integer(format(valid_dates, "%m")) == 10]
    if (length(m_dates) == 0) m_dates <- valid_dates
    if (length(m_dates) > 0) {
      shiny::updateDateRangeInput(session, "filter_date_range", start = min(m_dates), end = max(m_dates))
    }
  })

  # Data Source Badge in Sidebar
  output$ui_source_badge <- shiny::renderUI({
    badge_cls <- if (app_state$is_synthetic) "atmos-advisory-warning" else "atmos-advisory-info"
    badge_title <- if (app_state$is_synthetic) "DEMONSTRATION SAMPLE DATA" else "ACTIVE DATASET"

    shiny::tags$div(
      class = paste("atmos-advisory", badge_cls),
      style = "padding: 8px 10px; margin: 8px 0 10px 0;",
      shiny::tags$div(style = "font-weight: 700; font-size: 0.76rem;", badge_title),
      shiny::tags$div(style = "font-size: 0.72rem; word-break: break-all;", app_state$source_label)
    )
  })

  # Sidebar Filter Summary
  output$ui_sidebar_filter_summary <- shiny::renderUI({
    df_full <- app_state$cleaned_df
    df_filt <- filtered_data()

    if (is.null(df_full) || nrow(df_full) == 0) {
      return(shiny::tags$div("No data loaded."))
    }

    n_tot <- nrow(df_full)
    n_filt <- nrow(df_filt)
    n_val <- sum(df_filt$Is_Valid_AQI_Obs, na.rm = TRUE)

    shiny::tags$div(
      style = "background: #F1F5F9; border-radius: 6px; padding: 10px 12px; font-size: 0.78rem;",
      shiny::tags$div(shiny::tags$strong("Active Filter Coverage:")),
      shiny::tags$div(paste0("Matching Rows: ", n_filt, " / ", n_tot, " (", round(100 * n_filt / max(1, n_tot), 1), "%)")),
      shiny::tags$div(paste0("Valid AQI Readings: ", n_val))
    )
  })

  # Reactive Filter Expression - Guaranteed Real-Time Filtering
  filtered_data <- shiny::reactive({
    df <- app_state$cleaned_df
    if (is.null(df) || nrow(df) == 0) return(data.frame())

    # 1. Station Filtering
    if (isTRUE(input$enable_multi_station)) {
      if (!is.null(input$filter_stations_multi)) {
        if (length(input$filter_stations_multi) == 0) {
          return(df[0, ])
        }
        df <- df[df$Station %in% input$filter_stations_multi, ]
      }
    } else {
      sel_st <- input$filter_station
      if (!is.null(sel_st) && sel_st != "ALL") {
        df <- df[df$Station == sel_st, ]
      }
    }

    # 2. AQI Category Filtering
    sel_cat <- input$filter_category
    if (!is.null(sel_cat) && sel_cat != "ALL") {
      df <- df[as.character(df$AQI.Index) == sel_cat, ]
    }

    # 3. Month Filtering
    sel_mo <- input$filter_month
    if (!is.null(sel_mo) && sel_mo != "ALL") {
      df <- df[as.character(df$Month) == sel_mo, ]
    }

    # 4. Date Range Filtering
    if (!is.null(input$filter_date_range) && any(df$Date_Valid_Full, na.rm = TRUE)) {
      start_d <- tryCatch(as.Date(input$filter_date_range[1]), error = function(e) NA)
      end_d <- tryCatch(as.Date(input$filter_date_range[2]), error = function(e) NA)
      if (!is.na(start_d) && !is.na(end_d)) {
        in_range <- !is.na(df$Date_Parsed) & df$Date_Parsed >= start_d & df$Date_Parsed <= end_d
        df <- df[in_range, ]
      }
    }

    df
  })

  # Validation reactive
  validation_results <- shiny::reactive({
    df <- app_state$cleaned_df
    if (is.null(df) || nrow(df) == 0) return(NULL)
    validate_air_quality_data(df, app_state$raw_df)
  })

  # ============================================================================
  # PAGE 1: OVERVIEW OUTPUTS (Real-Time Reactive)
  # ============================================================================
  output$ui_overview_advisory <- shiny::renderUI({
    if (app_state$is_synthetic) {
      advisory_banner(
        "Demonstration Dataset Active",
        "The metrics and charts below are currently rendered from synthetic demonstration data generated to reflect typical Chennai air monitoring conditions and data quality edge cases. Use the sidebar file uploader to load your official station CSV files anytime.",
        type = "info"
      )
    } else {
      NULL
    }
  })

  output$ui_overview_kpis <- shiny::renderUI({
    df <- filtered_data()
    kpis <- compute_overview_kpis(df)

    shiny::tags$div(
      class = "atmos-kpi-grid",
      kpi_card(
        "Total Sample Records",
        kpis$total_obs,
        "Total observations matching current filter",
        badge_label = "Imported",
        badge_color = "#0F4C81"
      ),
      kpi_card(
        "Validated AQI Readings",
        kpis$valid_aqi_obs,
        "Observations with verified AQI and station",
        badge_label = "Validated",
        badge_color = "#16A34A"
      ),
      kpi_card(
        "Active Stations",
        kpis$station_count,
        "Chennai monitoring stations represented",
        badge_label = "Stations",
        badge_color = "#0D9488"
      ),
      kpi_card(
        "Overall Mean AQI",
        if (is.na(kpis$overall_avg_aqi)) "N/A" else kpis$overall_avg_aqi,
        "Arithmetic mean across filtered records",
        badge_label = "AQI Index",
        badge_color = "#D97706"
      ),
      kpi_card(
        "Peak Mean Station",
        kpis$highest_avg_station,
        if (is.na(kpis$highest_avg_value)) "No observations" else paste0("Mean AQI: ", kpis$highest_avg_value),
        badge_label = "Peak Area",
        badge_color = "#DC2626"
      )
    )
  })

  output$plot_overview_aqi_dist <- shiny::renderPlot({
    df <- filtered_data()
    aqi_dist <- compute_aqi_distribution(df)
    plot_aqi_category_distribution(aqi_dist, highlight_category = if (input$filter_category != "ALL") input$filter_category else NULL)
  })

  output$plot_overview_station_avg <- shiny::renderPlot({
    df <- filtered_data()
    st_sum <- compute_station_summary(df)
    current_st <- if (isTRUE(input$enable_multi_station)) input$filter_stations_multi else (if (input$filter_station != "ALL") input$filter_station else NULL)
    plot_station_avg_aqi(st_sum, highlight_station = current_st)
  })

  output$plot_overview_monthly <- shiny::renderPlot({
    df <- filtered_data()
    m_res <- compute_monthly_aqi(df)
    plot_monthly_aqi_trends(m_res)
  })

  output$table_overview_stations <- DT::renderDT({
    df <- filtered_data()
    st_sum <- compute_station_summary(df)
    if (nrow(st_sum) == 0) return(data.frame(Message = "No station data available."))

    display_df <- st_sum[, c("Rank", "Station", "Valid_Obs", "Mean_AQI", "Median_AQI", "Missing_Pct")]
    names(display_df) <- c("Rank", "Station", "Valid Obs", "Mean AQI", "Median AQI", "Missing %")

    DT::datatable(
      display_df,
      selection = "single",
      options = list(pageLength = 5, dom = "t", ordering = TRUE),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })

  output$text_overview_interpretation <- shiny::renderText({
    df <- filtered_data()
    kpis <- compute_overview_kpis(df)
    st_sum <- compute_station_summary(df)
    aqi_dist <- compute_aqi_distribution(df)
    generate_dynamic_interpretation(kpis, st_sum, aqi_dist)
  })

  # ============================================================================
  # PAGE 2: STATION COMPARISON OUTPUTS
  # ============================================================================
  output$plot_station_comp_bar <- shiny::renderPlot({
    df <- filtered_data()
    st_sum <- compute_station_summary(df)
    current_st <- if (input$page_station_select != "ALL") input$page_station_select else NULL
    plot_station_avg_aqi(st_sum, highlight_station = current_st)
  })

  output$table_station_detail <- DT::renderDT({
    df <- filtered_data()
    st_sum <- compute_station_summary(df)
    if (nrow(st_sum) == 0) return(data.frame(Message = "No station records available."))

    sort_metric <- app_state$sort_station_by
    if (sort_metric %in% names(st_sum)) {
      st_sum <- st_sum[order(-st_sum[[sort_metric]]), ]
      st_sum$Rank <- seq_len(nrow(st_sum))
    }

    display_tab <- st_sum[, c("Rank", "Station", "Valid_Obs", "Mean_AQI", "Median_AQI", "Min_AQI", "Max_AQI", "Missing_Pct")]
    names(display_tab) <- c("Rank", "Station", "Valid Obs", "Mean AQI", "Median AQI", "Min AQI", "Max AQI", "Missing %")

    DT::datatable(
      display_tab,
      selection = "single",
      options = list(pageLength = 5, dom = "tip", ordering = TRUE),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })

  output$plot_station_cat_breakdown <- shiny::renderPlot({
    df <- filtered_data()
    plot_station_category_breakdown(df)
  })

  # ============================================================================
  # PAGE 3: POLLUTANT ANALYSIS OUTPUTS (Real-Time Synchronized)
  # ============================================================================
  pollutant_data <- shiny::reactive({
    df <- filtered_data()
    if (!is.null(input$page_pollutant_station_select) && input$page_pollutant_station_select != "ALL") {
      df <- df[df$Station == input$page_pollutant_station_select, ]
    }
    pol_col <- input$page_pollutant_choice
    compute_pollutant_stats(df, pol_col)
  })

  output$ui_pollutant_kpis <- shiny::renderUI({
    pol_stats <- pollutant_data()
    ov <- pol_stats$overall
    pol_name <- input$page_pollutant_choice
    unit <- pol_stats$unit

    shiny::tags$div(
      class = "atmos-kpi-grid",
      kpi_card(
        paste0("Mean ", pol_name),
        if (is.na(ov$mean)) "N/A" else ov$mean,
        unit,
        badge_label = "Average",
        badge_color = "#0F4C81"
      ),
      kpi_card(
        paste0("Median ", pol_name),
        if (is.na(ov$median)) "N/A" else ov$median,
        unit,
        badge_label = "Median",
        badge_color = "#0D9488"
      ),
      kpi_card(
        "Valid Measurements",
        ov$valid,
        paste0(ov$missing, " missing readings"),
        badge_label = "Coverage",
        badge_color = "#16A34A"
      ),
      kpi_card(
        paste0("Min / Max ", pol_name),
        if (is.na(ov$min)) "N/A" else paste0(ov$min, " – ", ov$max),
        "Range across valid observations",
        badge_label = "Range",
        badge_color = "#64748B"
      )
    )
  })

  output$plot_pollutant_box <- shiny::renderPlot({
    df <- filtered_data()
    if (!is.null(input$page_pollutant_station_select) && input$page_pollutant_station_select != "ALL") {
      df <- df[df$Station == input$page_pollutant_station_select, ]
    }
    pol_stats <- pollutant_data()
    plot_pollutant_boxplot(df, input$page_pollutant_choice, pol_stats$unit)
  })

  output$plot_pollutant_hist <- shiny::renderPlot({
    df <- filtered_data()
    if (!is.null(input$page_pollutant_station_select) && input$page_pollutant_station_select != "ALL") {
      df <- df[df$Station == input$page_pollutant_station_select, ]
    }
    pol_stats <- pollutant_data()
    plot_pollutant_histogram(df, input$page_pollutant_choice, pol_stats$unit)
  })

  output$plot_pollutant_corr <- shiny::renderPlot({
    df <- filtered_data()
    cmat <- compute_pollutant_correlations(df)
    plot_correlation_heatmap(cmat)
  })

  output$table_pollutant_station <- DT::renderDT({
    pol_stats <- pollutant_data()
    st_tab <- pol_stats$station_table
    if (nrow(st_tab) == 0) return(data.frame(Message = "No pollutant data by station."))

    names(st_tab) <- c("Station", "Valid Obs", "Missing Obs", "Mean", "Median", "Min", "Max")
    DT::datatable(
      st_tab,
      options = list(pageLength = 5, dom = "tip", ordering = TRUE),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })

  # ============================================================================
  # PAGE 4: MONTHLY TRENDS OUTPUTS
  # ============================================================================
  monthly_data_r <- shiny::reactive({
    df <- filtered_data()
    compute_monthly_aqi(df)
  })

  output$ui_monthly_advisory <- shiny::renderUI({
    m_res <- monthly_data_r()
    advisory_banner(
      "Methodological Notice on Time Resolution",
      m_res$date_quality_advisory,
      type = if (grepl("ADVISORY", m_res$date_quality_advisory)) "warning" else "info"
    )
  })

  output$plot_monthly_trend_main <- shiny::renderPlot({
    m_res <- monthly_data_r()
    mode <- if (!is.null(input$page_monthly_view_mode)) input$page_monthly_view_mode else "chrono"
    plot_monthly_aqi_trends(m_res, view_mode = mode)
  })

  output$plot_monthly_station_trend <- shiny::renderPlot({
    m_res <- monthly_data_r()
    mode <- if (!is.null(input$page_monthly_view_mode)) input$page_monthly_view_mode else "chrono"
    sel_st <- if (input$page_monthly_station_select != "ALL") input$page_monthly_station_select else NULL
    plot_station_monthly_comparison(m_res, selected_stations = sel_st, view_mode = mode)
  })

  output$table_monthly_summary <- DT::renderDT({
    m_res <- monthly_data_r()
    mode <- if (!is.null(input$page_monthly_view_mode)) input$page_monthly_view_mode else "chrono"
    if (mode == "chrono" && !is.null(m_res$chronological_summary) && nrow(m_res$chronological_summary) > 0) {
      df <- m_res$chronological_summary
      names(df) <- c("Year - Month", "Mean AQI", "Median AQI", "Observation Count")
      DT::datatable(
        df,
        options = list(pageLength = 12, dom = "tip", ordering = TRUE),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    } else {
      df <- m_res$monthly_summary
      if (nrow(df) == 0) return(data.frame(Message = "No monthly summary data."))
      names(df) <- c("Month", "Mean AQI", "Median AQI", "Observation Count")
      DT::datatable(
        df,
        options = list(pageLength = 12, dom = "t", ordering = FALSE),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    }
  })

  # ============================================================================
  # PAGE 5: DATA QUALITY OUTPUTS
  # ============================================================================
  output$ui_dq_health_banner <- shiny::renderUI({
    val <- validation_results()
    if (is.null(val)) return(NULL)

    score <- val$overall_health_score
    b_type <- if (score >= 90) "info" else if (score >= 70) "warning" else "danger"

    advisory_banner(
      paste0("Overall Dataset Health Rating: ", score, "%"),
      paste0("Calculated from ", nrow(app_state$cleaned_df), " loaded observations. ",
             "The health score assesses complete station mapping, valid non-negative AQI measurements, and absence of exact duplicate entries."),
      type = b_type
    )
  })

  output$table_dq_audit <- DT::renderDT({
    val <- validation_results()
    if (is.null(val) || nrow(val$dq_summary) == 0) return(data.frame())

    DT::datatable(
      val$dq_summary,
      options = list(pageLength = 8, dom = "t", ordering = FALSE),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })

  output$table_dq_missing <- DT::renderDT({
    val <- validation_results()
    if (is.null(val) || nrow(val$missing_counts) == 0) return(data.frame())

    names(val$missing_counts) <- c("Field", "Missing Rows", "Total Rows", "Missing %")
    DT::datatable(
      val$missing_counts,
      options = list(pageLength = 10, dom = "t", ordering = TRUE),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })

  output$table_dq_flagged <- DT::renderDT({
    val <- validation_results()
    if (is.null(val) || nrow(val$flagged_records) == 0) {
      return(data.frame(Status = "No anomalous records flagged."))
    }

    DT::datatable(
      val$flagged_records,
      options = list(pageLength = 10, dom = "tip", ordering = TRUE),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })

  output$table_dq_cleaning_log <- DT::renderDT({
    clog <- app_state$cleaning_log
    if (is.null(clog) || nrow(clog) == 0) return(data.frame(Status = "No transformations logged."))

    DT::datatable(
      clog,
      options = list(pageLength = 10, dom = "t", ordering = FALSE),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })

  # Export Flagged Records CSV
  output$download_flagged_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("AtmosVue_Flagged_Records_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      val <- validation_results()
      out_df <- if (!is.null(val)) val$flagged_records else data.frame()
      readr::write_csv(out_df, file)
    }
  )

  # ============================================================================
  # PAGE 6: DATA EXPLORER OUTPUTS
  # ============================================================================
  output$table_data_explorer <- DT::renderDT({
    view_type <- input$explorer_data_view
    df <- if (view_type == "raw") {
      app_state$raw_df
    } else {
      filtered_data()
    }

    if (is.null(df) || nrow(df) == 0) {
      return(data.frame(Message = "No records to display under active filters."))
    }

    if (view_type == "cleaned") {
      display_cols <- intersect(
        c("Date_Raw", "Date_Parsed", "Station", "AQI.Value", "AQI.Index",
          "Prominent.Pollutant", "PM2.5", "PM10", "NO2", "SO2", "CO",
          "Month", "Date_Quality_Flag", "source_file"),
        names(df)
      )
      df <- df[, display_cols, drop = FALSE]
    }

    DT::datatable(
      df,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "Bfrtip"
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  })

  # Export Filtered Data CSV
  output$download_filtered_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("AtmosVue_Filtered_Data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      readr::write_csv(filtered_data(), file)
    }
  )

  # Export Full Cleaned Data CSV
  output$download_clean_all_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("AtmosVue_All_Cleaned_Data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      readr::write_csv(app_state$cleaned_df, file)
    }
  )
}

# ------------------------------------------------------------------------------
# 6. Launch Application
# ------------------------------------------------------------------------------
shiny::shinyApp(ui = ui, server = server)
