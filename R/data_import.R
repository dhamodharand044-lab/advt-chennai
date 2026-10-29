# ==============================================================================
# AtmosVue: Comparative Air Quality Analytics and Visualization for Chennai
# Module: R/data_import.R
# Purpose: Safe ingestion of air-quality CSV files with schema checks and provenance
# ==============================================================================

#' Expected columns in Chennai Air Quality datasets
EXPECTED_COLUMNS <- c(
  "Date", "Station", "SO2", "NO2", "CO",
  "PM2.5", "PM10", "AQI.Index", "AQI.Value", "Prominent.Pollutant"
)

#' Expected Chennai monitoring stations
EXPECTED_STATIONS <- c(
  "Kodungaiyur", "Koyambedu", "Manali", "Perungudi", "Royapuram"
)

#' Safe read of a single CSV file
#'
#' @param file_path Character path to the CSV file
#' @param original_name Character label for file provenance
#' @return A list containing:
#'   - status: "success", "warning", or "error"
#'   - message: Human-readable diagnostic message
#'   - data: Tibble/data.frame of raw data (NULL on error)
#'   - row_count: Number of rows read
#'   - column_count: Number of columns read
#'   - columns_found: Character vector of column names found
#'   - missing_expected: Character vector of expected columns not present
#'   - unexpected_columns: Character vector of columns present but unexpected
import_single_csv <- function(file_path, original_name = basename(file_path)) {
  if (!file.exists(file_path)) {
    return(list(
      status = "error",
      message = paste0("File not found: ", file_path),
      data = NULL,
      row_count = 0,
      column_count = 0,
      columns_found = character(0),
      missing_expected = EXPECTED_COLUMNS,
      unexpected_columns = character(0)
    ))
  }

  raw_df <- tryCatch({
    # Use readr::read_csv with col_types = cols(.default = "c") to preserve raw strings
    readr::read_csv(file_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  }, error = function(e) {
    # Fallback to utils::read.csv with stringsAsFactors = FALSE
    tryCatch({
      utils::read.csv(file_path, colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE)
    }, error = function(e2) {
      NULL
    })
  })

  if (is.null(raw_df) || nrow(raw_df) == 0) {
    return(list(
      status = "error",
      message = paste0("Failed to read data or file is empty: ", original_name),
      data = NULL,
      row_count = 0,
      column_count = 0,
      columns_found = character(0),
      missing_expected = EXPECTED_COLUMNS,
      unexpected_columns = character(0)
    ))
  }

  cols_present <- names(raw_df)
  missing_exp <- setdiff(EXPECTED_COLUMNS, cols_present)
  unexpected <- setdiff(cols_present, EXPECTED_COLUMNS)

  # Attach provenance column
  raw_df$source_file <- original_name

  status <- if (length(missing_exp) == 0) "success" else "warning"
  msg <- if (length(missing_exp) == 0) {
    paste0("Successfully imported ", nrow(raw_df), " rows from ", original_name, ".")
  } else {
    paste0("Imported ", nrow(raw_df), " rows, but missing expected columns: ",
           paste(missing_exp, collapse = ", "))
  }

  list(
    status = status,
    message = msg,
    data = raw_df,
    row_count = nrow(raw_df),
    column_count = ncol(raw_df),
    columns_found = cols_present,
    missing_expected = missing_exp,
    unexpected_columns = unexpected
  )
}

#' Import and combine multiple CSV files safely
#'
#' @param file_paths Named or unnamed vector/list of file paths
#' @param original_names Optional vector of original filenames
#' @return A list containing:
#'   - status: "success", "warning", or "error"
#'   - message: Combined diagnostic report
#'   - combined_data: Combined raw data frame
#'   - files_summary: Data frame summarizing each file imported
import_multiple_csvs <- function(file_paths, original_names = NULL) {
  if (is.null(original_names)) {
    original_names <- basename(file_paths)
  }

  results <- list()
  summaries <- list()

  for (i in seq_along(file_paths)) {
    fpath <- file_paths[[i]]
    fname <- original_names[[i]]
    res <- import_single_csv(fpath, fname)
    results[[i]] <- res
    summaries[[i]] <- data.frame(
      Filename = fname,
      Status = res$status,
      Rows = res$row_count,
      Columns = res$column_count,
      Missing_Columns = paste(res$missing_expected, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }

  summary_df <- do.call(rbind, summaries)
  successful_data <- lapply(results, function(r) r$data)
  successful_data <- successful_data[!sapply(successful_data, is.null)]

  if (length(successful_data) == 0) {
    return(list(
      status = "error",
      message = "No valid data could be imported from the supplied files.",
      combined_data = NULL,
      files_summary = summary_df
    ))
  }

  # Combine rows safely binding matching columns
  combined <- tryCatch({
    dplyr::bind_rows(successful_data)
  }, error = function(e) {
    # Base fallback
    do.call(rbind, successful_data)
  })

  overall_status <- if (all(summary_df$Status == "success")) "success" else "warning"

  list(
    status = overall_status,
    message = paste0("Combined ", nrow(combined), " total records across ", length(successful_data), " file(s)."),
    combined_data = combined,
    files_summary = summary_df
  )
}

#' Load default project dataset
#' Tries existing data files in order: known UUID names, then synthetic demo data
#'
#' @param base_dir Project directory
#' @return Import result list
load_default_data <- function(base_dir = ".") {
  candidate_files <- c(
    file.path(base_dir, "04c4903c-62e5-4437-bbd4-a03e5063e0d2.csv"),
    file.path(base_dir, "42961d2d-778e-4c80-b15f-b31b7df07b4d.csv"),
    file.path(base_dir, "data", "04c4903c-62e5-4437-bbd4-a03e5063e0d2.csv"),
    file.path(base_dir, "data", "42961d2d-778e-4c80-b15f-b31b7df07b4d.csv"),
    file.path(base_dir, "data", "chennai_air_quality_realtime.csv"),
    file.path(base_dir, "data", "synthetic_demonstration_data.csv")
  )

  existing_candidates <- candidate_files[file.exists(candidate_files)]

  if (length(existing_candidates) == 0) {
    return(list(
      status = "not_found",
      message = "No default data files found. Please upload CSV files via the sidebar.",
      combined_data = NULL,
      files_summary = NULL,
      is_synthetic = FALSE
    ))
  }

  # Prioritize non-synthetic if both exist
  non_synthetic <- existing_candidates[!grepl("synthetic_demonstration_data", existing_candidates)]
  target_files <- if (length(non_synthetic) > 0) non_synthetic else existing_candidates

  is_synth <- any(grepl("synthetic_demonstration_data", target_files))

  res <- import_multiple_csvs(target_files)
  res$is_synthetic <- is_synth
  res
}
