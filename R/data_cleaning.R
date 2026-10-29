# ==============================================================================
# AtmosVue: Comparative Air Quality Analytics and Visualization for Chennai
# Module: R/data_cleaning.R
# Purpose: Data cleaning, type conversion, robust date parsing, and audit trails
# ==============================================================================

# Standard AQI Categories according to Central Pollution Control Board (CPCB) India
STANDARD_AQI_CATEGORIES <- c(
  "Good", "Satisfactory", "Moderate", "Poor", "Very Poor", "Severe"
)

# Standard monitoring stations in Chennai
STANDARD_STATIONS <- c(
  "Kodungaiyur", "Koyambedu", "Manali", "Perungudi", "Royapuram"
)

# Numeric columns expected
NUMERIC_COLUMNS <- c("SO2", "NO2", "CO", "PM2.5", "PM10", "AQI.Value")

#' Parse and analyze raw date strings conservatively and strictly
#'
#' @param date_strings Character vector of raw date strings
#' @return A data frame with parsed date fields and diagnostic flags
parse_dates_conservatively <- function(date_strings) {
  n <- length(date_strings)
  if (n == 0) {
    return(data.frame(
      Date_Raw = character(0),
      Date_Parsed = as.Date(character(0)),
      Year = integer(0),
      Month = factor(character(0)),
      Month_Num = integer(0),
      Day = integer(0),
      Date_Has_Year = logical(0),
      Date_Valid_Full = logical(0),
      Date_Quality_Flag = character(0),
      stringsAsFactors = FALSE
    ))
  }

  cleaned_dates <- trimws(as.character(date_strings))
  cleaned_dates[cleaned_dates == "" | is.na(cleaned_dates) | tolower(cleaned_dates) %in% c("na", "null", "none", "-")] <- NA_character_

  parsed_date <- rep(as.Date(NA), n)
  year_vec <- rep(NA_integer_, n)
  month_name_vec <- rep(NA_character_, n)
  month_num_vec <- rep(NA_integer_, n)
  day_vec <- rep(NA_integer_, n)
  has_year_vec <- rep(FALSE, n)
  valid_full_vec <- rep(FALSE, n)
  quality_flag_vec <- rep("Missing Date", n)

  month_map <- c(
    "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6,
    "JUL" = 7, "JULY" = 7, "AUG" = 8, "SEP" = 9, "SEPT" = 9, "OCT" = 10,
    "NOV" = 11, "DEC" = 12
  )
  month_labels <- c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )

  for (i in seq_len(n)) {
    d_raw <- cleaned_dates[i]
    if (is.na(d_raw)) {
      quality_flag_vec[i] <- "Missing Date String"
      next
    }

    # Normalize multiple spaces
    d_norm <- gsub("\\s+", " ", d_raw)

    # --------------------------------------------------------------------------
    # PATTERN 1: Standard YYYY-MM-DD or YYYY/MM/DD (Strict 4-digit Year First)
    # Example: "2023-05-01", "2023/10/25"
    # --------------------------------------------------------------------------
    if (grepl("^\\s*([12][0-9]{3})[-/]([0-9]{1,2})[-/]([0-9]{1,2})\\s*$", d_norm)) {
      m <- regexec("^\\s*([12][0-9]{3})[-/]([0-9]{1,2})[-/]([0-9]{1,2})\\s*$", d_norm)
      parts <- regmatches(d_norm, m)[[1]]
      d_year <- as.integer(parts[2])
      d_month <- as.integer(parts[3])
      d_day <- as.integer(parts[4])

      if (d_year >= 2000 && d_year <= 2030 && d_month >= 1 && d_month <= 12 && d_day >= 1 && d_day <= 31) {
        constructed <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", d_year, d_month, d_day)), error = function(e) NA)
        if (!is.na(constructed)) {
          parsed_date[i] <- constructed
          year_vec[i] <- d_year
          month_num_vec[i] <- d_month
          month_name_vec[i] <- month_labels[d_month]
          day_vec[i] <- d_day
          has_year_vec[i] <- TRUE
          valid_full_vec[i] <- TRUE
          quality_flag_vec[i] <- "Valid Full Date (YYYY-MM-DD)"
          next
        }
      }
    }

    # --------------------------------------------------------------------------
    # PATTERN 2: Standard DD-MM-YYYY or DD/MM/YYYY (Strict 4-digit Year at end!)
    # Example: "25-10-2023", "01/05/2023"
    # Note: Regex requires EXACTLY 4 digits at the end to prevent "25" being parsed as 1925!
    # --------------------------------------------------------------------------
    if (grepl("^\\s*([0-9]{1,2})[-/]([0-9]{1,2})[-/]([12][0-9]{3})\\s*$", d_norm)) {
      m <- regexec("^\\s*([0-9]{1,2})[-/]([0-9]{1,2})[-/]([12][0-9]{3})\\s*$", d_norm)
      parts <- regmatches(d_norm, m)[[1]]
      d_day <- as.integer(parts[2])
      d_month <- as.integer(parts[3])
      d_year <- as.integer(parts[4])

      if (d_year >= 2000 && d_year <= 2030 && d_month >= 1 && d_month <= 12 && d_day >= 1 && d_day <= 31) {
        constructed <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", d_year, d_month, d_day)), error = function(e) NA)
        if (!is.na(constructed)) {
          parsed_date[i] <- constructed
          year_vec[i] <- d_year
          month_num_vec[i] <- d_month
          month_name_vec[i] <- month_labels[d_month]
          day_vec[i] <- d_day
          has_year_vec[i] <- TRUE
          valid_full_vec[i] <- TRUE
          quality_flag_vec[i] <- "Valid Full Date (DD-MM-YYYY)"
          next
        }
      }
    }

    # --------------------------------------------------------------------------
    # PATTERN 3: Text Month with 4-digit Year (e.g., "16JULY 2023", "16-Jul-2023", "16 July 2023")
    # --------------------------------------------------------------------------
    clean_text <- gsub("([0-9]+)([A-Za-z]+)", "\\1 \\2", d_norm)
    clean_text <- gsub("([A-Za-z]+)([0-9]+)", "\\1 \\2", clean_text)
    clean_text <- gsub("[-/]+", " ", toupper(clean_text))
    year_match <- regmatches(clean_text, regexpr("\\b(20[0-9]{2})\\b", clean_text))

    tokens <- strsplit(trimws(clean_text), "\\s+")[[1]]
    if (length(tokens) >= 2) {
      t_day <- suppressWarnings(as.integer(tokens[1]))
      t_month_token <- tokens[2]
      m_sub <- substr(t_month_token, 1, 3)
      m_idx <- unname(month_map[m_sub])

      t_year <- if (length(year_match) > 0) as.integer(year_match[1]) else NA_integer_

      if (!is.na(t_day) && !is.na(m_idx)) {
        day_vec[i] <- t_day
        month_num_vec[i] <- m_idx
        month_name_vec[i] <- month_labels[m_idx]

        if (!is.na(t_year) && t_year >= 2000 && t_year <= 2030) {
          constructed <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", t_year, m_idx, t_day)), error = function(e) NA)
          if (!is.na(constructed)) {
            parsed_date[i] <- constructed
            year_vec[i] <- t_year
            has_year_vec[i] <- TRUE
            valid_full_vec[i] <- TRUE
            quality_flag_vec[i] <- "Valid Full Date (Parsed Text)"
            next
          }
        } else {
          # Month & day present, but NO verifiable 4-digit year. Never invent a year!
          has_year_vec[i] <- FALSE
          valid_full_vec[i] <- FALSE
          quality_flag_vec[i] <- "Month & Day Only (Missing Year)"
          next
        }
      }
    }

    # --------------------------------------------------------------------------
    # PATTERN 4: Compact Month-Day tokens without year (e.g. "02MAY", "15AUG")
    # --------------------------------------------------------------------------
    compact_match <- regexec("^([0-9]{1,2})([A-Z]+)$", toupper(d_norm))
    compact_parts <- regmatches(toupper(d_norm), compact_match)[[1]]
    if (length(compact_parts) == 3) {
      t_day <- suppressWarnings(as.integer(compact_parts[2]))
      t_month_token <- compact_parts[3]
      m_sub <- substr(t_month_token, 1, 3)
      m_idx <- unname(month_map[m_sub])

      if (!is.na(t_day) && !is.na(m_idx)) {
        day_vec[i] <- t_day
        month_num_vec[i] <- m_idx
        month_name_vec[i] <- month_labels[m_idx]
        has_year_vec[i] <- FALSE
        valid_full_vec[i] <- FALSE
        quality_flag_vec[i] <- "Month & Day Only (Missing Year)"
        next
      }
    }

    # If all parsing attempts failed:
    quality_flag_vec[i] <- "Unparseable Date Format"
  }

  data.frame(
    Date_Raw = as.character(date_strings),
    Date_Parsed = parsed_date,
    Year = year_vec,
    Month = factor(month_name_vec, levels = month_labels),
    Month_Num = month_num_vec,
    Day = day_vec,
    Date_Has_Year = has_year_vec,
    Date_Valid_Full = valid_full_vec,
    Date_Quality_Flag = quality_flag_vec,
    stringsAsFactors = FALSE
  )
}

#' Clean and standardize raw air quality dataset
#'
#' @param raw_df Data frame containing raw imported records
#' @return A list containing:
#'   - cleaned_data: Data frame with typed, standardized columns
#'   - cleaning_log: Data frame summarizing cleaning operations and counts
#'   - conversion_failures: Data frame of values that failed numeric conversion
clean_air_quality_data <- function(raw_df) {
  if (is.null(raw_df) || nrow(raw_df) == 0) {
    return(list(
      cleaned_data = data.frame(),
      cleaning_log = data.frame(Action = "Empty Input", Details = "No records to clean"),
      conversion_failures = data.frame()
    ))
  }

  log_entries <- list()
  failures_list <- list()

  df <- raw_df

  # 1. Trim whitespace on all character columns
  char_cols <- names(df)[sapply(df, is.character)]
  for (cname in char_cols) {
    df[[cname]] <- trimws(df[[cname]])
    na_idx <- which(df[[cname]] == "" | tolower(df[[cname]]) %in% c("na", "n/a", "null", "none", "-"))
    if (length(na_idx) > 0) {
      df[[cname]][na_idx] <- NA_character_
    }
  }
  log_entries[[length(log_entries) + 1]] <- data.frame(
    Step = "Whitespace & Blank String Standardization",
    Count = nrow(df),
    Notes = "Trimmed whitespace and converted empty/placeholder strings to NA."
  )

  # 2. Strict Date Parsing (Preserving Date_Raw)
  if ("Date" %in% names(df)) {
    date_df <- parse_dates_conservatively(df$Date)
    df$Date_Raw <- df$Date
    df$Date_Parsed <- date_df$Date_Parsed
    df$Year <- date_df$Year
    df$Month <- date_df$Month
    df$Month_Num <- date_df$Month_Num
    df$Day <- date_df$Day
    df$Date_Has_Year <- date_df$Date_Has_Year
    df$Date_Valid_Full <- date_df$Date_Valid_Full
    df$Date_Quality_Flag <- date_df$Date_Quality_Flag

    full_dates_count <- sum(date_df$Date_Valid_Full, na.rm = TRUE)
    month_only_count <- sum(!date_df$Date_Valid_Full & !is.na(date_df$Month), na.rm = TRUE)
    unparseable_count <- sum(date_df$Date_Quality_Flag %in% c("Unparseable Date Format", "Missing Date String"), na.rm = TRUE)

    log_entries[[length(log_entries) + 1]] <- data.frame(
      Step = "Strict Date Parsing & Audit Trail",
      Count = full_dates_count,
      Notes = paste0("Valid full calendar dates: ", full_dates_count,
                     "; Month/Day only (missing year): ", month_only_count,
                     "; Missing/unparseable dates: ", unparseable_count, ".")
    )
  } else {
    df$Date_Raw <- NA_character_
    df$Date_Parsed <- as.Date(NA)
    df$Year <- NA_integer_
    df$Month <- factor(NA_character_, levels = c(
      "January", "February", "March", "April", "May", "June",
      "July", "August", "September", "October", "November", "December"
    ))
    df$Month_Num <- NA_integer_
    df$Day <- NA_integer_
    df$Date_Has_Year <- FALSE
    df$Date_Valid_Full <- FALSE
    df$Date_Quality_Flag <- "Date Column Missing"
  }

  # 3. Station standardization
  if ("Station" %in% names(df)) {
    df$Station_Raw <- df$Station
    station_clean <- df$Station
    for (st in STANDARD_STATIONS) {
      match_idx <- which(tolower(station_clean) == tolower(st))
      if (length(match_idx) > 0) {
        station_clean[match_idx] <- st
      }
    }
    df$Station <- station_clean
    df$Station_Recognized <- df$Station %in% STANDARD_STATIONS

    unrec_st <- sum(!df$Station_Recognized, na.rm = TRUE)
    log_entries[[length(log_entries) + 1]] <- data.frame(
      Step = "Station Name Normalization",
      Count = sum(df$Station_Recognized, na.rm = TRUE),
      Notes = paste0("Standardized recognized stations. Unrecognized or missing stations: ", unrec_st)
    )
  } else {
    df$Station_Raw <- NA_character_
    df$Station <- NA_character_
    df$Station_Recognized <- FALSE
  }

  # 4. Safe numeric conversions
  for (col in NUMERIC_COLUMNS) {
    if (col %in% names(df)) {
      raw_vals <- df[[col]]
      suppressWarnings({
        num_vals <- as.numeric(raw_vals)
      })

      failed_idx <- which(!is.na(raw_vals) & is.na(num_vals))
      if (length(failed_idx) > 0) {
        failures_list[[length(failures_list) + 1]] <- data.frame(
          Row = failed_idx,
          Column = col,
          Original_Value = raw_vals[failed_idx],
          Reason = "Non-numeric characters in numeric field",
          stringsAsFactors = FALSE
        )
      }

      df[[col]] <- num_vals
    } else {
      df[[col]] <- NA_real_
    }
  }

  conv_fail_count <- if (length(failures_list) > 0) sum(sapply(failures_list, nrow)) else 0
  log_entries[[length(log_entries) + 1]] <- data.frame(
    Step = "Numeric Field Conversion",
    Count = conv_fail_count,
    Notes = paste0("Safely converted numeric fields without imputing zeros. Conversion failures: ", conv_fail_count)
  )

  # 5. AQI Index (Category) Normalization
  if ("AQI.Index" %in% names(df)) {
    df$AQI.Index_Raw <- df$AQI.Index
    cat_clean <- df$AQI.Index

    for (cat in STANDARD_AQI_CATEGORIES) {
      match_idx <- which(tolower(cat_clean) == tolower(cat))
      if (length(match_idx) > 0) {
        cat_clean[match_idx] <- cat
      }
    }
    df$AQI.Index <- factor(cat_clean, levels = STANDARD_AQI_CATEGORIES)
    df$AQI.Index_Valid <- !is.na(df$AQI.Index)

    inv_cat <- sum(is.na(df$AQI.Index) & !is.na(df$AQI.Index_Raw))
    log_entries[[length(log_entries) + 1]] <- data.frame(
      Step = "AQI Category Standardization",
      Count = sum(df$AQI.Index_Valid, na.rm = TRUE),
      Notes = paste0("Validated CPCB AQI categories. Non-standard/unrecognized categories: ", inv_cat)
    )
  } else {
    df$AQI.Index_Raw <- NA_character_
    df$AQI.Index <- factor(NA_character_, levels = STANDARD_AQI_CATEGORIES)
    df$AQI.Index_Valid <- FALSE
  }

  # 6. Flag duplicates
  dup_cols <- intersect(c("Date_Raw", "Station", "AQI.Value", "PM2.5", "PM10"), names(df))
  if (length(dup_cols) > 0) {
    df$Is_Duplicate <- duplicated(df[dup_cols])
  } else {
    df$Is_Duplicate <- FALSE
  }
  dup_count <- sum(df$Is_Duplicate, na.rm = TRUE)
  log_entries[[length(log_entries) + 1]] <- data.frame(
    Step = "Duplicate Detection",
    Count = dup_count,
    Notes = paste0("Identified ", dup_count, " duplicate rows based on key columns.")
  )

  # 7. Valid AQI Observation Flag
  df$Is_Valid_AQI_Obs <- !is.na(df$AQI.Value) & df$AQI.Value >= 0 & df$Station_Recognized

  cleaning_log_df <- do.call(rbind, log_entries)
  conv_fail_df <- if (length(failures_list) > 0) do.call(rbind, failures_list) else data.frame(
    Row = integer(0), Column = character(0), Original_Value = character(0), Reason = character(0)
  )

  list(
    cleaned_data = df,
    cleaning_log = cleaning_log_df,
    conversion_failures = conv_fail_df
  )
}
