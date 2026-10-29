# AtmosVue: Comparative Air Quality Analytics and Visualization for Chennai

> **Explore Air. Understand Impact.**  
> An academic-grade, reproducible R Shiny analytics platform for comparative air quality assessment across monitoring stations in Chennai, India.

---

## 1. Project Overview & Problem Statement

Urban air pollution poses severe public health, ecological, and economic challenges across rapidly developing metropolitan centers. In Chennai, atmospheric dynamics are shaped by coastal sea-breeze circulations, intensive vehicular corridors, power generation clusters, and petrochemical industrial zones in North Chennai.

**AtmosVue** is designed to provide environmental analysts, municipal authorities, students, and citizens with an interactive, transparent, and statistically rigorous platform to:
- Compare air quality indices (AQI) across five key continuous monitoring stations:
  1. **Kodungaiyur** (Northern municipal solid waste/industrial precinct)
  2. **Koyambedu** (Central-western transportation and wholesale market hub)
  3. **Manali** (Northern petrochemical and heavy industrial cluster)
  4. **Perungudi** (Southern IT corridor and marshland/landfill vicinity)
  5. **Royapuram** (Dense coastal and harbour-adjacent corridor)
- Identify primary criteria pollutants driving elevated AQI readings (`PM2.5`, `PM10`, `NO2`, `SO2`, `CO`).
- Perform exploratory month-by-month comparative analysis.
- Expose data completeness, missing value patterns, and irregular timestamp notations without silent filtering or unjustified imputation.

---

## 2. Key Objectives & Analytical Questions

1. **Station Disparity Analysis:** How significantly do average and median AQI levels differ across geographical and industrial zones in Chennai?
2. **Pollutant Fingerprinting:** Which pollutants dominate ambient air quality at each station, and what are their pairwise correlations?
3. **Temporal Exploration:** What monthly variations emerge in the sampled observations, and how do they relate to seasonal atmospheric transitions?
4. **Data Integrity & Provenance:** How do missing measurements, inconsistent date formats, and category reporting discrepancies affect analytical conclusions?

---

## 3. Technology Stack

- **Language:** R (>= 4.0.0)
- **Interactive Framework:** [Shiny](https://shiny.posit.co/)
- **UI Framework & Theming:** [bslib](https://rstudio.github.io/bslib/) (Bootstrap 5, Inter typography, responsive card layouts)
- **Data Visualizations:** [ggplot2](https://ggplot2.tidyverse.org/) (custom academic publication theme with zero baselines)
- **Data Manipulation:** [dplyr](https://dplyr.tidyverse.org/), [tidyr](https://tidyr.tidyverse.org/), [readr](https://readr.tidyverse.org/)
- **Interactive Tables:** [DT](https://rstudio.github.io/DT/) (DataTables interface with search, pagination, and column filtering)
- **Development Environment:** RStudio Desktop / Posit

---

## 4. Project Folder Structure

```text
ADVT_Chennai/
├── atmosvue-chennai.Rproj           # RStudio project configuration file
├── app.R                            # Main Shiny application entry point
├── atmosvue_analysis.R              # Standalone batch analysis & figure exporter
├── README.md                        # Complete project documentation
├── .gitignore                       # Git exclusion rules
├── data/
│   ├── chennai_air_quality_realtime.csv # Multi-year observations (Jan 2023 – Oct 2026) across all 5 stations
│   └── synthetic_demonstration_data.csv # Representative demo dataset with edge cases
├── R/
│   ├── data_import.R                # Safe ingestion, schema validation & provenance
│   ├── data_cleaning.R              # Conservative parsing, date extraction & cleaning log
│   ├── data_validation.R            # Data health auditing, range checks & anomaly flags
│   ├── data_analysis.R              # Statistical summaries, correlations & interpretations
│   ├── visualization_functions.R    # Standardized ggplot2 charts with empty-state handling
│   └── dashboard_modules.R          # Modular UI pages, KPI cards & advisory banners
├── www/
│   └── custom.css                   # Custom responsive styling and theme variables
└── tests/
    └── test_data_processing.R       # Automated test suite for data pipeline & calculations
```

---

## 5. Dataset Schema

AtmosVue ingests standard continuous ambient air quality monitoring datasets with the following fields:

| Field Name | Type | Description | Handling / Cleaning Rule |
| :--- | :--- | :--- | :--- |
| `Date` | Character | Observation date string (e.g., `01-05-2023`, `02MAY`, `16JULY 2023`) | Preserved in `Date_Raw`. Parsed to full calendar date if year is present; otherwise flagged as month/day only. Missing years are **never** fabricated. |
| `Station` | Character | Monitoring station name | Standardized against the 5 recognized Chennai stations. |
| `SO2` | Numeric | Sulphur Dioxide concentration | Converted safely to numeric. Missing values preserved as `NA` (never replaced with zero). |
| `NO2` | Numeric | Nitrogen Dioxide concentration | Converted safely to numeric. Preserved as `NA` if missing. |
| `CO` | Numeric | Carbon Monoxide concentration | Converted safely to numeric. Preserved as `NA` if missing. |
| `PM2.5` | Numeric | Fine particulate matter (≤ 2.5 µm) | Converted safely to numeric. Preserved as `NA` if missing. |
| `PM10` | Numeric | Coarse particulate matter (≤ 10 µm) | Converted safely to numeric. Preserved as `NA` if missing. |
| `AQI.Index` | Character / Factor | Air Quality category classification | Validated against CPCB standard bands (`Good`, `Satisfactory`, `Moderate`, `Poor`, `Very Poor`, `Severe`). |
| `AQI.Value` | Numeric | Calculated Air Quality Index | Numeric value; validated for non-negativity. |
| `Prominent.Pollutant` | Character | Primary pollutant responsible for AQI | Categorical identifier. |

---

## 6. Installation & Execution Guide

### Step 1: Install Required R Packages

Open R or RStudio and run:

```r
install.packages(c(
  "shiny",
  "bslib",
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "DT"
))
```

### Step 2: Open the RStudio Project

Double-click `atmosvue-chennai.Rproj` in your local file explorer to open RStudio with the correct working directory.

### Step 3: Run the Shiny Dashboard

You can start AtmosVue in any of the following ways:

- **Option A:** Open `app.R` in RStudio and click the **Run App** button in the top-right corner of the editor window.
- **Option B:** Run the command in your R console:
  ```r
  shiny::runApp()
  ```

### Step 4: Run the Standalone Analysis (Batch Mode)

To execute the data pipeline and generate the four standalone figures (`.png` files) without launching the browser UI:

```r
source("atmosvue_analysis.R")
```

### Step 5: Run Automated Tests

To verify that data parsing, cleaning, validation, and analytics calculations behave correctly:

```r
source("tests/test_data_processing.R")
```

---

## 7. Dashboard Architecture & Features

The dashboard consists of seven interactive modules:

1. **Overview Dashboard:**
   - Real-time KPI metric cards (Total Records, Valid AQI Observations, Active Stations, Overall Mean AQI, Peak Station).
   - AQI Category distribution chart.
   - Station-wise mean AQI bar chart (zero baseline).
   - Aggregated monthly mean trajectory.
   - Station comparison summary table.
   - Dynamically generated analytical synthesis summarizing key observations.
2. **Station Comparison:**
   - In-depth station rankings by mean AQI.
   - Statistical spread (Mean, Median, Min, Max, Missing %).
   - 100% stacked proportional category distribution across stations.
3. **Pollutant Analysis:**
   - Selector for `PM2.5`, `PM10`, `NO2`, `SO2`, and `CO`.
   - Station distribution boxplots highlighting medians, interquartile ranges (IQR), and extreme values.
   - Concentration frequency histograms.
   - Pairwise correlation heatmap with explicit disclaimer that correlation does not establish causation.
   - Station-level breakdown table.
4. **Monthly Trends:**
   - Exploratory monthly average line and point plots.
   - Station-wise monthly trajectories.
   - Prominent methodological notice warning users when timestamps lack verified calendar years.
5. **Data Quality & Auditing:**
   - Dataset health rating gauge.
   - Data integrity checks (missing values, duplicate rows, unrecognized stations, non-standard categories, numeric range checks).
   - Column-wise missingness table.
   - Interactive table of flagged records with one-click CSV export for transparent review.
   - Full data cleaning and transformation audit log.
6. **Data Explorer:**
   - Toggle between cleaned standardized data and raw imported records.
   - Searchable, sortable, paginated DT datatable with column filters.
   - Download buttons for active filtered data and complete cleaned datasets.
7. **About Project:**
   - Comprehensive background, research questions, methodology, and software architecture.

---

## 8. Data Ethics & Methodological Principles

- **No Zero Imputation:** Missing pollutant concentrations are kept as `NA`. Imputing missing values with zero would artificially depress pollution averages and distort risk assessments.
- **No Silent Outlier Deletion:** Unusual observations (e.g., AQI > 500) are flagged for analyst inspection rather than deleted automatically.
- **Conservative Date Handling:** Dates missing a year (such as `02MAY`) are treated as month-day groupings, not continuous time series. Missing years are never invented.
- **Observational Integrity:** Visualizations and automated text summaries explicitly state that observational monitoring data cannot establish direct causal relationships without controlled meteorology and emission modeling.

---

## 9. Repository Information & Future Enhancements

- **Repository:** [https://github.com/dhamodharand044-lab/atmosvue-chennai](https://github.com/dhamodharand044-lab/atmosvue-chennai)
- **Author:** AtmosVue Environmental Analytics Team
- **Future Improvements:**
  - Integration of meteorological covariates (wind speed, relative humidity, boundary layer height).
  - Spatial GIS mapping using Leaflet for station coordinates across Chennai.
  - Integration of live API endpoints from official environmental portals (e.g., CPCB / TNPCB) when public API tokens are available.
