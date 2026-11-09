# Semiconductors II
# 02_build_wsts_base.R
# Purpose:
#   - Parse the WSTS Historical Billings workbook.
#   - Build tidy monthly and 3MMA regional datasets for 2010-2025.
#   - Validate that regional totals match Worldwide.
#   - No China split or policy variables are added yet.

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "readr",
  "lubridate",
  "tibble"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(tibble)

# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

wsts_path <- file.path(
  "data",
  "raw",
  "wsts_historical_billings.xlsx"
)

processed_dir <- file.path(
  "data",
  "processed"
)

output_tables_dir <- file.path(
  "output",
  "tables"
)

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  output_tables_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(wsts_path)) {
  stop(
    paste0(
      "File not found: ", wsts_path,
      "\nExpected location: data/raw/wsts_historical_billings.xlsx"
    )
  )
}

# ------------------------------------------------------------
# 3. Constants
# ------------------------------------------------------------

start_year <- 2010L
end_year <- 2025L

source_regions_expected <- c(
  "Americas",
  "Europe",
  "Japan",
  "Asia Pacific",
  "Worldwide"
)

regions_expected <- c(
  "America",
  "Europe",
  "Japan",
  "Asia Pacific",
  "Worldwide"
)

month_names <- month.name

# ------------------------------------------------------------
# 4. Parser
# ------------------------------------------------------------

parse_wsts_sheet <- function(sheet_name) {

  raw <- read_excel(
    wsts_path,
    sheet = sheet_name,
    col_names = FALSE,
    .name_repair = "unique"
  )

  # Ensure all temporary column names are valid before using dplyr.
  names(raw) <- paste0("col_", seq_len(ncol(raw)))

  # Workbook layout:
  # row 4 -> month names
  # row 5 onward -> repeated blocks:
  # year / Americas / Europe / Japan / Asia Pacific / Worldwide
  # WSTS source label "Americas" is normalized to project label "America".

  data_rows <- raw[5:nrow(raw), , drop = FALSE]

  first_col <- data_rows[[1]] %>%
    as.character() %>%
    str_trim()

  year_rows <- which(
    str_detect(first_col, "^\\d{4}$")
  )

  if (length(year_rows) == 0) {
    stop(
      paste0(
        "No year rows detected in sheet: ",
        sheet_name
      )
    )
  }

  parsed_list <- vector(
    mode = "list",
    length = length(year_rows)
  )

  for (i in seq_along(year_rows)) {

    row_index <- year_rows[i]

    year_value <- as.integer(
      first_col[row_index]
    )

    region_start <- row_index + 1
    region_end <- row_index + 5

    if (region_end > nrow(data_rows)) {
      next
    }

    block <- data_rows[
      region_start:region_end,
      ,
      drop = FALSE
    ]

    region_labels <- block[[1]] %>%
      as.character() %>%
      str_trim()

    if (!all(source_regions_expected %in% region_labels)) {
      warning(
        paste0(
          "Unexpected region structure for year ",
          year_value,
          " in sheet ",
          sheet_name,
          ". Found: ",
          paste(region_labels, collapse = " | ")
        )
      )
      next
    }

    # Columns 2:13 correspond to January-December.
    month_block <- block[, 2:13, drop = FALSE]

    names(month_block) <- month_names

    month_block <- month_block %>%
      mutate(
        region = region_labels,
        year = year_value,
        .before = 1
      )

    parsed_list[[i]] <- month_block
  }

  parsed <- bind_rows(parsed_list) %>%
    filter(
      year >= start_year,
      year <= end_year
    ) %>%
    pivot_longer(
      cols = all_of(month_names),
      names_to = "month",
      values_to = "sales_usd_thousands"
    ) %>%
    mutate(
      sales_usd_thousands = as.numeric(
        sales_usd_thousands
      ),
      month_number = match(
        month,
        month.name
      ),
      date = make_date(
        year,
        month_number,
        1
      ),
      sales_usd_billions =
        sales_usd_thousands / 1e6,
      region = recode(
        region,
        "Americas" = "America"
      )
    ) %>%
    select(
      date,
      year,
      month_number,
      month,
      region,
      sales_usd_thousands,
      sales_usd_billions
    ) %>%
    arrange(
      date,
      factor(
        region,
        levels = regions_expected
      )
    )

  parsed
}

# ------------------------------------------------------------
# 5. Build both datasets
# ------------------------------------------------------------

monthly_data <- parse_wsts_sheet(
  "Monthly Data"
)

mma3_data <- parse_wsts_sheet(
  "3MMA"
)

# ------------------------------------------------------------
# 6. Validation function
# ------------------------------------------------------------

validate_world_total <- function(
  data,
  dataset_name,
  tolerance_usd_thousands = 5
) {

  validation <- data %>%
    select(
      date,
      region,
      sales_usd_thousands
    ) %>%
    pivot_wider(
      names_from = region,
      values_from = sales_usd_thousands
    ) %>%
    mutate(
      calculated_worldwide =
        America +
        Europe +
        Japan +
        `Asia Pacific`,
      difference =
        Worldwide -
        calculated_worldwide,
      abs_difference =
        abs(difference),
      valid =
        abs_difference <= tolerance_usd_thousands,
      dataset =
        dataset_name
    ) %>%
    select(
      dataset,
      date,
      Worldwide,
      calculated_worldwide,
      difference,
      abs_difference,
      valid
    )

  validation
}

monthly_validation <- validate_world_total(
  monthly_data,
  "Monthly Data"
)

mma3_validation <- validate_world_total(
  mma3_data,
  "3MMA"
)

validation_all <- bind_rows(
  monthly_validation,
  mma3_validation
)

# ------------------------------------------------------------
# 7. Completeness checks
# ------------------------------------------------------------

expected_months <-
  (end_year - start_year + 1) * 12

completeness_summary <- tibble(
  dataset = c(
    "Monthly Data",
    "3MMA"
  ),
  observations = c(
    nrow(monthly_data),
    nrow(mma3_data)
  ),
  unique_dates = c(
    n_distinct(monthly_data$date),
    n_distinct(mma3_data$date)
  ),
  expected_dates = expected_months,
  missing_sales_values = c(
    sum(is.na(
      monthly_data$sales_usd_thousands
    )),
    sum(is.na(
      mma3_data$sales_usd_thousands
    ))
  )
)

# ------------------------------------------------------------
# 8. Save processed files
# ------------------------------------------------------------

write_csv(
  monthly_data,
  file.path(
    processed_dir,
    "wsts_monthly_2010_2025.csv"
  )
)

write_csv(
  mma3_data,
  file.path(
    processed_dir,
    "wsts_3mma_2010_2025.csv"
  )
)

write_csv(
  validation_all,
  file.path(
    output_tables_dir,
    "wsts_regional_total_validation.csv"
  )
)

write_csv(
  completeness_summary,
  file.path(
    output_tables_dir,
    "wsts_completeness_summary.csv"
  )
)

# ------------------------------------------------------------
# 9. Console report
# ------------------------------------------------------------

cat("\n==============================\n")
cat("WSTS BASE BUILD COMPLETED\n")
cat("==============================\n\n")

cat(
  "Period:",
  start_year,
  "-",
  end_year,
  "\n\n"
)

cat("MONTHLY DATA\n")
cat(
  "Rows:",
  nrow(monthly_data),
  "\n"
)
cat(
  "Dates:",
  n_distinct(
    monthly_data$date
  ),
  "\n"
)
cat(
  "Missing sales:",
  sum(
    is.na(
      monthly_data$sales_usd_thousands
    )
  ),
  "\n\n"
)

cat("3MMA DATA\n")
cat(
  "Rows:",
  nrow(mma3_data),
  "\n"
)
cat(
  "Dates:",
  n_distinct(
    mma3_data$date
  ),
  "\n"
)
cat(
  "Missing sales:",
  sum(
    is.na(
      mma3_data$sales_usd_thousands
    )
  ),
  "\n\n"
)

cat("REGIONAL TOTAL VALIDATION\n")

print(
  validation_all %>%
    count(
      dataset,
      valid
    )
)

cat(
  "\nMaximum absolute discrepancy ",
  "(thousand USD):\n"
)

print(
  validation_all %>%
    group_by(
      dataset
    ) %>%
    summarise(
      max_abs_difference =
        max(
          abs_difference,
          na.rm = TRUE
        ),
      .groups = "drop"
    )
)

cat(
  "\nProcessed files created:\n"
)

cat(
  "- data/processed/wsts_monthly_2010_2025.csv\n"
)

cat(
  "- data/processed/wsts_3mma_2010_2025.csv\n"
)

cat(
  "\nValidation files created:\n"
)

cat(
  "- output/tables/wsts_regional_total_validation.csv\n"
)

cat(
  "- output/tables/wsts_completeness_summary.csv\n"
)
