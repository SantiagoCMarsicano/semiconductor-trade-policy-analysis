# Semiconductors II
# 06_build_sia_regional_3mma.R
#
# Purpose:
#   - Build the main econometric dataset using a single consistent data vintage.
#   - Re-parse the cached official SIA/WSTS monthly chart PDFs.
#   - Extract the WSTS source region "Americas" and normalize it as America,
#     together with Europe, Japan, China, Asia Pacific/All Other and Total.
#   - Use current 3MMA values for 2016-2025.
#   - Reconstruct 2015 from the "Last Year" column in the 2016 PDFs.
#   - Validate that the five regions add up to Total inside the SIA vintage.
#   - Compare the SIA vintage with the current WSTS Historical Billings dataset
#     as an external audit only.
#
# Main output:
#   data/processed/sia_regional_3mma_2015_2025.csv
#
# Important methodological rule:
#   The main econometric dataset must not mix SIA contemporary-vintage values
#   with current WSTS Historical Billings values.

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "lubridate",
  "tibble",
  "tidyr",
  "pdftools"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tibble)
library(tidyr)
library(pdftools)

# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

sources_path <- file.path(
  "data",
  "raw",
  "source_registry",
  "sia_2016_2025_extraction_sources.csv"
)

pdf_cache_dir <- file.path(
  "data",
  "raw",
  "sia_charts"
)

wsts_path <- file.path(
  "data",
  "processed",
  "wsts_3mma_2010_2025.csv"
)

processed_dir <- file.path(
  "data",
  "processed"
)

output_dir <- file.path(
  "output",
  "tables"
)

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(sources_path)) {
  stop(
    paste0(
      "Source registry not found: ",
      sources_path,
      "\nRun R/05_build_sia_china_3mma.R first."
    )
  )
}

if (!file.exists(wsts_path)) {
  stop(
    paste0(
      "WSTS processed dataset not found: ",
      wsts_path,
      "\nRun R/02_build_wsts_base.R first."
    )
  )
}

# ------------------------------------------------------------
# 3. Read source registry
# ------------------------------------------------------------

sources <- read_csv(
  sources_path,
  show_col_types = FALSE
) %>%
  mutate(
    reference_month = as.Date(
      reference_month
    ),
    publication_date = as.Date(
      publication_date
    )
  ) %>%
  arrange(
    reference_month
  )

expected_source_months <- seq(
  as.Date("2016-01-01"),
  as.Date("2025-12-01"),
  by = "month"
)

if (
  nrow(sources) != 120 ||
  !all(
    expected_source_months %in%
      sources$reference_month
  )
) {
  stop(
    paste0(
      "The extraction-source registry is not complete for 2016-2025. ",
      "Expected 120 monthly sources."
    )
  )
}

# ------------------------------------------------------------
# 4. Region definitions
# ------------------------------------------------------------

region_patterns <- c(
  america =
    "Americas",  # exact SIA/WSTS source label
  europe =
    "Europe",
  japan =
    "Japan",
  china =
    "China",
  asia_pacific_other =
    "Asia\\s+Pacific/All\\s+Other",
  total =
    "Total"
)

# ------------------------------------------------------------
# 5. Generic PDF row extractor
# ------------------------------------------------------------

extract_region_rows <- function(
  pdf_text_full,
  region_pattern
) {

  # Each market normally appears three times:
  #
  # 1. Month-to-Month Sales
  # 2. Year-to-Year Sales
  # 3. Three-Month-Moving Average Sales
  #
  # Expected row structure:
  #
  # Region | comparison value | current value | percent change

  pattern <- paste0(
    region_pattern,
    "\\s+",
    "([0-9]+(?:\\.[0-9]+)?)",
    "\\s+",
    "([0-9]+(?:\\.[0-9]+)?)",
    "\\s+",
    "(-?[0-9]+(?:\\.[0-9]+)?)",
    "\\s*%"
  )

  matches <- str_match_all(
    pdf_text_full,
    regex(
      pattern,
      ignore_case = TRUE
    )
  )[[1]]

  if (nrow(matches) < 3) {
    return(
      tibble(
        row_number = integer(),
        comparison_value = double(),
        current_value = double(),
        percent_change = double()
      )
    )
  }

  tibble(
    row_number = seq_len(
      nrow(matches)
    ),
    comparison_value = as.numeric(
      matches[, 2]
    ),
    current_value = as.numeric(
      matches[, 3]
    ),
    percent_change = as.numeric(
      matches[, 4]
    )
  )
}

# ------------------------------------------------------------
# 6. Parse all regions from one cached PDF
# ------------------------------------------------------------

parse_regional_pdf <- function(
  reference_month,
  local_path
) {

  pages <- pdf_text(
    local_path
  )

  pdf_text_full <- paste(
    pages,
    collapse = "\n"
  )

  extracted <- lapply(
    region_patterns,
    function(pattern) {
      extract_region_rows(
        pdf_text_full,
        pattern
      )
    }
  )

  row_counts <- sapply(
    extracted,
    nrow
  )

  if (any(row_counts < 3)) {

    return(
      tibble(
        reference_month =
          reference_month,
        parse_status =
          "failed_region_rows",
        america_row_count =
          row_counts["america"],
        europe_row_count =
          row_counts["europe"],
        japan_row_count =
          row_counts["japan"],
        china_row_count =
          row_counts["china"],
        asia_pacific_other_row_count =
          row_counts["asia_pacific_other"],
        total_row_count =
          row_counts["total"]
      )
    )
  }

  # Current 3MMA:
  # third appearance -> current_value.
  current_values <- sapply(
    extracted,
    function(x) {
      x$current_value[3]
    }
  )

  # Previous-year same month:
  # second appearance -> comparison_value.
  last_year_values <- sapply(
    extracted,
    function(x) {
      x$comparison_value[2]
    }
  )

  # Internal PDF consistency:
  # current month should repeat identically in all three tables.
  consistency <- sapply(
    extracted,
    function(x) {
      max(
        x$current_value[1:3]
      ) -
        min(
          x$current_value[1:3]
        ) <
        0.001
    }
  )

  tibble(
    reference_month =
      reference_month,
    parse_status =
      "ok",

    america_current =
      current_values["america"],
    europe_current =
      current_values["europe"],
    japan_current =
      current_values["japan"],
    china_current =
      current_values["china"],
    asia_pacific_other_current =
      current_values["asia_pacific_other"],
    total_current =
      current_values["total"],

    america_last_year =
      last_year_values["america"],
    europe_last_year =
      last_year_values["europe"],
    japan_last_year =
      last_year_values["japan"],
    china_last_year =
      last_year_values["china"],
    asia_pacific_other_last_year =
      last_year_values["asia_pacific_other"],
    total_last_year =
      last_year_values["total"],

    america_consistent =
      consistency["america"],
    europe_consistent =
      consistency["europe"],
    japan_consistent =
      consistency["japan"],
    china_consistent =
      consistency["china"],
    asia_pacific_other_consistent =
      consistency["asia_pacific_other"],
    total_consistent =
      consistency["total"]
  )
}

# ------------------------------------------------------------
# 7. Parse the 120 cached PDFs
# ------------------------------------------------------------

cat(
  "\n==============================\n"
)
cat(
  "SIA REGIONAL 3MMA BUILD\n"
)
cat(
  "==============================\n\n"
)

cat(
  "Cached PDFs expected: 120\n\n"
)

regional_results <- vector(
  mode = "list",
  length = nrow(sources)
)

for (
  i in seq_len(
    nrow(sources)
  )
) {

  source_row <- sources[
    i,
  ]

  reference_month <-
    source_row$reference_month

  local_path <- file.path(
    pdf_cache_dir,
    paste0(
      "sia_",
      format(
        reference_month,
        "%Y-%m"
      ),
      ".pdf"
    )
  )

  cat(
    "Parsing ",
    format(
      reference_month,
      "%Y-%m"
    ),
    " ... ",
    sep = ""
  )

  if (!file.exists(local_path)) {

    regional_results[[i]] <- tibble(
      reference_month =
        reference_month,
      parse_status =
        "pdf_missing"
    )

    cat(
      "PDF MISSING\n"
    )

    next
  }

  parsed <- tryCatch(
    parse_regional_pdf(
      reference_month,
      local_path
    ),
    error = function(e) {

      message(
        "\nParse error: ",
        conditionMessage(e)
      )

      tibble(
        reference_month =
          reference_month,
        parse_status =
          "parse_error"
      )
    }
  )

  regional_results[[i]] <- parsed

  cat(
    parsed$parse_status,
    "\n"
  )
}

regional_audit <- bind_rows(
  regional_results
) %>%
  left_join(
    sources %>%
      select(
        reference_month,
        publication_date,
        article_url,
        chart_url,
        source_origin
      ),
    by = "reference_month"
  ) %>%
  arrange(
    reference_month
  )

regional_audit_path <- file.path(
  output_dir,
  "sia_regional_pdf_parse_audit.csv"
)

write_csv(
  regional_audit,
  regional_audit_path
)

failed_regional <- regional_audit %>%
  filter(
    parse_status != "ok"
  )

if (nrow(failed_regional) > 0) {
  stop(
    paste0(
      nrow(failed_regional),
      " cached PDFs could not be parsed. ",
      "Inspect output/tables/sia_regional_pdf_parse_audit.csv"
    )
  )
}

# ------------------------------------------------------------
# 8. Build direct 2016-2025 dataset
# ------------------------------------------------------------

series_2016_2025 <- regional_audit %>%
  transmute(
    date =
      reference_month,

    america_3mma_usd_billions =
      america_current,

    europe_3mma_usd_billions =
      europe_current,

    japan_3mma_usd_billions =
      japan_current,

    china_3mma_usd_billions =
      china_current,

    asia_pacific_other_3mma_usd_billions =
      asia_pacific_other_current,

    total_3mma_usd_billions =
      total_current,

    source_method =
      "sia_pdf_current_3mma",

    source_reference_month =
      reference_month,

    publication_date =
      publication_date,

    chart_url =
      chart_url
  )

# ------------------------------------------------------------
# 9. Reconstruct 2015 from 2016 PDFs
# ------------------------------------------------------------

series_2015 <- regional_audit %>%
  filter(
    year(
      reference_month
    ) == 2016
  ) %>%
  transmute(
    date =
      reference_month %m-%
      years(1),

    america_3mma_usd_billions =
      america_last_year,

    europe_3mma_usd_billions =
      europe_last_year,

    japan_3mma_usd_billions =
      japan_last_year,

    china_3mma_usd_billions =
      china_last_year,

    asia_pacific_other_3mma_usd_billions =
      asia_pacific_other_last_year,

    total_3mma_usd_billions =
      total_last_year,

    source_method =
      "reconstructed_from_2016_last_year",

    source_reference_month =
      reference_month,

    publication_date =
      publication_date,

    chart_url =
      chart_url
  )

# ------------------------------------------------------------
# 10. Combine 2015-2025
# ------------------------------------------------------------

sia_regional <- bind_rows(
  series_2015,
  series_2016_2025
) %>%
  arrange(
    date
  ) %>%
  mutate(

    asia_pacific_total_3mma_usd_billions =
      china_3mma_usd_billions +
      asia_pacific_other_3mma_usd_billions,

    calculated_total_3mma_usd_billions =
      america_3mma_usd_billions +
      europe_3mma_usd_billions +
      japan_3mma_usd_billions +
      china_3mma_usd_billions +
      asia_pacific_other_3mma_usd_billions,

    china_share_asia_pacific =
      china_3mma_usd_billions /
      asia_pacific_total_3mma_usd_billions,

    america_share_total =
      america_3mma_usd_billions /
      total_3mma_usd_billions,

    europe_share_total =
      europe_3mma_usd_billions /
      total_3mma_usd_billions,

    japan_share_total =
      japan_3mma_usd_billions /
      total_3mma_usd_billions,

    china_share_total =
      china_3mma_usd_billions /
      total_3mma_usd_billions,

    asia_pacific_other_share_total =
      asia_pacific_other_3mma_usd_billions /
      total_3mma_usd_billions
  )

# ------------------------------------------------------------
# 11. Completeness validation
# ------------------------------------------------------------

expected_dates <- tibble(
  date = seq(
    as.Date(
      "2015-01-01"
    ),
    as.Date(
      "2025-12-01"
    ),
    by = "month"
  )
)

completeness <- expected_dates %>%
  left_join(
    sia_regional %>%
      select(
        date,
        america_3mma_usd_billions,
        europe_3mma_usd_billions,
        japan_3mma_usd_billions,
        china_3mma_usd_billions,
        asia_pacific_other_3mma_usd_billions,
        total_3mma_usd_billions
      ),
    by = "date"
  ) %>%
  mutate(
    complete =
      if_all(
        -date,
        ~ !is.na(.x)
      )
  )

# ------------------------------------------------------------
# 12. Internal SIA-vintage total validation
# ------------------------------------------------------------

# Each regional figure is rounded to two decimals.
# With five regional components, total rounding error can
# theoretically approach 0.025 billion USD.
#
# We therefore use a 0.03 billion USD tolerance.

internal_tolerance_billions <- 0.03

internal_validation <- sia_regional %>%
  transmute(
    date,

    america_3mma_usd_billions,
    europe_3mma_usd_billions,
    japan_3mma_usd_billions,
    china_3mma_usd_billions,
    asia_pacific_other_3mma_usd_billions,

    calculated_total_3mma_usd_billions,

    reported_total_3mma_usd_billions =
      total_3mma_usd_billions,

    difference_billions =
      calculated_total_3mma_usd_billions -
      reported_total_3mma_usd_billions,

    abs_difference_billions =
      abs(
        difference_billions
      ),

    valid =
      abs_difference_billions <=
      internal_tolerance_billions
  )

# ------------------------------------------------------------
# 13. External vintage comparison with WSTS Historical Billings
# ------------------------------------------------------------

wsts_historical <- read_csv(
  wsts_path,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(
      date
    )
  ) %>%
  filter(
    date >= as.Date(
      "2015-01-01"
    ),
    date <= as.Date(
      "2025-12-01"
    )
  ) %>%
  select(
    date,
    region,
    sales_usd_billions
  ) %>%
  pivot_wider(
    names_from = region,
    values_from = sales_usd_billions
  ) %>%
  rename(
    wsts_america =
      America,
    wsts_europe =
      Europe,
    wsts_japan =
      Japan,
    wsts_asia_pacific =
      `Asia Pacific`,
    wsts_worldwide =
      Worldwide
  )

vintage_comparison <- sia_regional %>%
  select(
    date,
    sia_america =
      america_3mma_usd_billions,
    sia_europe =
      europe_3mma_usd_billions,
    sia_japan =
      japan_3mma_usd_billions,
    sia_asia_pacific =
      asia_pacific_total_3mma_usd_billions,
    sia_total =
      total_3mma_usd_billions
  ) %>%
  left_join(
    wsts_historical,
    by = "date"
  ) %>%
  mutate(
    gap_america =
      sia_america -
      wsts_america,

    gap_europe =
      sia_europe -
      wsts_europe,

    gap_japan =
      sia_japan -
      wsts_japan,

    gap_asia_pacific =
      sia_asia_pacific -
      wsts_asia_pacific,

    gap_total =
      sia_total -
      wsts_worldwide
  )

# ------------------------------------------------------------
# 14. Summarize vintage gaps
# ------------------------------------------------------------

vintage_summary <- tibble(
  region = c(
    "America",
    "Europe",
    "Japan",
    "Asia Pacific",
    "Worldwide"
  ),

  correlation = c(
    cor(
      vintage_comparison$sia_america,
      vintage_comparison$wsts_america,
      use = "complete.obs"
    ),
    cor(
      vintage_comparison$sia_europe,
      vintage_comparison$wsts_europe,
      use = "complete.obs"
    ),
    cor(
      vintage_comparison$sia_japan,
      vintage_comparison$wsts_japan,
      use = "complete.obs"
    ),
    cor(
      vintage_comparison$sia_asia_pacific,
      vintage_comparison$wsts_asia_pacific,
      use = "complete.obs"
    ),
    cor(
      vintage_comparison$sia_total,
      vintage_comparison$wsts_worldwide,
      use = "complete.obs"
    )
  ),

  mean_gap_billions = c(
    mean(
      vintage_comparison$gap_america,
      na.rm = TRUE
    ),
    mean(
      vintage_comparison$gap_europe,
      na.rm = TRUE
    ),
    mean(
      vintage_comparison$gap_japan,
      na.rm = TRUE
    ),
    mean(
      vintage_comparison$gap_asia_pacific,
      na.rm = TRUE
    ),
    mean(
      vintage_comparison$gap_total,
      na.rm = TRUE
    )
  ),

  max_abs_gap_billions = c(
    max(
      abs(
        vintage_comparison$gap_america
      ),
      na.rm = TRUE
    ),
    max(
      abs(
        vintage_comparison$gap_europe
      ),
      na.rm = TRUE
    ),
    max(
      abs(
        vintage_comparison$gap_japan
      ),
      na.rm = TRUE
    ),
    max(
      abs(
        vintage_comparison$gap_asia_pacific
      ),
      na.rm = TRUE
    ),
    max(
      abs(
        vintage_comparison$gap_total
      ),
      na.rm = TRUE
    )
  )
)

# ------------------------------------------------------------
# 15. Save outputs
# ------------------------------------------------------------

main_dataset_path <- file.path(
  processed_dir,
  "sia_regional_3mma_2015_2025.csv"
)

completeness_path <- file.path(
  output_dir,
  "sia_regional_completeness.csv"
)

internal_validation_path <- file.path(
  output_dir,
  "sia_internal_total_validation.csv"
)

vintage_comparison_path <- file.path(
  output_dir,
  "sia_vs_wsts_vintage_comparison.csv"
)

vintage_summary_path <- file.path(
  output_dir,
  "sia_vs_wsts_vintage_summary.csv"
)

write_csv(
  sia_regional,
  main_dataset_path
)

write_csv(
  completeness,
  completeness_path
)

write_csv(
  internal_validation,
  internal_validation_path
)

write_csv(
  vintage_comparison,
  vintage_comparison_path
)

write_csv(
  vintage_summary,
  vintage_summary_path
)

# ------------------------------------------------------------
# 16. Console report
# ------------------------------------------------------------

cat(
  "\n==============================\n"
)
cat(
  "SIA REGIONAL DATASET COMPLETED\n"
)
cat(
  "==============================\n\n"
)

cat(
  "Final observations:",
  nrow(sia_regional),
  "\n"
)

cat(
  "Unique dates:",
  n_distinct(
    sia_regional$date
  ),
  "\n"
)

cat(
  "Expected dates: 132\n"
)

cat(
  "Missing final months:",
  sum(
    !completeness$complete
  ),
  "\n\n"
)

cat(
  "Regional PDF parse status:\n"
)

print(
  regional_audit %>%
    count(
      parse_status
    ),
  n = Inf
)

cat(
  "\nInternal PDF consistency failures:\n"
)

print(
  regional_audit %>%
    summarise(
      america =
        sum(
          !america_consistent,
          na.rm = TRUE
        ),
      europe =
        sum(
          !europe_consistent,
          na.rm = TRUE
        ),
      japan =
        sum(
          !japan_consistent,
          na.rm = TRUE
        ),
      china =
        sum(
          !china_consistent,
          na.rm = TRUE
        ),
      asia_pacific_other =
        sum(
          !asia_pacific_other_consistent,
          na.rm = TRUE
        ),
      total =
        sum(
          !total_consistent,
          na.rm = TRUE
        )
    )
)

cat(
  "\nInternal regional-total validation:\n"
)

print(
  internal_validation %>%
    count(
      valid
    ),
  n = Inf
)

cat(
  "\nMaximum internal total discrepancy ",
  "(USD billions):\n",
  sep = ""
)

print(
  max(
    internal_validation$abs_difference_billions,
    na.rm = TRUE
  )
)

cat(
  "\nSIA vintage vs WSTS Historical Billings:\n"
)

print(
  vintage_summary,
  n = Inf
)

cat(
  "\nFiles created:\n"
)

cat(
  "- ",
  main_dataset_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  regional_audit_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  completeness_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  internal_validation_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  vintage_comparison_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  vintage_summary_path,
  "\n",
  sep = ""
)
