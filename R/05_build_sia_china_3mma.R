# Semiconductors II
# 05_build_sia_china_3mma.R
#
# Purpose:
#   - Normalize legacy SIA chart URLs.
#   - Download and cache official SIA monthly chart PDFs.
#   - Extract China and Asia Pacific/All Other 3MMA values.
#   - Build the 2016-2025 current-month series directly from each PDF.
#   - Reconstruct 2015 from the "Last Year" column in the 2016 PDFs.
#   - Repair the missing March 2020 source with its official SIA PDF URL.
#   - Validate China + Asia Pacific/All Other against the WSTS Asia Pacific series.
#
# Final target:
#   132 monthly observations from January 2015 through December 2025.
#
# Important:
#   SIA monthly sales figures are WSTS three-month moving averages (3MMA).
#   The public PDFs report values in billions of US dollars.

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "lubridate",
  "tibble",
  "purrr",
  "tidyr",
  "httr2",
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
library(purrr)
library(tidyr)
library(httr2)
library(pdftools)

# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

registry_path <- file.path(
  "data",
  "raw",
  "source_registry",
  "sia_market_release_registry_with_reference_month.csv"
)

wsts_path <- file.path(
  "data",
  "processed",
  "wsts_3mma_2010_2025.csv"
)

pdf_cache_dir <- file.path(
  "data",
  "raw",
  "sia_charts"
)

processed_dir <- file.path(
  "data",
  "processed"
)

output_dir <- file.path(
  "output",
  "tables"
)

source_registry_dir <- file.path(
  "data",
  "raw",
  "source_registry"
)

dir.create(
  pdf_cache_dir,
  recursive = TRUE,
  showWarnings = FALSE
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

if (!file.exists(registry_path)) {
  stop(
    paste0(
      "Source registry not found: ",
      registry_path,
      "\nRun R/04_audit_sia_month_coverage.R first."
    )
  )
}

if (!file.exists(wsts_path)) {
  stop(
    paste0(
      "Processed WSTS 3MMA dataset not found: ",
      wsts_path,
      "\nRun R/02_build_wsts_base.R first."
    )
  )
}

# ------------------------------------------------------------
# 3. URL normalizer
# ------------------------------------------------------------

normalize_chart_url <- function(x) {

  x <- str_trim(x)

  # Decode HTML entities that can remain inside href values.
  x <- str_replace_all(
    x,
    "&amp;",
    "&"
  )

  case_when(

    # Already complete.
    str_detect(
      x,
      regex(
        "^https?://",
        ignore_case = TRUE
      )
    ) ~ x,

    # Protocol-relative URL:
    # //www.semiconductors.org/file.pdf
    str_detect(
      x,
      "^//"
    ) ~ paste0(
      "https:",
      x
    ),

    # Root-relative URL:
    # /wp-content/uploads/file.pdf
    str_detect(
      x,
      "^/"
    ) ~ paste0(
      "https://www.semiconductors.org",
      x
    ),

    # Domain present but scheme missing:
    # www.semiconductors.org/file.pdf
    str_detect(
      x,
      regex(
        "^(www\\.)?semiconductors\\.org/",
        ignore_case = TRUE
      )
    ) ~ paste0(
      "https://",
      x
    ),

    TRUE ~ x
  )
}

# ------------------------------------------------------------
# 4. Read source registry
# ------------------------------------------------------------

registry <- read_csv(
  registry_path,
  show_col_types = FALSE
) %>%
  mutate(
    publication_date = as.Date(
      publication_date
    ),
    reference_month = as.Date(
      reference_month
    )
  )

target_sources <- registry %>%
  filter(
    reference_month >= as.Date(
      "2016-01-01"
    ),
    reference_month <= as.Date(
      "2025-12-01"
    )
  ) %>%
  select(
    reference_month,
    publication_date,
    article_title,
    article_url,
    chart_link_text,
    chart_url
  ) %>%
  distinct(
    reference_month,
    .keep_all = TRUE
  ) %>%
  mutate(
    source_origin = "registry",
    chart_url_original = chart_url,
    chart_url = normalize_chart_url(
      chart_url
    )
  )

# ------------------------------------------------------------
# 5. Repair the March 2020 source gap
# ------------------------------------------------------------

march_2020_date <- as.Date(
  "2020-03-01"
)

march_2020_pdf <- paste0(
  "https://www.semiconductors.org/",
  "wp-content/uploads/2020/05/",
  "March-2020-GSR-table-and-graph-for-press-release.pdf"
)

if (!march_2020_date %in%
    target_sources$reference_month) {

  march_2020_source <- tibble(
    reference_month = march_2020_date,
    publication_date = as.Date(
      "2020-05-01"
    ),
    article_title =
      "March 2020 official SIA chart",
    article_url =
      "https://www.semiconductors.org/global-semiconductor-sales-decrease-3-6-percent-in-first-quarter-of-2020/",
    chart_link_text =
      "March 2020 chart and graph",
    chart_url =
      march_2020_pdf,
    source_origin =
      "official_source_repair",
    chart_url_original =
      march_2020_pdf
  )

  target_sources <- bind_rows(
    target_sources,
    march_2020_source
  )
}

target_sources <- target_sources %>%
  mutate(
    chart_url = normalize_chart_url(
      chart_url
    )
  ) %>%
  arrange(
    reference_month
  )

# ------------------------------------------------------------
# 6. URL audit
# ------------------------------------------------------------

target_sources <- target_sources %>%
  mutate(
    valid_http_scheme = str_detect(
      chart_url,
      regex(
        "^https?://",
        ignore_case = TRUE
      )
    ),
    url_was_normalized =
      chart_url != chart_url_original
  )

invalid_urls <- target_sources %>%
  filter(
    !valid_http_scheme
  )

if (nrow(invalid_urls) > 0) {

  write_csv(
    invalid_urls,
    file.path(
      output_dir,
      "sia_invalid_chart_urls.csv"
    )
  )

  stop(
    paste0(
      "Some chart URLs still do not have a valid HTTP/HTTPS scheme. ",
      "Inspect output/tables/sia_invalid_chart_urls.csv"
    )
  )
}

# ------------------------------------------------------------
# 7. Audit source coverage before downloading
# ------------------------------------------------------------

expected_2016_2025 <- tibble(
  reference_month = seq(
    as.Date(
      "2016-01-01"
    ),
    as.Date(
      "2025-12-01"
    ),
    by = "month"
  )
)

source_coverage <- expected_2016_2025 %>%
  left_join(
    target_sources %>%
      count(
        reference_month,
        name = "source_count"
      ),
    by = "reference_month"
  ) %>%
  mutate(
    source_count = replace_na(
      source_count,
      0L
    )
  )

missing_sources <- source_coverage %>%
  filter(
    source_count == 0L
  )

duplicate_sources <- source_coverage %>%
  filter(
    source_count > 1L
  )

if (nrow(missing_sources) > 0) {
  stop(
    paste0(
      "Missing source PDFs remain for: ",
      paste(
        missing_sources$reference_month,
        collapse = ", "
      )
    )
  )
}

if (nrow(duplicate_sources) > 0) {
  stop(
    paste0(
      "Duplicate source months detected: ",
      paste(
        duplicate_sources$reference_month,
        collapse = ", "
      )
    )
  )
}

# ------------------------------------------------------------
# 8. Download helper
# ------------------------------------------------------------

download_sia_pdf <- function(
  reference_month,
  chart_url
) {

  file_name <- paste0(
    "sia_",
    format(
      reference_month,
      "%Y-%m"
    ),
    ".pdf"
  )

  local_path <- file.path(
    pdf_cache_dir,
    file_name
  )

  # Reuse PDFs downloaded successfully in previous runs.
  if (file.exists(local_path)) {
    return(local_path)
  }

  response <- request(
    chart_url
  ) %>%
    req_user_agent(
      "Semiconductors-II academic research project"
    ) %>%
    req_timeout(60) %>%
    req_retry(
      max_tries = 3
    ) %>%
    req_perform(
      path = local_path
    )

  status <- resp_status(
    response
  )

  if (
    status >= 400 ||
    !file.exists(local_path)
  ) {
    stop(
      paste0(
        "PDF download failed with HTTP status ",
        status,
        " for ",
        reference_month,
        ": ",
        chart_url
      )
    )
  }

  Sys.sleep(0.15)

  local_path
}

# ------------------------------------------------------------
# 9. PDF row parser
# ------------------------------------------------------------

extract_region_rows <- function(
  pdf_text_full,
  region_pattern
) {

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
# 10. Parse one PDF
# ------------------------------------------------------------

parse_sia_pdf <- function(
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

  china_rows <- extract_region_rows(
    pdf_text_full,
    "China"
  )

  ap_other_rows <- extract_region_rows(
    pdf_text_full,
    "Asia\\s+Pacific/All\\s+Other"
  )

  if (
    nrow(china_rows) < 3 ||
    nrow(ap_other_rows) < 3
  ) {

    return(
      tibble(
        reference_month = reference_month,
        local_path = local_path,
        parse_status = "failed_region_rows",
        china_row_count = nrow(
          china_rows
        ),
        ap_other_row_count = nrow(
          ap_other_rows
        ),
        china_current_3mma = NA_real_,
        ap_other_current_3mma = NA_real_,
        china_last_year = NA_real_,
        ap_other_last_year = NA_real_,
        china_current_consistent = NA,
        ap_other_current_consistent = NA
      )
    )
  }

  china_last_year <-
    china_rows$comparison_value[2]

  ap_other_last_year <-
    ap_other_rows$comparison_value[2]

  china_current_3mma <-
    china_rows$current_value[3]

  ap_other_current_3mma <-
    ap_other_rows$current_value[3]

  china_current_consistent <-
    max(
      china_rows$current_value[1:3]
    ) -
      min(
        china_rows$current_value[1:3]
      ) <
      0.001

  ap_other_current_consistent <-
    max(
      ap_other_rows$current_value[1:3]
    ) -
      min(
        ap_other_rows$current_value[1:3]
      ) <
      0.001

  tibble(
    reference_month = reference_month,
    local_path = local_path,
    parse_status = "ok",
    china_row_count = nrow(
      china_rows
    ),
    ap_other_row_count = nrow(
      ap_other_rows
    ),
    china_current_3mma =
      china_current_3mma,
    ap_other_current_3mma =
      ap_other_current_3mma,
    china_last_year =
      china_last_year,
    ap_other_last_year =
      ap_other_last_year,
    china_current_consistent =
      china_current_consistent,
    ap_other_current_consistent =
      ap_other_current_consistent
  )
}

# ------------------------------------------------------------
# 11. Download and parse all 2016-2025 PDFs
# ------------------------------------------------------------

cat(
  "\n==============================\n"
)
cat(
  "SIA PDF EXTRACTION\n"
)
cat(
  "==============================\n\n"
)

cat(
  "Source PDFs to process:",
  nrow(target_sources),
  "\n"
)

cat(
  "Legacy URLs normalized:",
  sum(
    target_sources$url_was_normalized
  ),
  "\n\n"
)

parse_results <- vector(
  mode = "list",
  length = nrow(
    target_sources
  )
)

for (
  i in seq_len(
    nrow(target_sources)
  )
) {

  source_row <- target_sources[
    i,
  ]

  cat(
    "Processing ",
    format(
      source_row$reference_month,
      "%Y-%m"
    ),
    " ... ",
    sep = ""
  )

  local_path <- tryCatch(
    download_sia_pdf(
      source_row$reference_month,
      source_row$chart_url
    ),
    error = function(e) {
      message(
        "\nDownload error: ",
        conditionMessage(e)
      )
      NA_character_
    }
  )

  if (is.na(local_path)) {

    parse_results[[i]] <- tibble(
      reference_month =
        source_row$reference_month,
      local_path =
        NA_character_,
      parse_status =
        "download_failed",
      china_row_count =
        NA_integer_,
      ap_other_row_count =
        NA_integer_,
      china_current_3mma =
        NA_real_,
      ap_other_current_3mma =
        NA_real_,
      china_last_year =
        NA_real_,
      ap_other_last_year =
        NA_real_,
      china_current_consistent =
        NA,
      ap_other_current_consistent =
        NA
    )

    cat(
      "DOWNLOAD FAILED\n"
    )

    next
  }

  parsed <- tryCatch(
    parse_sia_pdf(
      source_row$reference_month,
      local_path
    ),
    error = function(e) {

      message(
        "\nParse error: ",
        conditionMessage(e)
      )

      tibble(
        reference_month =
          source_row$reference_month,
        local_path =
          local_path,
        parse_status =
          "parse_error",
        china_row_count =
          NA_integer_,
        ap_other_row_count =
          NA_integer_,
        china_current_3mma =
          NA_real_,
        ap_other_current_3mma =
          NA_real_,
        china_last_year =
          NA_real_,
        ap_other_last_year =
          NA_real_,
        china_current_consistent =
          NA,
        ap_other_current_consistent =
          NA
      )
    }
  )

  parse_results[[i]] <- parsed

  cat(
    parsed$parse_status,
    "\n"
  )
}

parse_audit <- bind_rows(
  parse_results
) %>%
  left_join(
    target_sources,
    by = "reference_month"
  ) %>%
  arrange(
    reference_month
  )

parse_audit_path <- file.path(
  output_dir,
  "sia_pdf_parse_audit.csv"
)

write_csv(
  parse_audit,
  parse_audit_path
)

# ------------------------------------------------------------
# 12. Diagnose failures separately
# ------------------------------------------------------------

download_failures <- parse_audit %>%
  filter(
    parse_status ==
      "download_failed"
  )

parser_failures <- parse_audit %>%
  filter(
    parse_status %in% c(
      "failed_region_rows",
      "parse_error"
    )
  )

if (nrow(download_failures) > 0) {
  stop(
    paste0(
      nrow(download_failures),
      " PDF downloads failed. ",
      "Inspect output/tables/sia_pdf_parse_audit.csv."
    )
  )
}

if (nrow(parser_failures) > 0) {
  stop(
    paste0(
      nrow(parser_failures),
      " PDFs downloaded but could not be parsed. ",
      "Inspect output/tables/sia_pdf_parse_audit.csv."
    )
  )
}

# ------------------------------------------------------------
# 13. Build direct 2016-2025 series
# ------------------------------------------------------------

series_2016_2025 <- parse_audit %>%
  transmute(
    date =
      reference_month,
    china_3mma_usd_billions =
      china_current_3mma,
    asia_pacific_other_3mma_usd_billions =
      ap_other_current_3mma,
    source_method =
      "sia_pdf_current_3mma",
    source_reference_month =
      reference_month,
    publication_date =
      publication_date,
    chart_url =
      chart_url,
    source_file =
      local_path
  )

# ------------------------------------------------------------
# 14. Reconstruct 2015 from 2016 Year-to-Year tables
# ------------------------------------------------------------

series_2015 <- parse_audit %>%
  filter(
    year(
      reference_month
    ) == 2016
  ) %>%
  transmute(
    date =
      reference_month %m-%
      years(1),
    china_3mma_usd_billions =
      china_last_year,
    asia_pacific_other_3mma_usd_billions =
      ap_other_last_year,
    source_method =
      "reconstructed_from_2016_last_year",
    source_reference_month =
      reference_month,
    publication_date =
      publication_date,
    chart_url =
      chart_url,
    source_file =
      local_path
  )

# ------------------------------------------------------------
# 15. Combine 2015-2025
# ------------------------------------------------------------

sia_series <- bind_rows(
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

    china_share_asia_pacific =
      china_3mma_usd_billions /
      asia_pacific_total_3mma_usd_billions
  )

# ------------------------------------------------------------
# 16. Completeness audit
# ------------------------------------------------------------

expected_2015_2025 <- tibble(
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

completeness <- expected_2015_2025 %>%
  left_join(
    sia_series %>%
      select(
        date,
        china_3mma_usd_billions,
        asia_pacific_other_3mma_usd_billions,
        source_method
      ),
    by = "date"
  ) %>%
  mutate(
    complete =
      !is.na(
        china_3mma_usd_billions
      ) &
      !is.na(
        asia_pacific_other_3mma_usd_billions
      )
  )

# ------------------------------------------------------------
# 17. Validate against WSTS Asia Pacific
# ------------------------------------------------------------

wsts_ap <- read_csv(
  wsts_path,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(
      date
    )
  ) %>%
  filter(
    region == "Asia Pacific",
    date >= as.Date(
      "2015-01-01"
    ),
    date <= as.Date(
      "2025-12-01"
    )
  ) %>%
  select(
    date,
    wsts_asia_pacific_3mma_usd_billions =
      sales_usd_billions
  )

validation_tolerance_billions <- 0.02

wsts_validation <- sia_series %>%
  select(
    date,
    china_3mma_usd_billions,
    asia_pacific_other_3mma_usd_billions,
    asia_pacific_total_3mma_usd_billions
  ) %>%
  left_join(
    wsts_ap,
    by = "date"
  ) %>%
  mutate(
    difference_billions =
      asia_pacific_total_3mma_usd_billions -
      wsts_asia_pacific_3mma_usd_billions,

    abs_difference_billions =
      abs(
        difference_billions
      ),

    valid =
      abs_difference_billions <=
      validation_tolerance_billions
  )

# ------------------------------------------------------------
# 18. Save outputs
# ------------------------------------------------------------

series_path <- file.path(
  processed_dir,
  "sia_china_ap_other_3mma_2015_2025.csv"
)

completeness_path <- file.path(
  output_dir,
  "sia_china_series_completeness.csv"
)

validation_path <- file.path(
  output_dir,
  "sia_wsts_asia_pacific_validation.csv"
)

source_extract_path <- file.path(
  source_registry_dir,
  "sia_2016_2025_extraction_sources.csv"
)

write_csv(
  sia_series,
  series_path
)

write_csv(
  completeness,
  completeness_path
)

write_csv(
  wsts_validation,
  validation_path
)

write_csv(
  target_sources,
  source_extract_path
)

# ------------------------------------------------------------
# 19. Console report
# ------------------------------------------------------------

cat(
  "\n==============================\n"
)
cat(
  "SIA CHINA 3MMA BUILD COMPLETED\n"
)
cat(
  "==============================\n\n"
)

cat(
  "Final observations:",
  nrow(sia_series),
  "\n"
)

cat(
  "Unique dates:",
  n_distinct(
    sia_series$date
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
  "PDF parse status:\n"
)

print(
  parse_audit %>%
    count(
      parse_status
    ),
  n = Inf
)

cat(
  "\nInternal PDF consistency:\n"
)

print(
  parse_audit %>%
    summarise(
      china_inconsistent =
        sum(
          !china_current_consistent,
          na.rm = TRUE
        ),
      ap_other_inconsistent =
        sum(
          !ap_other_current_consistent,
          na.rm = TRUE
        )
    )
)

cat(
  "\nWSTS Asia Pacific validation:\n"
)

print(
  wsts_validation %>%
    count(
      valid
    ),
  n = Inf
)

cat(
  "\nMaximum absolute discrepancy vs WSTS ",
  "(USD billions):\n",
  sep = ""
)

print(
  max(
    wsts_validation$abs_difference_billions,
    na.rm = TRUE
  )
)

cat(
  "\nFiles created:\n"
)

cat(
  "- ",
  series_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  parse_audit_path,
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
  validation_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  source_extract_path,
  "\n",
  sep = ""
)

cat(
  "\nRaw PDFs cached in:\n"
)

cat(
  "- ",
  pdf_cache_dir,
  "\n",
  sep = ""
)
