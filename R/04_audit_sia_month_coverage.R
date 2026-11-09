# Semiconductors II
# 04_audit_sia_month_coverage.R
#
# Purpose:
#   - Read the SIA source registry created by script 03.
#   - Infer the actual reference month represented by each SIA release/chart.
#   - Audit monthly coverage for January 2014 through December 2025.
#   - Detect missing months, duplicate months, and unresolved releases.
#
# Important:
#   This script DOES NOT extract semiconductor sales values yet.
#   It only builds and validates the time index that will later be used
#   to download and parse the China / Asia Pacific-All Other 3MMA series.

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "lubridate",
  "tidyr",
  "tibble"
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
library(tidyr)
library(tibble)

# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

registry_path <- file.path(
  "data",
  "raw",
  "source_registry",
  "sia_market_release_registry.csv"
)

output_dir <- file.path(
  "output",
  "tables",
  "phase1"
)

registry_output_dir <- file.path(
  "data",
  "raw",
  "source_registry"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  registry_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(registry_path)) {
  stop(
    paste0(
      "Registry not found: ",
      registry_path,
      "\nRun R/03_discover_sia_market_sources.R first."
    )
  )
}

# ------------------------------------------------------------
# 3. Read and validate the source registry
# ------------------------------------------------------------

registry <- read_csv(
  registry_path,
  show_col_types = FALSE
)

required_columns <- c(
  "article_title",
  "publication_date",
  "article_url",
  "chart_url",
  "chart_link_text"
)

missing_columns <- setdiff(
  required_columns,
  names(registry)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "Missing required columns in registry: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

registry <- registry %>%
  mutate(
    publication_date = as.Date(
      publication_date
    )
  )

# ------------------------------------------------------------
# 4. Month dictionaries
# ------------------------------------------------------------

month_lookup <- c(
  january = 1L,
  jan = 1L,
  february = 2L,
  feb = 2L,
  march = 3L,
  mar = 3L,
  april = 4L,
  apr = 4L,
  may = 5L,
  june = 6L,
  jun = 6L,
  july = 7L,
  jul = 7L,
  august = 8L,
  aug = 8L,
  september = 9L,
  sept = 9L,
  sep = 9L,
  october = 10L,
  oct = 10L,
  november = 11L,
  nov = 11L,
  december = 12L,
  dec = 12L
)

month_pattern <- paste(
  names(month_lookup),
  collapse = "|"
)

# ------------------------------------------------------------
# 5. Helper functions
# ------------------------------------------------------------

normalize_source_text <- function(x) {

  x <- ifelse(
    is.na(x),
    "",
    x
  )

  x <- URLdecode(x)

  x %>%
    str_to_lower() %>%
    str_replace_all(
      "[_\\-]+",
      " "
    ) %>%
    str_replace_all(
      "\\s+",
      " "
    ) %>%
    str_trim()
}

extract_explicit_month_year <- function(x) {

  text <- normalize_source_text(x)

  pattern_month_year <- paste0(
    "\\b(",
    month_pattern,
    ")\\b\\s+(20\\d{2})\\b"
  )

  match_a <- str_match(
    text,
    regex(
      pattern_month_year,
      ignore_case = TRUE
    )
  )

  pattern_year_month <- paste0(
    "\\b(20\\d{2})\\b\\s+(",
    month_pattern,
    ")\\b"
  )

  match_b <- str_match(
    text,
    regex(
      pattern_year_month,
      ignore_case = TRUE
    )
  )

  result <- rep(
    as.Date(NA),
    length(text)
  )

  has_a <- !is.na(
    match_a[, 1]
  )

  if (any(has_a)) {

    month_number <- unname(
      month_lookup[
        match_a[has_a, 2]
      ]
    )

    year_number <- as.integer(
      match_a[has_a, 3]
    )

    result[has_a] <- make_date(
      year_number,
      month_number,
      1
    )
  }

  has_b <- is.na(result) &
    !is.na(
      match_b[, 1]
    )

  if (any(has_b)) {

    year_number <- as.integer(
      match_b[has_b, 2]
    )

    month_number <- unname(
      month_lookup[
        match_b[has_b, 3]
      ]
    )

    result[has_b] <- make_date(
      year_number,
      month_number,
      1
    )
  }

  result
}

extract_month_only <- function(x) {

  text <- normalize_source_text(x)

  pattern <- paste0(
    "\\b(",
    month_pattern,
    ")\\b"
  )

  matches <- str_match(
    text,
    regex(
      pattern,
      ignore_case = TRUE
    )
  )

  month_word <- matches[, 2]

  unname(
    month_lookup[
      month_word
    ]
  )
}

infer_reference_year <- function(
  publication_date,
  reference_month_number
) {

  publication_year <- year(
    publication_date
  )

  publication_month <- month(
    publication_date
  )

  inferred_year <- publication_year

  previous_year <- !is.na(
    reference_month_number
  ) &
    !is.na(
      publication_month
    ) &
    reference_month_number >
    publication_month

  inferred_year[previous_year] <-
    inferred_year[previous_year] - 1L

  inferred_year
}

# ------------------------------------------------------------
# 6. Extract the reference month using a priority hierarchy
# ------------------------------------------------------------

registry_enriched <- registry %>%
  mutate(

    ref_from_chart_text =
      extract_explicit_month_year(
        chart_link_text
      ),

    ref_from_chart_url =
      extract_explicit_month_year(
        chart_url
      ),

    ref_from_article_title =
      extract_explicit_month_year(
        article_title
      ),

    month_from_chart_text =
      extract_month_only(
        chart_link_text
      ),

    month_from_article_title =
      extract_month_only(
        article_title
      ),

    fallback_month_number =
      coalesce(
        month_from_chart_text,
        month_from_article_title
      ),

    fallback_year =
      infer_reference_year(
        publication_date,
        fallback_month_number
      ),

    ref_from_fallback =
      if_else(
        !is.na(
          fallback_month_number
        ) &
          !is.na(
            fallback_year
          ),
        make_date(
          fallback_year,
          fallback_month_number,
          1
        ),
        as.Date(NA)
      ),

    reference_month =
      coalesce(
        ref_from_chart_text,
        ref_from_chart_url,
        ref_from_article_title,
        ref_from_fallback
      ),

    reference_source =
      case_when(
        !is.na(ref_from_chart_text) ~
          "chart_link_text_explicit",

        is.na(ref_from_chart_text) &
          !is.na(ref_from_chart_url) ~
          "chart_url_explicit",

        is.na(ref_from_chart_text) &
          is.na(ref_from_chart_url) &
          !is.na(ref_from_article_title) ~
          "article_title_explicit",

        is.na(ref_from_chart_text) &
          is.na(ref_from_chart_url) &
          is.na(ref_from_article_title) &
          !is.na(ref_from_fallback) ~
          "publication_date_inference",

        TRUE ~
          "unresolved"
      ),

    reference_confidence =
      case_when(
        reference_source %in% c(
          "chart_link_text_explicit",
          "chart_url_explicit"
        ) ~ "high",

        reference_source ==
          "article_title_explicit" ~
          "medium_high",

        reference_source ==
          "publication_date_inference" ~
          "medium",

        TRUE ~
          "unresolved"
      )
  )

# ------------------------------------------------------------
# 7. Build expected monthly calendar: 2014-2025
# ------------------------------------------------------------

target_start <- as.Date(
  "2014-01-01"
)

target_end <- as.Date(
  "2025-12-01"
)

expected_calendar <- tibble(
  reference_month = seq(
    from = target_start,
    to = target_end,
    by = "month"
  )
)

# ------------------------------------------------------------
# 8. Summarize actual coverage
# ------------------------------------------------------------

coverage_actual <- registry_enriched %>%
  filter(
    !is.na(
      reference_month
    ),
    reference_month >=
      target_start,
    reference_month <=
      target_end
  ) %>%
  group_by(
    reference_month
  ) %>%
  summarise(
    source_count = n(),
    release_dates = paste(
      sort(
        unique(
          publication_date
        )
      ),
      collapse = " | "
    ),
    article_titles = paste(
      unique(
        article_title
      ),
      collapse = " || "
    ),
    article_urls = paste(
      unique(
        article_url
      ),
      collapse = " || "
    ),
    chart_urls = paste(
      unique(
        chart_url
      ),
      collapse = " || "
    ),
    extraction_sources = paste(
      unique(
        reference_source
      ),
      collapse = " | "
    ),
    .groups = "drop"
  )

coverage_audit <- expected_calendar %>%
  left_join(
    coverage_actual,
    by = "reference_month"
  ) %>%
  mutate(
    source_count = replace_na(
      source_count,
      0L
    ),
    coverage_status =
      case_when(
        source_count == 0L ~
          "missing",

        source_count == 1L ~
          "unique",

        source_count > 1L ~
          "duplicate"
      )
  )

# ------------------------------------------------------------
# 9. Duplicate and unresolved audits
# ------------------------------------------------------------

duplicate_months <- registry_enriched %>%
  filter(
    !is.na(
      reference_month
    ),
    reference_month >=
      target_start,
    reference_month <=
      target_end
  ) %>%
  group_by(
    reference_month
  ) %>%
  filter(
    n() > 1
  ) %>%
  ungroup() %>%
  arrange(
    reference_month,
    publication_date
  ) %>%
  select(
    reference_month,
    publication_date,
    reference_source,
    reference_confidence,
    article_title,
    article_url,
    chart_link_text,
    chart_url
  )

unresolved_releases <- registry_enriched %>%
  filter(
    is.na(
      reference_month
    )
  ) %>%
  arrange(
    publication_date
  ) %>%
  select(
    publication_date,
    article_title,
    article_url,
    chart_link_text,
    chart_url,
    reference_source,
    reference_confidence
  )

# ------------------------------------------------------------
# 10. Save outputs
# ------------------------------------------------------------

enriched_registry_path <- file.path(
  registry_output_dir,
  "sia_market_release_registry_with_reference_month.csv"
)

coverage_audit_path <- file.path(
  output_dir,
  "sia_reference_month_coverage_2014_2025.csv"
)

duplicates_path <- file.path(
  output_dir,
  "sia_reference_month_duplicates.csv"
)

unresolved_path <- file.path(
  output_dir,
  "sia_unresolved_releases.csv"
)

write_csv(
  registry_enriched,
  enriched_registry_path
)

write_csv(
  coverage_audit,
  coverage_audit_path
)

write_csv(
  duplicate_months,
  duplicates_path
)

write_csv(
  unresolved_releases,
  unresolved_path
)

# ------------------------------------------------------------
# 11. Console report
# ------------------------------------------------------------

coverage_summary <- coverage_audit %>%
  count(
    coverage_status,
    name = "months"
  )

extraction_summary <- registry_enriched %>%
  count(
    reference_source,
    reference_confidence,
    name = "releases"
  ) %>%
  arrange(
    reference_confidence,
    reference_source
  )

cat(
  "\n==============================\n"
)
cat(
  "SIA MONTH COVERAGE AUDIT\n"
)
cat(
  "==============================\n\n"
)

cat(
  "Target period: January 2014 - December 2025\n"
)

cat(
  "Expected months:",
  nrow(expected_calendar),
  "\n\n"
)

cat(
  "Coverage summary:\n"
)

print(
  coverage_summary,
  n = Inf
)

cat(
  "\nReference-month extraction methods:\n"
)

print(
  extraction_summary,
  n = Inf
)

cat(
  "\nMissing reference months:\n"
)

missing_months <- coverage_audit %>%
  filter(
    coverage_status == "missing"
  ) %>%
  select(
    reference_month
  )

if (nrow(missing_months) == 0) {

  cat(
    "None.\n"
  )

} else {

  print(
    missing_months,
    n = Inf
  )
}

cat(
  "\nDuplicate reference months:\n"
)

duplicate_summary <- coverage_audit %>%
  filter(
    coverage_status == "duplicate"
  ) %>%
  select(
    reference_month,
    source_count
  )

if (nrow(duplicate_summary) == 0) {

  cat(
    "None.\n"
  )

} else {

  print(
    duplicate_summary,
    n = Inf
  )
}

cat(
  "\nUnresolved releases:",
  nrow(unresolved_releases),
  "\n"
)

cat(
  "\nFiles created:\n"
)

cat(
  "- ",
  enriched_registry_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  coverage_audit_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  duplicates_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  unresolved_path,
  "\n",
  sep = ""
)

cat(
  "\nImportant: no sales values were extracted in this step.\n"
)
