# Semiconductors II
# 08_america_country_source_audit.R
#
# Purpose:
#   - Prepare and audit the source layer for the America-by-country extension.
#   - Keep the validated monthly 2015-2025 pipeline untouched.
#   - Define the expected quarterly coverage from 2015 Q1 to 2025 Q4.
#   - Detect any official WSTS Americas-by-Country files placed locally.
#   - Generate a reproducible source registry and file manifest.
#
# Important:
#   - This script DOES NOT invent or estimate country-level values.
#   - WSTS Americas-by-Country reports are a separate quarterly product.
#   - The detailed historical files require authorized/subscriber access.
#   - Once legitimate source files are available, script 09 will parse them.

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "tibble",
  "digest"
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
library(tibble)
library(digest)

# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

raw_dir <- file.path(
  "data",
  "raw",
  "wsts_america_country"
)

registry_dir <- file.path(
  "data",
  "raw",
  "source_registry"
)

output_dir <- file.path(
  "output",
  "tables"
)

dir.create(
  raw_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  registry_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 3. Official source metadata
# ------------------------------------------------------------

official_sources <- tibble(
  source_name = c(
    "WSTS Americas by Country",
    "WSTS Market Statistics",
    "WSTS Publication Schedule",
    "ESIA WSTS Subscription Overview"
  ),

  source_type = c(
    "quarterly_report",
    "documentation",
    "documentation",
    "documentation"
  ),

  url = c(
    NA_character_,
    "https://www.wsts.org/61/MARKET-STATISTICS",
    "https://www.wsts.org/61/73/Publication-Schedule-for-WSTS-Reports",
    "https://www.eusemiconductors.eu/esia/industry-statistics/wsts"
  ),

  access_status = c(
    "authorized_access_required",
    "public",
    "public",
    "public"
  ),

  notes = c(
    paste(
      "Quarterly country-level semiconductor shipments for",
      "USA incl. Puerto Rico, Canada, Mexico, Brazil,",
      "and Rest of Latin America."
    ),
    paste(
      "Official WSTS page documenting the America-by-Country",
      "report and its quarterly frequency."
    ),
    paste(
      "Official publication schedule confirming quarterly",
      "By-Country report releases."
    ),
    paste(
      "ESIA documentation describing WSTS subscription products",
      "and the annual By-Country Summary from 2003 onward."
    )
  )
)

# ------------------------------------------------------------
# 4. Expected quarterly coverage
# ------------------------------------------------------------

years <- 2015:2025
quarters <- 1:4

expected_quarters <- expand.grid(
  year = years,
  quarter = quarters
) %>%
  as_tibble() %>%
  arrange(
    year,
    quarter
  ) %>%
  mutate(
    period = paste0(
      year,
      "-Q",
      quarter
    ),

    expected_filename = paste0(
      "wsts_america_country_",
      year,
      "_q",
      quarter,
      "."
    )
  )

# ------------------------------------------------------------
# 5. Scan local raw-source directory
# ------------------------------------------------------------

local_files <- list.files(
  raw_dir,
  full.names = TRUE,
  recursive = FALSE
)

supported_extensions <- c(
  "xlsx",
  "xls",
  "csv",
  "pdf"
)

if (length(local_files) == 0) {

  local_manifest <- tibble(
    filename = character(),
    filepath = character(),
    extension = character(),
    file_size_bytes = double(),
    sha256 = character()
  )

} else {

  local_manifest <- tibble(
    filepath = local_files,
    filename = basename(
      local_files
    )
  ) %>%
    mutate(
      extension = tolower(
        tools::file_ext(
          filename
        )
      )
    ) %>%
    filter(
      extension %in%
        supported_extensions
    ) %>%
    mutate(
      file_size_bytes = as.numeric(
        file.info(
          filepath
        )$size
      ),

      sha256 = vapply(
        filepath,
        function(path) {
          digest(
            file = path,
            algo = "sha256",
            serialize = FALSE
          )
        },
        character(1)
      )
    )
}

# ------------------------------------------------------------
# 6. Infer quarter from filenames
# ------------------------------------------------------------

if (nrow(local_manifest) > 0) {

  local_manifest <- local_manifest %>%
    mutate(
      filename_lower = tolower(
        filename
      ),

      detected_year = as.integer(
        str_extract(
          filename_lower,
          "20(1[5-9]|2[0-5])"
        )
      ),

      detected_quarter = case_when(
        str_detect(
          filename_lower,
          "q1|quarter[ _-]?1"
        ) ~ 1L,

        str_detect(
          filename_lower,
          "q2|quarter[ _-]?2"
        ) ~ 2L,

        str_detect(
          filename_lower,
          "q3|quarter[ _-]?3"
        ) ~ 3L,

        str_detect(
          filename_lower,
          "q4|quarter[ _-]?4"
        ) ~ 4L,

        TRUE ~ NA_integer_
      ),

      period = if_else(
        !is.na(
          detected_year
        ) &
          !is.na(
            detected_quarter
          ),
        paste0(
          detected_year,
          "-Q",
          detected_quarter
        ),
        NA_character_
      )
    )
}

# ------------------------------------------------------------
# 7. Build coverage audit
# ------------------------------------------------------------

if (nrow(local_manifest) == 0) {

  coverage_audit <- expected_quarters %>%
    mutate(
      file_found = FALSE,
      matched_filename = NA_character_,
      matched_sha256 = NA_character_
    )

} else {

  file_matches <- local_manifest %>%
    filter(
      !is.na(
        period
      )
    ) %>%
    group_by(
      period
    ) %>%
    summarise(
      matched_files = n(),
      matched_filename = paste(
        filename,
        collapse = " | "
      ),
      matched_sha256 = paste(
        sha256,
        collapse = " | "
      ),
      .groups = "drop"
    )

  coverage_audit <- expected_quarters %>%
    left_join(
      file_matches,
      by = "period"
    ) %>%
    mutate(
      file_found =
        !is.na(
          matched_filename
        ),

      matched_files =
        coalesce(
          matched_files,
          0L
        )
    )
}

# ------------------------------------------------------------
# 8. Save source registry and audit outputs
# ------------------------------------------------------------

official_sources_path <- file.path(
  registry_dir,
  "america_country_official_sources.csv"
)

manifest_path <- file.path(
  registry_dir,
  "america_country_file_manifest.csv"
)

coverage_path <- file.path(
  output_dir,
  "america_country_quarterly_coverage.csv"
)

write_csv(
  official_sources,
  official_sources_path
)

write_csv(
  local_manifest,
  manifest_path
)

write_csv(
  coverage_audit,
  coverage_path
)

# ------------------------------------------------------------
# 9. Console report
# ------------------------------------------------------------

cat(
  "\n==============================\n"
)
cat(
  "AMERICAS COUNTRY SOURCE AUDIT\n"
)
cat(
  "==============================\n\n"
)

cat(
  "Target period: 2015 Q1 to 2025 Q4\n"
)

cat(
  "Expected quarters:",
  nrow(
    expected_quarters
  ),
  "\n"
)

cat(
  "Supported local files found:",
  nrow(
    local_manifest
  ),
  "\n"
)

cat(
  "Quarterly periods matched:",
  sum(
    coverage_audit$file_found
  ),
  "\n"
)

cat(
  "Quarterly periods still missing:",
  sum(
    !coverage_audit$file_found
  ),
  "\n\n"
)

cat(
  "Raw-source directory:\n"
)

cat(
  "- ",
  raw_dir,
  "\n\n",
  sep = ""
)

cat(
  "Files created:\n"
)

cat(
  "- ",
  official_sources_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  manifest_path,
  "\n",
  sep = ""
)

cat(
  "- ",
  coverage_path,
  "\n",
  sep = ""
)

cat(
  "\nMethodological note:\n"
)

cat(
  paste(
    "No country-level values are estimated in this step.",
    "The extension will only be built from legitimate WSTS",
    "Americas-by-Country source files or another explicitly",
    "documented compatible source.\n"
  )
)
