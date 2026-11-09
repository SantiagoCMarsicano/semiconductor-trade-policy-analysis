# Semiconductors II
# 01_data_audit.R
# Purpose: inspect the structure and contents of the WSTS Historical Billings workbook.
# No transformations are performed in this script.

required_packages <- c("readxl", "dplyr", "purrr", "tibble")

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(readxl)
library(dplyr)
library(purrr)
library(tibble)

wsts_path <- file.path(
  "data",
  "raw",
  "wsts_historical_billings.xlsx"
)

if (!file.exists(wsts_path)) {
  stop(
    paste0(
      "File not found: ", wsts_path,
      "\nCheck that the workbook is stored in data/raw/ ",
      "and named wsts_historical_billings.xlsx"
    )
  )
}

sheet_names <- excel_sheets(wsts_path)

cat("\n==============================\n")
cat("WSTS WORKBOOK AUDIT\n")
cat("==============================\n\n")

cat("Workbook:\n", wsts_path, "\n\n")

cat("Sheets found:\n")
print(sheet_names)

sheet_previews <- map(
  sheet_names,
  ~ read_excel(
    wsts_path,
    sheet = .x,
    n_max = 15,
    col_names = FALSE
  )
)

names(sheet_previews) <- sheet_names

walk2(
  sheet_previews,
  names(sheet_previews),
  ~ {
    cat("\n\n----------------------------------------\n")
    cat("SHEET:", .y, "\n")
    cat("----------------------------------------\n")
    print(.x, width = Inf)
  }
)

sheet_data <- map(
  sheet_names,
  ~ read_excel(
    wsts_path,
    sheet = .x
  )
)

names(sheet_data) <- sheet_names

sheet_summary <- imap_dfr(
  sheet_data,
  ~ tibble(
    sheet = .y,
    rows = nrow(.x),
    columns = ncol(.x),
    column_names = paste(names(.x), collapse = " | ")
  )
)

cat("\n\n==============================\n")
cat("SHEET SUMMARY\n")
cat("==============================\n\n")

print(sheet_summary, width = Inf)

regional_terms <- c(
  "Americas",  # exact WSTS source label; normalized to America downstream
  "Europe",
  "Japan",
  "Asia Pacific",
  "Asia-Pacific",
  "China",
  "Total",
  "World"
)

cat("\n\n==============================\n")
cat("REGIONAL LABEL SEARCH\n")
cat("==============================\n")

walk2(
  sheet_data,
  names(sheet_data),
  ~ {
    df <- .x
    sheet_name <- .y

    df_character <- df %>%
      mutate(across(everything(), as.character))

    matches <- map_dfr(
      regional_terms,
      function(term) {
        hit_matrix <- sapply(
          df_character,
          function(col) grepl(term, col, ignore.case = TRUE)
        )

        if (is.null(dim(hit_matrix))) {
          hit_matrix <- matrix(
            hit_matrix,
            ncol = ncol(df_character)
          )
        }

        hit_rows <- which(
          apply(hit_matrix, 1, any)
        )

        if (length(hit_rows) == 0) {
          return(NULL)
        }

        tibble(
          term = term,
          row = hit_rows
        )
      }
    )

    if (nrow(matches) > 0) {
      cat("\nSheet:", sheet_name, "\n")
      print(matches)
    }
  }
)

dir.create(
  file.path("output", "tables", "phase1"),
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  sheet_summary,
  file.path(
    "output",
    "tables",
    "phase1",
    "wsts_workbook_audit.csv"
  ),
  row.names = FALSE
)

cat("\n\nAudit completed successfully.\n")
cat(
  "Summary saved to:",
  file.path("output", "tables", "phase1", "wsts_workbook_audit.csv"),
  "\n"
)
