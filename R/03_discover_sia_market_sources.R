# Semiconductors II
# 03_discover_sia_market_sources.R
#
# Purpose:
#   Discover public SIA press releases related to monthly semiconductor sales,
#   extract their publication dates and "chart and graph" links, and build a
#   source registry for the China / Asia Pacific-All Other 3MMA reconstruction.
#
# This script DOES NOT extract sales values yet.
# It only builds and audits the list of official source pages.

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

required_packages <- c(
  "rvest",
  "httr2",
  "dplyr",
  "purrr",
  "stringr",
  "readr",
  "tibble",
  "lubridate"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(rvest)
library(httr2)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tibble)
library(lubridate)

# ------------------------------------------------------------
# 2. Settings
# ------------------------------------------------------------

archive_root <- "https://www.semiconductors.org/category/press-releases/"

# We search backwards through the public press-release archive.
# Increase this number later if the 2014-2015 period is not reached.
max_pages <- 100L

output_dir <- file.path(
  "data",
  "raw",
  "source_registry"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 3. Helper: safely download HTML
# ------------------------------------------------------------

safe_read_html <- function(url) {

  response <- tryCatch(
    request(url) %>%
      req_user_agent(
        "Semiconductors-II academic research project"
      ) %>%
      req_timeout(30) %>%
      req_perform(),
    error = function(e) NULL
  )

  if (is.null(response)) {
    return(NULL)
  }

  status <- resp_status(response)

  if (status >= 400) {
    return(NULL)
  }

  read_html(
    resp_body_string(response)
  )
}

# ------------------------------------------------------------
# 4. Discover candidate press-release URLs
# ------------------------------------------------------------

candidate_links <- list()

cat("\n==============================\n")
cat("SIA SOURCE DISCOVERY\n")
cat("==============================\n\n")

for (page_number in seq_len(max_pages)) {

  page_url <- if (page_number == 1L) {
    archive_root
  } else {
    paste0(
      archive_root,
      "page/",
      page_number,
      "/"
    )
  }

  cat(
    "Scanning archive page:",
    page_number,
    "\n"
  )

  html <- safe_read_html(page_url)

  # Stop once pagination no longer exists.
  if (is.null(html)) {
    cat(
      "Archive ended or page unavailable at page ",
      page_number,
      ".\n",
      sep = ""
    )
    break
  }

  anchors <- html %>%
    html_elements("a")

  links <- tibble(
    link_text = anchors %>%
      html_text2() %>%
      str_squish(),
    article_url = anchors %>%
      html_attr("href")
  ) %>%
    filter(
      !is.na(article_url),
      str_detect(
        article_url,
        "^https://www\\.semiconductors\\.org/"
      )
    ) %>%
    filter(
      str_detect(
        str_to_lower(link_text),
        "semiconductor"
      ),
      str_detect(
        str_to_lower(link_text),
        "sales"
      )
    ) %>%
    filter(
      !str_detect(
        article_url,
        "/category/|/tag/|/policies/"
      )
    ) %>%
    distinct(
      article_url,
      .keep_all = TRUE
    )

  if (nrow(links) > 0) {
    candidate_links[[length(candidate_links) + 1L]] <-
      links
  }

  # Be polite to the public website.
  Sys.sleep(0.15)
}

candidates <- bind_rows(
  candidate_links
) %>%
  distinct(
    article_url,
    .keep_all = TRUE
  )

if (nrow(candidates) == 0) {
  stop(
    paste0(
      "No candidate SIA semiconductor-sales releases were found. ",
      "The archive structure may have changed."
    )
  )
}

cat(
  "\nCandidate releases found:",
  nrow(candidates),
  "\n\n"
)

# ------------------------------------------------------------
# 5. Inspect each candidate release
# ------------------------------------------------------------

extract_release_metadata <- function(
  article_url,
  fallback_title
) {

  html <- safe_read_html(
    article_url
  )

  if (is.null(html)) {
    return(
      tibble(
        article_title = fallback_title,
        publication_date = as.Date(NA),
        article_url = article_url,
        chart_url = NA_character_,
        chart_link_text = NA_character_
      )
    )
  }

  # Prefer the page H1 as canonical title.
  h1 <- html %>%
    html_elements("h1") %>%
    html_text2() %>%
    str_squish()

  article_title <- if (
    length(h1) > 0 &&
    nzchar(h1[1])
  ) {
    h1[1]
  } else {
    fallback_title
  }

  # WordPress commonly exposes the publication timestamp here.
  published_meta <- html %>%
    html_elements(
      "meta[property='article:published_time']"
    ) %>%
    html_attr("content")

  publication_date <- as.Date(NA)

  if (
    length(published_meta) > 0 &&
    !is.na(published_meta[1])
  ) {
    publication_date <- suppressWarnings(
      as.Date(
        substr(
          published_meta[1],
          1,
          10
        )
      )
    )
  }

  # Fallback to a visible <time> element.
  if (is.na(publication_date)) {

    time_value <- html %>%
      html_elements("time") %>%
      html_attr("datetime")

    if (
      length(time_value) > 0 &&
      !is.na(time_value[1])
    ) {
      publication_date <- suppressWarnings(
        as.Date(
          substr(
            time_value[1],
            1,
            10
          )
        )
      )
    }
  }

  article_anchors <- html %>%
    html_elements("a")

  article_links <- tibble(
    text = article_anchors %>%
      html_text2() %>%
      str_squish(),
    href = article_anchors %>%
      html_attr("href")
  ) %>%
    filter(
      !is.na(href)
    )

  # SIA monthly releases usually contain a link such as:
  # "June 2020 chart and graph"
  chart_candidates <- article_links %>%
    filter(
      str_detect(
        str_to_lower(text),
        "chart.*graph|graph.*chart"
      ) |
        str_detect(
          str_to_lower(href),
          "gsr.*\\.pdf$|table.*graph.*\\.pdf$"
        )
    ) %>%
    distinct(
      href,
      .keep_all = TRUE
    )

  if (nrow(chart_candidates) > 0) {

    chart_url <- chart_candidates$href[1]
    chart_link_text <- chart_candidates$text[1]

  } else {

    chart_url <- NA_character_
    chart_link_text <- NA_character_
  }

  tibble(
    article_title = article_title,
    publication_date = publication_date,
    article_url = article_url,
    chart_url = chart_url,
    chart_link_text = chart_link_text
  )
}

cat(
  "Inspecting candidate release pages...\n"
)

registry <- map2_dfr(
  candidates$article_url,
  candidates$link_text,
  ~ {
    result <- extract_release_metadata(
      .x,
      .y
    )

    Sys.sleep(0.15)

    result
  }
) %>%
  mutate(
    publication_year = year(
      publication_date
    ),
    has_chart_link =
      !is.na(chart_url),
    relevant_period =
      publication_year >= 2015 &
      publication_year <= 2026
  ) %>%
  arrange(
    publication_date
  )

# ------------------------------------------------------------
# 6. Keep the working period
# ------------------------------------------------------------

registry_target <- registry %>%
  filter(
    relevant_period
  )

# ------------------------------------------------------------
# 7. Save source registry
# ------------------------------------------------------------

registry_path <- file.path(
  output_dir,
  "sia_market_release_registry.csv"
)

write_csv(
  registry_target,
  registry_path
)

# ------------------------------------------------------------
# 8. Audit coverage
# ------------------------------------------------------------

coverage_by_year <- registry_target %>%
  count(
    publication_year,
    name = "release_count"
  ) %>%
  arrange(
    publication_year
  )

chart_coverage <- registry_target %>%
  count(
    publication_year,
    has_chart_link
  ) %>%
  arrange(
    publication_year,
    desc(has_chart_link)
  )

cat("\n==============================\n")
cat("SOURCE REGISTRY COMPLETED\n")
cat("==============================\n\n")

cat(
  "Target-period releases:",
  nrow(registry_target),
  "\n\n"
)

cat(
  "Release count by publication year:\n"
)

print(
  coverage_by_year,
  n = Inf
)

cat(
  "\nChart-link coverage:\n"
)

print(
  chart_coverage,
  n = Inf
)

cat(
  "\nRegistry saved to:\n",
  registry_path,
  "\n",
  sep = ""
)

cat(
  "\nImportant: this script only discovers sources. ",
  "It does not yet extract China sales values.\n",
  sep = ""
)
